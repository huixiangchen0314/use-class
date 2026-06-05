(ns top.kzre.use-class.corex
  "数据驱动的 Java 实现复用引擎，v2.9"
  (:require [clojure.string :as str])
  (:import (java.lang.reflect Method)
           (java.util Optional)))

;; ========== 返回值转换 ==========
(defmulti convert-result (fn [return-type value] return-type) :default ::default)
(defmethod convert-result ::default [_ v] v)
(defmethod convert-result java.util.Optional [_ ^Optional opt]
  (when (.isPresent opt) (.get opt)))

(defmacro register-result-converter! [class-sym converter-fn]
  `(defmethod convert-result ~class-sym [_# v#] (~converter-fn v#)))

;; ========== 参数转换 ==========
(defmulti convert-arg (fn [param-type value] param-type) :default ::default)
(defmethod convert-arg ::default [_ v] v)

;; ========== 执行分派 ==========
(defmulti wrap-execution
          (fn [protocol-sym method-name impl this & args] [protocol-sym method-name])
          :default ::default)
(defmethod wrap-execution ::default [_ _ impl this & args]
  (apply impl this args))

;; ========== 内部反射 ==========
(defn- bean-method->fn-symbol [^Method m]
  (let [name (.getName m)
        [prefix body bang?]
        (cond
          (re-find #"^get[A-Z]" name) [nil (subs name 3) false]
          (re-find #"^set[A-Z]" name) ["set" (subs name 3) true]
          (re-find #"^is[A-Z]" name)  [nil (str (subs name 2) "?") false]
          :else                        [nil name false])
        hyphenated (-> body
                       (str/replace #"([A-Z])" #(str "-" (.toLowerCase (second %))))
                       (str/replace #"^-" ""))
        full (str (when prefix (str prefix "-")) hyphenated (when bang? "!"))]
    (symbol full)))

(defn- analyze-java-class* [class-sym]
  (let [cls (resolve class-sym)
        _ (when-not cls (throw (Exception. (str "Class not found: " class-sym))))
        methods (.getMethods cls)]
    (for [^Method m methods
          :when (not (#{:equals :hashCode :toString :getClass :notify :notifyAll :wait}
                      (keyword (.getName m))))]
      {:java-name   (.getName m)
       :clj-name    (bean-method->fn-symbol m)
       :return-type (.getReturnType m)
       :param-types (vec (.getParameterTypes m))
       :param-count (.getParameterCount m)})))

(defn- method-info->entry [info]
  {:protocol-fn (:clj-name info)
   :impl-spec   {:delegate (:java-name info)}
   :arity       (inc (:param-count info))
   :return-type (symbol (.getName (:return-type info)))
   :param-types (mapv #(symbol (.getName ^Class %)) (:param-types info))})

;; ========== 公共流水线 ==========
(defn analyze-class [class-sym]
  (mapv method-info->entry (analyze-java-class* class-sym)))

(defn via-methods [host-class specs]
  (let [host-cls (resolve host-class)]
    (mapcat (fn [spec]
              (let [[proto-fn getter target-method]
                    (if (= (count spec) 3)
                      spec
                      (let [proto-fn (first spec)
                            getter   (second spec)
                            camel (-> (name proto-fn)
                                      (str/split #"-")
                                      (->> (map str/capitalize)
                                           (apply str)))
                            camel (str (str/lower-case (subs camel 0 1)) (subs camel 1))]
                        [proto-fn getter (symbol camel)]))
                    gm (some #(when (= (.getName ^Method %) (name getter)) %)
                             (.getMethods host-cls))
                    _ (when-not gm (throw (Exception. (str "Getter not found: " (name getter)))))
                    target-cls (.getReturnType ^Method gm)
                    tm (some #(when (= (.getName ^Method %) (name target-method)) %)
                             (.getMethods target-cls))
                    _ (when-not tm (throw (Exception. (str "Target method not found: " (name target-method)))))
                    arity (inc (.getParameterCount ^Method tm))]
                [{:protocol-fn proto-fn
                  :impl-spec   {:via {:getter (symbol (name getter))
                                      :method (symbol (name target-method))}}
                  :arity       arity
                  :return-type (symbol (.getName (.getReturnType ^Method tm)))
                  :param-types (mapv #(symbol (.getName ^Class %)) (.getParameterTypes ^Method tm))}]))
            specs)))

(defn custom-methods [specs]
  (mapv (fn [[proto-fn arity f]]
          (assert (symbol? f) "custom function must be a symbol (Var name)")
          {:protocol-fn proto-fn
           :impl-spec   {:custom f}
           :arity       arity
           :return-type nil
           :param-types nil})
        specs))

(defn merge-methods [& colls]
  (vals (reduce (fn [m entries]
                  (reduce #(assoc %1 (:protocol-fn %2) %2) m entries))
                {} colls)))

(defn filter-methods [methods & {:keys [except only]}]
  (let [normalize (fn [x] (keyword (name x)))]
    (cond
      only   (let [keep-set (set (map normalize only))]
               (filter #(contains? keep-set (normalize (:protocol-fn %))) methods))
      except (let [drop-set (set (map normalize except))]
               (remove #(contains? drop-set (normalize (:protocol-fn %))) methods))
      :else  methods)))

(defn rename-methods [methods mapping]
  (let [normalize-key (fn [k] (keyword (name k)))]
    (mapv (fn [entry]
            (if-let [new-name (get mapping (normalize-key (:protocol-fn entry))
                                   (get mapping (:protocol-fn entry)))]
              (assoc entry :protocol-fn new-name)
              entry))
          methods)))

(defn auto-wrap-results [methods]
  (mapv (fn [entry]
          (if (contains? #{:delegate :via} (ffirst (:impl-spec entry)))
            (assoc entry :wrap-result true)
            entry))
        methods))

;; ========== 运行时扩展 ==========
(defn extend-by-methods* [class-sym protocol-sym methods]
  (let [method-map (reduce (fn [m {:keys [protocol-fn impl-spec arity]}]
                             (let [params (vec (repeatedly (dec arity) gensym))
                                   impl   (case (ffirst impl-spec)
                                            :delegate (let [jm (get-in impl-spec [:delegate])]
                                                        `(fn [~'this ~@params]
                                                           (. ~'this ~jm ~@params)))
                                            :via (let [{:keys [getter method]} (get-in impl-spec [:via])]
                                                   `(fn [~'this ~@params]
                                                      (let [obj# (. ~'this ~(symbol (name getter)))]
                                                        (. obj# ~(symbol (name method)) ~@params))))
                                            :custom (let [f (get-in impl-spec [:custom])]
                                                      `(fn [~'this ~@params] (~f ~'this ~@params))))]
                               (assoc m protocol-fn (eval impl))))
                           {} methods)]
    (eval `(extend ~class-sym ~protocol-sym ~@(mapcat identity method-map)))))

(defn inspect-java-class [class-sym]
  (doseq [e (analyze-class class-sym)]
    (println (str (:protocol-fn e) " (" (:arity e) " args)"))))

;; ========== 宏 ==========
(defmacro use-class
  [java-class protocol-sym & {:keys [except only rename custom dangerous via]
                              :or {except #{} only nil rename {} custom {} dangerous #{}}}]
  (let [proto-var (resolve protocol-sym)
        _ (assert proto-var (str "Protocol not found: " protocol-sym))
        proto-method-set (set (keys (:sigs @proto-var)))
        auto-entries   (-> (analyze-class java-class) (rename-methods rename))
        via-entries    (when via (via-methods java-class via))
        custom-specs   (if (map? custom)
                         (mapv (fn [[k [arity f]]] [k arity f]) custom)
                         (vec custom))
        _ (when custom-specs
            (doseq [[proto-fn arity f] custom-specs]
              (assert (symbol? f) (str "custom function for " proto-fn " must be a symbol (Var name)"))))
        custom-entries (custom-methods custom-specs)
        all-entries    (merge-methods auto-entries (or via-entries []) custom-entries)
        dangerous-renamed (mapv (fn [entry]
                                  (let [kw (keyword (name (:protocol-fn entry)))]
                                    (if (contains? dangerous kw)
                                      (update entry :protocol-fn #(symbol (str (name %) "!")))
                                      entry)))
                                all-entries)
        filtered (cond
                   only   (let [keep (set (map #(keyword (name %)) only))]
                            (filter #(contains? keep (keyword (name (:protocol-fn %)))) dangerous-renamed))
                   :else  (let [drop (set (map #(keyword (name %)) except))]
                            (remove #(contains? drop (keyword (name (:protocol-fn %)))) dangerous-renamed)))
        valid    (filter #(contains? proto-method-set (keyword (name (:protocol-fn %)))) filtered)
        wrapped  (auto-wrap-results valid)
        method-clauses
        (mapcat (fn [{:keys [protocol-fn impl-spec arity return-type wrap-result]}]
                  (let [this-sym 'this                                   ;; 固定用 this
                        arg-syms (vec (repeatedly (dec arity) #(gensym "arg")))
                        impl-type (ffirst impl-spec)
                        raw-impl
                        (case impl-type
                          :delegate (let [jm (get-in impl-spec [:delegate])]
                                      `(~(symbol (str "." jm)) ~this-sym ~@arg-syms))
                          :via (let [{:keys [getter method]} (get-in impl-spec [:via])]
                                 `(let [obj# (~(symbol (str "." (name getter))) ~this-sym)]
                                    (~(symbol (str "." (name method))) obj# ~@arg-syms)))
                          :custom (let [f (get-in impl-spec [:custom])]
                                    `(~f ~this-sym ~@arg-syms)))
                        final-impl
                        (if wrap-result
                          (list `convert-result return-type raw-impl)
                          raw-impl)
                        wrapped-impl
                        `(top.kzre.use-class.corex/wrap-execution
                           ~protocol-sym '~(symbol (name protocol-fn))
                           (fn [~this-sym ~@arg-syms] ~final-impl)
                           ~this-sym ~@arg-syms)]
                    ;; 关键：使用 (fn [this ...] ...) 形式，避免 extend-type 解析问题
                    `(~(symbol (name protocol-fn)) (fn [~this-sym ~@arg-syms] ~wrapped-impl))))
                wrapped)]
    `(extend-type ~java-class ~protocol-sym ~@method-clauses)))

(defmacro define-class
  [java-class protocol-name & {:keys [except only] :as opts}]
  (let [all-entries (analyze-class java-class)
        filtered (filter-methods all-entries :except except :only only)
        unique (vals (reduce (fn [m e]
                               (if (contains? m (keyword (name (:protocol-fn e))))
                                 m
                                 (assoc m (keyword (name (:protocol-fn e))) e)))
                             {} filtered))
        proto-sigs (mapv (fn [{:keys [protocol-fn arity]}]
                           (let [this-sym 'this
                                 arg-syms (vec (repeatedly (dec arity) #(gensym "arg")))]
                             `(~(symbol (name protocol-fn)) [~this-sym ~@arg-syms])))
                         unique)]
    `(do
       (defprotocol ~protocol-name ~@proto-sigs)
       (use-class ~java-class ~protocol-name ~@(mapcat identity opts)))))