(ns top.kzre.use-class.core
  (:require
    [clojure.string :as str]
    [clojure.walk :as walk]
    [top.kzre.use-class.spec :as spec])
  (:import
    [java.lang.reflect Method Modifier]))

(defn bean-name->kebab-name
  "将 Java 方法名符号转为 Clojure kebab-case 符号，保留 get/set/is 前缀。
   例如：getTime → get-time，setTime → set-time，isReady → is-ready。"
  [method-sym]
  (let [name   (name method-sym)
        [prefix body]
        (cond
          (re-find #"^get[A-Z]" name) ["get" (subs name 3)]
          (re-find #"^set[A-Z]" name) ["set" (subs name 3)]
          (re-find #"^is[A-Z]" name)  ["is" (subs name 2)]
          :else                        [nil name])
        hyphenated (-> body
                       (str/replace #"([A-Z])" #(str "-" (.toLowerCase (second %))))
                       (str/replace #"^-" ""))
        full (str (when prefix (str prefix "-")) hyphenated)]
    (symbol full)))

(defn bean-name->kebab-name-strip
  "将 Java 方法名符号转为 Clojure kebab-case 符号，并去除 get/set/is 前缀。
   例如：getTime → time，setTime → set-time，isReady → ready?。"
  [method-sym]
  (let [name   (name method-sym)
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

;; ── 常量 ──
(defonce ^:private object-method-names
         (->> (.getMethods Object)
              (map #(.getName ^Method %))
              set))

(defn ->sym
  [x]
  (cond
    (symbol? x) x
    (keyword? x) (symbol (name x))
    (string? x) (symbol x)
    (and (seq? x) (= (first x) 'quote) (= (count x) 2))
    (->sym (second x))
    :else (throw (IllegalArgumentException. (str "无法转换为符号: " x)))))

(defn- kebab->camel-safe
  [method-sym]
  (let [s (name method-sym)
        parts (str/split s #"-")]
    (if (= 1 (count parts))
      method-sym
      (let [prefix (first parts)
            rest-words (rest parts)]
        (if (#{"get" "set" "is"} prefix)
          (symbol (str prefix (apply str (map str/capitalize rest-words))))
          nil)))))

(defn- unquote-form
  [form]
  (if (and (seq? form) (= (first form) 'quote) (= (count form) 2))
    (second form)
    form))

(defn- deep-unquote
  [coll]
  (walk/postwalk unquote-form coll))

;; ── 步骤 1：解析 Java 类型 ──
(defn- extract-methods [cls]
  (.getMethods cls))

(defn- filter-object-methods [methods]
  (remove (comp object-method-names #(.getName ^Method %)) methods))

(defn- static-method? [^Method m]
  (Modifier/isStatic (.getModifiers m)))

(defn- filter-static-methods [methods]
  (remove static-method? methods))

(defn- method->sig [^Method m]
  (let [jname       (symbol (.getName m))
        param-types (.getParameterTypes m)
        params      (->> param-types
                         (map #(symbol (.getName ^Class %)))
                         (cons 'this)
                         vec)
        return      (symbol (.getName (.getReturnType m)))]
    [jname jname params return]))

(defn resolve-type
  [type-sym]
  (let [cls (resolve type-sym)]
    (when-not cls
      (throw (IllegalArgumentException. (str "无法解析的 Java 类型: " type-sym))))
    {::spec/type-name type-sym
     ::spec/method-sigs (vec (->> cls
                                  extract-methods
                                  filter-object-methods
                                  filter-static-methods
                                  (mapv method->sig)))}))

;; ── 重命名 ──
(defn rename-methods-in-type-def
  [type-def rename-map rename-fn]
  (let [newname (fn [orig-name]
                  (or (get rename-map orig-name)
                      (when rename-fn (rename-fn orig-name))
                      orig-name))]
    (update type-def ::spec/method-sigs
            (fn [sigs]
              (mapv (fn [[proto java params return]]
                      [(newname proto) java params return])
                    sigs)))))

;; ── 前缀 ──
(defn add-prefix-to-type-def
  [type-def prefix]
  (if prefix
    (update type-def ::spec/method-sigs
            (fn [sigs]
              (mapv (fn [[proto java params return]]
                      [(symbol (str prefix (name proto))) java params return])
                    sigs)))
    type-def))

;; ── 过滤 ──
(defn filter-methods-in-type-def
  [type-def default-include include exclude]
  (let [include-set (set include)
        exclude-set (set exclude)
        pred (if default-include
               #(not (contains? exclude-set %))
               #(contains? include-set %))]
    (update type-def ::spec/method-sigs
            (fn [sigs]
              (filterv (fn [[proto]] (pred proto)) sigs)))))

;; ── 危险标记 ──
(defn- setter-name? [sym]
  (let [s (name sym)]
    (and (str/starts-with? s "set-") (> (count s) 4))))

(defn mark-dangerous-in-type-def
  [type-def danger-set & {:keys [setter-danger?] :or {setter-danger? true}}]
  (update type-def ::spec/method-sigs
          (fn [sigs]
            (mapv (fn [[proto java params return]]
                    (if (and (not (str/ends-with? (name proto) "!"))
                             (or (contains? danger-set proto)
                                 (and setter-danger? (setter-name? proto))))
                      [(symbol (str (name proto) "!")) java params return]
                      [proto java params return]))
                  sigs))))

;; ── 去重 ──
(defn dedupe-methods [type-def]
  (update type-def ::spec/method-sigs
          (fn [sigs]
            (vec (vals (reduce (fn [m [proto java params return :as sig]]
                                 (let [arity (dec (count params))]
                                   (if-let [existing (get m proto)]
                                     (let [existing-arity (dec (count (nth existing 2)))]
                                       (if (> arity existing-arity)
                                         (assoc m proto sig)
                                         m))
                                     (assoc m proto sig))))
                               {}
                               sigs))))))

;; ── 合并额外方法 ──
(defn merge-extra-methods [type-def delegate-config custom-config]
  (let [existing-names (set (map first (::spec/method-sigs type-def)))
        extra-sigs
        (concat
          (for [entry delegate-config
                :let [proto-fn (->sym (first entry))]
                :when (not (contains? existing-names proto-fn))]
            [proto-fn proto-fn '[this] nil])
          (for [[proto-fn arity custom-fn & [param-types]] custom-config
                :let [proto-fn (->sym proto-fn)]
                :when (not (contains? existing-names proto-fn))]
            (let [params (if param-types
                           (into ['this] param-types)
                           (into ['this] (repeat (dec arity) 'java.lang.Object)))]
              [proto-fn proto-fn params nil])))]
    (update type-def ::spec/method-sigs into extra-sigs)))

;; ── 合并 type-def ──
(defn merge-type-def
  [base-type-def & more-type-defs]
  (let [all-sigs (reduce (fn [sigs td]
                           (reduce (fn [acc sig]
                                     (if (some #(= (first %) (first sig)) acc)
                                       acc
                                       (conj acc sig)))
                                   sigs
                                   (::spec/method-sigs td)))
                         (::spec/method-sigs base-type-def)
                         more-type-defs)]
    (assoc base-type-def ::spec/method-sigs (vec all-sigs))))

;; ── 委托辅助函数 ──
(defn- find-method-by-name-and-arity [cls method-name arity]
  (some #(when (and (= (.getName ^Method %) (name method-name))
                    (= (.getParameterCount ^Method %) arity))
           %)
        (.getMethods cls)))

(defn resolve-conflict-methods
  [sigs method-name]
  (let [candidates (filter #(= (second %) method-name) sigs)
        _ (when (empty? candidates)
            (throw (ex-info (str "未找到方法 " method-name) {})))
        groups (group-by (fn [[_ _ params _]] (dec (count params))) candidates)
        max-arity (apply max (keys groups))
        top-group (get groups max-arity)]
    (first top-group)))

(defn gen-delegate-entries [host-class-sym getter-sym]
  (let [host-cls      (resolve host-class-sym)
        _             (when-not host-cls (throw (IllegalArgumentException. (str "无法解析类型: " host-class-sym))))
        getter-method (first (filter #(= (.getName ^Method %) (name getter-sym)) (.getMethods host-cls)))
        _             (when-not getter-method (throw (ex-info (str "Getter " getter-sym " not found in " host-class-sym) {})))
        ret-class     (.getReturnType ^Method getter-method)
        ret-class-sym (symbol (.getName ret-class))
        target-td     (resolve-type ret-class-sym)
        method-names  (map second (::spec/method-sigs target-td))]
    (mapv (fn [m] [m getter-sym]) method-names)))

(defn normalize-delegate-config [host-class-sym raw-config]
  (mapcat
    (fn [entry]
      (let [entry (if (and (seq? entry) (= (first entry) 'quote) (= (count entry) 2))
                    (->sym entry)
                    entry)
            entry (if (symbol? entry) [entry] entry)
            entry (mapv ->sym entry)
            getter (first entry)
            target-methods (rest entry)]
        (if (seq target-methods)
          (mapv (fn [m] [m getter]) target-methods)
          (gen-delegate-entries host-class-sym getter))))
    raw-config))

(defn merge-type-def-with-delegates [host-type-def delegate-config]
  (let [host-class-sym (::spec/type-name host-type-def)
        extra-sigs
        (mapcat
          (fn [entry]
            (let [[target-method getter] entry
                  host-cls      (resolve host-class-sym)
                  getter-method (first (filter #(= (.getName ^Method %) (name getter)) (.getMethods host-cls)))
                  _             (when-not getter-method
                                  (throw (ex-info (str "Getter " getter " not found in " host-class-sym) {})))
                  ret-class     (.getReturnType ^Method getter-method)
                  ret-class-sym (symbol (.getName ret-class))
                  target-td     (resolve-type ret-class-sym)
                  ;; 用原始 Java 方法名查找
                  target-sig    (resolve-conflict-methods (::spec/method-sigs target-td) target-method)
                  _             (when-not target-sig
                                  (throw (ex-info (str "Target method " target-method " not found in " ret-class-sym) {})))]
              (let [[_ target-java target-params target-return] target-sig
                    new-params (cons 'this (rest target-params))]
                ;; proto-name 暂用 target-method（原始 Java 名），后续由 rename 处理
                [[target-method target-java new-params target-return]])))
          delegate-config)]
    (merge-type-def host-type-def
                    {::spec/type-name ::merge-placeholder
                     ::spec/method-sigs (vec extra-sigs)})))

;; ── 构建最终的 type-def ──
(defn build-type-def
  [java-class & {:keys [rename rename-fn only except dangerous setter-danger? delegate custom prefix]
                 :or {rename {} dangerous #{} setter-danger? true delegate [] custom [] prefix nil}}]
  (let [raw-only  (deep-unquote only)
        raw-except (deep-unquote except)
        dangerous (deep-unquote dangerous)
        delegate  (deep-unquote delegate)
        custom    (deep-unquote custom)
        rename    (deep-unquote rename)
        rename-fn (if (some? rename-fn) rename-fn bean-name->kebab-name)
        ;; 名称转换函数
        resolve-name (fn [orig-name]
                       (let [renamed (or (get rename orig-name) orig-name)]
                         (if rename-fn (rename-fn renamed) renamed)))
        type-def (resolve-type java-class)
        delegate-entries (normalize-delegate-config java-class delegate)
        type-def (if (seq delegate-entries)
                   (merge-type-def-with-delegates type-def delegate-entries)
                   type-def)
        type-def (merge-extra-methods type-def [] custom)
        type-def (rename-methods-in-type-def type-def rename rename-fn)
        type-def (add-prefix-to-type-def type-def prefix)
        ;; 转换过滤列表
        only-seq   (mapv resolve-name (or raw-only []))
        except-seq (mapv resolve-name (or raw-except []))
        ;; 决定过滤策略
        default-include (nil? raw-only)
        type-def (filter-methods-in-type-def type-def default-include only-seq except-seq)
        type-def (mark-dangerous-in-type-def type-def dangerous :setter-danger? setter-danger?)
        type-def (dedupe-methods type-def)]
    type-def))

;; ── 协议定义生成 ──
(defn- core-publics []
  (set (keys (ns-publics 'clojure.core))))

(defn filter-core-conflicts
  [sigs]
  (let [core-syms (core-publics)
        conflicts (filter #(contains? core-syms (first %)) sigs)]
    (when (seq conflicts)
      (binding [*out* *err*]
        (println "警告：以下协议方法名与 clojure.core 冲突，已自动从协议中移除："
                 (str/join ", " (map (comp name first) conflicts)))))
    (remove #(contains? core-syms (first %)) sigs)))

(defn type-def->protocol-def
  [type-def proto-name]
  (let [sigs (filter-core-conflicts (::spec/method-sigs type-def))]
    {::spec/protocol-name (->sym proto-name)
     ::spec/protocol-method-sigs
     (mapv (fn [[proto _ params _]]
             (let [arity (dec (count params))]
               [proto (into ['this] (repeatedly arity #(gensym "arg")))]))
           sigs)}))

(defn emit-defprotocol
  [protocol-def]
  (let [proto-name (::spec/protocol-name protocol-def)
        sigs (::spec/protocol-method-sigs protocol-def)]
    `(defprotocol ~proto-name
       ~@(for [[name params] sigs]
           `(~name ~params)))))

(defn derive-protocol-name
  [class-sym]
  (let [simple (-> (str class-sym)
                   (str/replace #"^.*\." "")
                   (str/replace #"^I" ""))]
    (symbol (str "I" simple))))

;; ── 顶层宏：生成协议 ──
(defmacro defprotocol-from-type [java-class & opts]
  (let [opts-map (apply hash-map opts)
        java-class-sym (->sym java-class)
        proto-name (->sym (or (:protocol-name opts-map) (derive-protocol-name java-class-sym)))
        type-def (apply build-type-def java-class-sym (mapcat identity opts-map))
        proto-def (type-def->protocol-def type-def proto-name)]
    (emit-defprotocol proto-def)))

;; ── 实现注入与包装器 ──
(defn- unwrap-danger [method-sym]
  (let [s (name method-sym)]
    (if (str/ends-with? s "!")
      (symbol (subs s 0 (dec (count s))))
      method-sym)))

(defn direct-impl-policy [& {:keys [rename-inverse]}]
  (fn [[proto java params return :as sig] cls]
    (when (find-method-by-name-and-arity cls java (dec (count params)))
      [proto java params return {:delegate java}])))

(defn delegate-impl-policy [delegate-config]
  (let [mapping (into {} (map (fn [[proto-fn getter target-method]]
                                [proto-fn {:delegate {:getter getter :method target-method}}])
                              delegate-config))]
    (fn [[proto java params return :as sig] cls]
      (when-let [impl (get mapping proto)]
        [proto java params return impl]))))

(defn custom-impl-policy [custom-config]
  (let [mapping (into {} (map (fn [[k _ f]] [k {:custom f}]) custom-config))]
    (fn [[proto java params return :as sig] cls]
      (when-let [impl (get mapping proto)]
        [proto java params return impl]))))

(defn smart-delegate-policy [& {:keys [rename-inverse delegate-classes]}]
  (fn [[proto java params return :as sig] cls]
    (let [java-name java
          arity (dec (count params))
          getters (if delegate-classes
                    (filter #(contains? delegate-classes (.getReturnType ^Method %))
                            (.getMethods cls))
                    (.getMethods cls))]
      (some (fn [^Method g]
              (when (and (zero? (.getParameterCount g))
                         (not= java.lang.Void/TYPE (.getReturnType g)))
                (when-let [target (find-method-by-name-and-arity (.getReturnType g) java-name arity)]
                  [proto java params return
                   {:delegate {:getter (symbol (.getName g))
                               :method java-name}}])))
            getters))))

(defn merge-impl-policies [& policies]
  (fn [sig cls]
    (some #(% sig cls) policies)))

(defn inject-impl-in-type-def [type-def impl-policy]
  (let [cls (resolve (::spec/type-name type-def))]
    (when-not cls (throw (ex-info "Cannot resolve type" {:type (::spec/type-name type-def)})))
    (update type-def ::spec/method-sigs
            (fn [sigs]
              (mapv (fn [sig]
                      (or (impl-policy sig cls)
                          (throw (ex-info (str "No implementation found for " (first sig))
                                          {:method (first sig)}))))
                    sigs)))))

(defn impl-expr
  [impl-map this-sym arg-syms]
  (let [[type val] (first impl-map)]
    (case type
      :delegate (if (symbol? val)
                  `(. ~this-sym ~val ~@arg-syms)
                  (let [getter (:getter val)
                        method (:method val)]
                    `(let [obj# (. ~this-sym ~getter)]
                       (. obj# ~method ~@arg-syms))))
      :custom `(~val ~this-sym ~@arg-syms))))

(defn wrap-expr
  [wrappers inner-expr this-sym arg-syms]
  (reduce (fn [expr wrapper-sym]
            `(~wrapper-sym (fn [~this-sym ~@arg-syms] ~expr)))
          inner-expr
          wrappers))

(defn wrap-impl-in-type-def
  [type-def wrappers-config]
  (let [global-wrappers (get wrappers-config :global [])
        method-wrappers (get wrappers-config :methods {})]
    (update type-def ::spec/method-sigs
            (fn [sigs]
              (mapv (fn [sig]
                      (let [proto (first sig)
                            wrappers (vec (concat global-wrappers (get method-wrappers proto [])))]
                        (conj (vec sig) wrappers)))
                    sigs)))))

(defn emit-extend-type [type-def protocol-def]
  (let [type-sym (::spec/type-name type-def)
        proto-sym (::spec/protocol-name protocol-def)
        sigs (::spec/method-sigs type-def)
        proto-method-map (into {} (map (fn [[k v]] [(keyword (name k)) v])
                                       (::spec/protocol-method-sigs protocol-def)))
        clauses (for [[proto java params _ impl wrappers] sigs
                      :let [proto-keyword (keyword (name proto))
                            proto-params (get proto-method-map proto-keyword)]
                      :when proto-params
                      :let [this-sym (first proto-params)
                            arg-syms (rest proto-params)
                            params-vec (vec proto-params)
                            body (wrap-expr wrappers
                                            (impl-expr impl this-sym arg-syms)
                                            this-sym arg-syms)]]
                  `(~proto ~params-vec ~body))]
    `(extend-type ~type-sym ~proto-sym ~@clauses)))

;; ── 顶层宏：生成协议和实现 ──
(defmacro use-class [java-class & opts]
  (let [opts-map (apply hash-map opts)
        java-class-sym (->sym java-class)
        proto-name (->sym (or (:protocol-name opts-map) (derive-protocol-name java-class-sym)))
        type-def (apply build-type-def java-class-sym (mapcat identity opts-map))
        proto-def (type-def->protocol-def type-def proto-name)
        rename      (deep-unquote (:rename opts-map {}))
        delegate    (deep-unquote (:delegate opts-map []))
        custom      (deep-unquote (:custom opts-map []))
        wrappers    (deep-unquote (:wrappers opts-map {}))
        delegate-classes (deep-unquote (:delegate-classes opts-map nil))
        rename-fn   (or (:rename-fn opts-map) bean-name->kebab-name)
        resolve-name (fn [orig-name]
                       (let [renamed (or (get rename orig-name) orig-name)]
                         (if rename-fn (rename-fn renamed) renamed)))
        ;; 委托条目处理（与 build-type-def 中保持一致）
        normalized-delegate (normalize-delegate-config java-class-sym delegate)
        delegate-config (mapv (fn [[target-method getter]]
                                [(resolve-name target-method) getter target-method])
                              normalized-delegate)
        custom-config (mapv (fn [[k a f]] [(resolve-name k) a f]) custom)
        rename-inverse (zipmap (vals rename) (keys rename))
        policies (merge-impl-policies
                   (custom-impl-policy custom-config)
                   (delegate-impl-policy delegate-config)
                   (direct-impl-policy :rename-inverse rename-inverse)
                   (smart-delegate-policy :rename-inverse rename-inverse :delegate-classes delegate-classes))
        type-def (inject-impl-in-type-def type-def policies)
        type-def (wrap-impl-in-type-def type-def wrappers)]
    `(do
       ~(emit-defprotocol proto-def)
       ~(emit-extend-type type-def proto-def))))