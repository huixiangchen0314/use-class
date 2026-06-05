(ns top.kzre.use-class.core
  (:require [clojure.string :as str]
            [top.kzre.use-class.spec :as spec])
  (:import (java.lang.reflect Method)))

;; ── 工具函数 ──
(defn ->sym [x] (if (symbol? x) x (symbol (str x))))

(defn derive-protocol-name
  "从 Java 类名推导协议名：简单类名前加 I。"
  [class-sym]
  (let [simple (-> (str class-sym)
                   (str/replace #"^.*\." "")
                   (str/replace #"^I" ""))]
    (symbol (str "I" simple))))

(defn- unwrap-danger [method-sym]
  (let [s (name method-sym)]
    (if (str/ends-with? s "!")
      (symbol (subs s 0 (dec (count s))))
      method-sym)))

(defn- kebab->camel-safe [method-sym]
  "安全地将 kebab-case 转换为 camelCase，失败返回 nil。"
  (let [s (name method-sym)
        parts (str/split s #"-")]
    (if (= 1 (count parts))
      method-sym
      (let [prefix (first parts)
            rest-words (rest parts)]
        (if (#{"get" "set" "is"} prefix)
          (symbol (str prefix (apply str (map str/capitalize rest-words))))
          nil)))))

(defn- find-method-by-name-and-arity [cls method-name arity]
  (some #(when (and (= (.getName ^Method %) (name method-name))
                    (= (.getParameterCount ^Method %) arity))
           %)
        (.getMethods cls)))

(defonce ^:private object-method-names
         (->> (.getMethods Object)
              (map #(.getName ^Method %))
              set))

;; ── 步骤 1：解析 Java 类型 ──
(defn- method->sig [^Method m]
  (let [proto-name  (symbol (.getName m))
        param-types (.getParameterTypes m)
        params      (->> param-types
                         (map #(symbol (.getName ^Class %)))
                         (cons 'this)
                         vec)
        return      (symbol (.getName (.getReturnType m)))]
    [proto-name params return]))

(defn resolve-type [type-sym]
  (let [cls (resolve type-sym)]
    (when-not cls
      (throw (IllegalArgumentException. (str "无法解析的 Java 类型: " type-sym))))
    {::spec/type-name type-sym
     ::spec/method-sigs (->> (.getMethods cls)
                             (remove (comp object-method-names #(.getName ^Method %)))
                             (mapv method->sig))}))

;; ── 步骤：合并额外方法 ──
(defn merge-extra-methods [type-def delegate-config custom-config]
  (let [existing-names (set (map #(->sym (first %)) (::spec/method-sigs type-def)))
        extra-sigs
        (concat
          (for [entry delegate-config
                :let [proto-fn (->sym (first entry))]
                :when (not (contains? existing-names proto-fn))]   ;; 避免冲突
            [proto-fn '[this] nil])
          (for [[proto-fn arity custom-fn] custom-config
                :let [proto-fn (->sym proto-fn)]
                :when (not (contains? existing-names proto-fn))]   ;; 避免冲突
            (let [params (into ['this] (repeat (dec arity) 'java.lang.Object))]
              [proto-fn params nil])))]
    (update type-def ::spec/method-sigs into extra-sigs)))

;; ── 步骤 2：重命名 ──
(defn rename-methods-in-type-def [type-def rename-map rename-fn]
  (let [newname (fn [orig-name]
                  (->sym (or (get rename-map orig-name)
                             (when rename-fn (rename-fn orig-name))
                             orig-name)))]
    (update type-def ::spec/method-sigs
            (fn [sigs]
              (mapv (fn [[method-name params return]]
                      [(newname method-name) params return])
                    sigs)))))

;; ── 步骤 3：过滤 ──
(defn filter-methods-in-type-def [type-def default-include include exclude & {:keys [filter-fn]}]
  (let [include-set (set (map ->sym include))
        exclude-set (set (map ->sym exclude))
        pred (or filter-fn
                 (if default-include
                   #(not (contains? exclude-set (->sym %)))
                   #(contains? include-set (->sym %))))]
    (update type-def ::spec/method-sigs
            (fn [sigs]
              (filterv (fn [[method-sym & _]] (pred (->sym method-sym))) sigs)))))

;; ── 步骤 4：危险标记 ──
(defn- setter-name? [sym]
  (let [s (name sym)]
    (and (str/starts-with? s "set-") (> (count s) 4))))

(defn mark-dangerous-in-type-def [type-def danger-set & {:keys [setter-danger?] :or {setter-danger? true}}]
  (update type-def ::spec/method-sigs
          (fn [sigs]
            (mapv (fn [[method-name :as sig]]
                    (let [mn (->sym method-name)]
                      (if (and (not (str/ends-with? (name mn) "!"))
                               (or (contains? danger-set mn)
                                   (and setter-danger? (setter-name? mn))))
                        [(symbol (str (name mn) "!")) (nth sig 1) (nth sig 2)]
                        sig)))
                  sigs))))

;; ── 步骤 5：实现注入策略 ──
(defn direct-impl-policy [& {:keys [rename-inverse]}]
  (fn [[method-name params return] cls]
    (let [protocol-name (unwrap-danger method-name)
          ;; 优先使用 kebab->camel 转换，其次使用 rename 逆映射，最后使用原始名
          java-name (or (kebab->camel-safe protocol-name)
                        (get rename-inverse protocol-name)
                        protocol-name)
          arity (dec (count params))]
      (when (find-method-by-name-and-arity cls java-name arity)
        [method-name params return {:delegate java-name}]))))

(defn delegate-impl-policy [delegate-config]
  (let [mapping
        (into {}
              (map (fn [entry]
                     (let [proto-fn (->sym (first entry))
                           getter   (second entry)
                           target   (if (= (count entry) 2)
                                      (or (kebab->camel-safe proto-fn)
                                          (throw (IllegalArgumentException.
                                                   (str "无法从协议方法名 '" proto-fn
                                                        "' 推导目标方法名，请使用三元素形式 [协议方法 getter target]"))))
                                      (nth entry 2))]
                       [proto-fn {:delegate {:getter getter :method target}}])))
              delegate-config)]
    (fn [[method-name params return] cls]
      (when-let [impl (get mapping (->sym method-name))]
        [method-name params return impl]))))

(defn custom-impl-policy [custom-config]
  (let [mapping (into {} (map (fn [[k _ f]] [(->sym k) {:custom f}]) custom-config))]
    (fn [[method-name params return] cls]
      (when-let [impl (get mapping (->sym method-name))]
        [method-name params return impl]))))

(defn smart-delegate-policy [& {:keys [rename-inverse]}]
  (fn [[method-name params return] cls]
    (let [protocol-name (unwrap-danger method-name)
          java-name (or (kebab->camel-safe protocol-name)
                        (get rename-inverse protocol-name)
                        protocol-name)
          arity (dec (count params))]
      (some (fn [^Method g]
              (when (and (zero? (.getParameterCount g))
                         (not= java.lang.Void/TYPE (.getReturnType g)))
                (when-let [target (find-method-by-name-and-arity (.getReturnType g) java-name arity)]
                  [method-name params return
                   {:delegate {:getter (symbol (.getName g))
                               :method java-name}}])))
            (.getMethods cls)))))
(defn merge-impl-policies [& policies]
  (fn [sig cls]
    (some #(% sig cls) policies)))

(def default-impl-policy
  (merge-impl-policies (custom-impl-policy {})
                       (direct-impl-policy)
                       (smart-delegate-policy)))

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

;; ── 步骤 6：包装器 ──
(defn wrap-impl-in-type-def [type-def wrappers-config]
  (let [global-wrappers (get wrappers-config :global [])
        method-wrappers (get wrappers-config :methods {})]
    (update type-def ::spec/method-sigs
            (fn [sigs]
              (mapv (fn [sig]
                      (let [method-name (->sym (first sig))
                            wrappers (vec (concat global-wrappers (get method-wrappers method-name [])))]
                        (conj (vec sig) wrappers)))
                    sigs)))))

;; ── 代码生成 ──
(defn type-def->protocol-def [type-def proto-name]
  (let [sigs (::spec/method-sigs type-def)
        names (map #(->sym (first %)) sigs)
        duplicates (keys (filter (fn [[_ v]] (> v 1)) (frequencies names)))]
    (when (seq duplicates)
      (throw (ex-info (str "协议方法名冲突：" (str/join ", " (map name duplicates))
                           "。请使用 :rename 选项手动重命名以解决冲突。")
                      {:duplicates duplicates})))
    {::spec/protocol-name (->sym proto-name)
     ::spec/protocol-method-sigs
     (mapv (fn [[name params _]]
             (let [arity (dec (count params))]
               [(->sym name) (into ['this] (repeatedly arity #(gensym "arg")))]))
           sigs)}))

(defn emit-defprotocol [protocol-def]
  (let [proto-name (->sym (::spec/protocol-name protocol-def))
        sigs (::spec/protocol-method-sigs protocol-def)]
    `(defprotocol ~proto-name
       ~@(for [[name params] sigs]
           `(~(->sym name) ~params)))))

(defn- impl-expr [impl-map this-sym arg-syms]
  (let [[type val] (first impl-map)]
    (case type
      :delegate (if (symbol? val)
                  `(. ~this-sym ~val ~@arg-syms)
                  (let [getter (:getter val)
                        method (:method val)]
                    `(let [obj# (. ~this-sym ~getter)]
                       (. obj# ~method ~@arg-syms))))
      :custom `(~val ~this-sym ~@arg-syms))))  ;; 直接使用 val

(defn- wrap-expr [wrappers inner-expr this-sym arg-syms]
  (reduce (fn [expr wrapper-sym]
            `(~wrapper-sym (fn [~this-sym ~@arg-syms] ~expr)))
          inner-expr
          wrappers))

(defn emit-extend-type [type-def protocol-def]
  (let [type-sym (->sym (::spec/type-name type-def))
        proto-sym (->sym (::spec/protocol-name protocol-def))
        sigs (::spec/method-sigs type-def)
        proto-method-map (into {} (::spec/protocol-method-sigs protocol-def))
        clauses (for [[name _ _ impl wrappers] sigs
                      :let [proto-params (get proto-method-map (->sym name))
                            this-sym (first proto-params)
                            arg-syms (rest proto-params)
                            params-vec (vec proto-params)
                            body (wrap-expr wrappers
                                            (impl-expr impl this-sym arg-syms)
                                            this-sym arg-syms)]]
                  `(~(->sym name) ~params-vec ~body))]
    `(extend-type ~type-sym ~proto-sym ~@clauses)))

;; ── 顶层宏 ──
(defmacro defprotocol-from-type [java-class & opts]
  (let [{:keys [protocol-name rename rename-fn only except filter dangerous setter-danger? delegate custom]
         :or {rename {} dangerous #{} setter-danger? true delegate [] custom []}}
        (apply hash-map opts)
        proto-name (->sym (or protocol-name (derive-protocol-name java-class)))
        type-def (resolve-type java-class)
        type-def (rename-methods-in-type-def type-def rename rename-fn)
        type-def (merge-extra-methods type-def delegate custom)
        type-def (filter-methods-in-type-def type-def (nil? only) only except :filter-fn filter)
        type-def (mark-dangerous-in-type-def type-def dangerous :setter-danger? setter-danger?)
        proto-def (type-def->protocol-def type-def proto-name)
        ;; 实现注入不再需要，因为只生成协议
        ]
    (emit-defprotocol proto-def)))

(defmacro use-class [java-class & opts]
  (let [{:keys [protocol-name rename rename-fn only except filter dangerous setter-danger? delegate custom wrappers]
         :or {rename {} dangerous #{} setter-danger? true delegate [] custom [] wrappers {}}}
        (apply hash-map opts)
        proto-name (->sym (or protocol-name (derive-protocol-name java-class)))
        type-def (resolve-type java-class)
        type-def (rename-methods-in-type-def type-def rename rename-fn)     ;; 先重命名
        type-def (merge-extra-methods type-def delegate custom)            ;; 再合并
        type-def (filter-methods-in-type-def type-def (nil? only) only except :filter-fn filter)
        type-def (mark-dangerous-in-type-def type-def dangerous :setter-danger? setter-danger?)
        proto-def (type-def->protocol-def type-def proto-name)
        ;; 构建 rename 逆映射：新名 -> 原始 Java 方法名
        rename-inverse (zipmap (vals rename) (keys rename))
        delegate-config (mapv (fn [entry]
                                (let [proto-fn (->sym (or (get rename (first entry)) (first entry)))
                                      getter (second entry)
                                      target (when (= (count entry) 3) (nth entry 2))]
                                  (if target [proto-fn getter target] [proto-fn getter])))
                              delegate)
        custom-config (mapv (fn [[proto-fn arity f]]
                              [(->sym (or (get rename proto-fn) proto-fn)) arity f])
                            custom)
        policies (merge-impl-policies
                   (custom-impl-policy custom-config)
                   (delegate-impl-policy delegate-config)
                   (direct-impl-policy :rename-inverse rename-inverse)
                   (smart-delegate-policy :rename-inverse rename-inverse))
        type-def (inject-impl-in-type-def type-def policies)
        type-def (wrap-impl-in-type-def type-def wrappers)]
    `(do
       ~(emit-defprotocol proto-def)
       ~(emit-extend-type type-def proto-def))))