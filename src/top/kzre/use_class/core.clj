(ns top.kzre.use-class.core
  (:require
    [clojure.string :as str]
    [top.kzre.use-class.spec :as spec]
    [top.kzre.use-class.util :as util])
  (:import
    (java.lang.reflect Method Modifier)))

(def ^:const sig-proto     0)  ; 协议方法名
(def ^:const sig-java      1)  ; 原始 Java 方法名
(def ^:const sig-params    2)  ; 参数向量（含 this）
(def ^:const sig-vararg-type    3)  ; 返回类型
(def ^:const sig-impl      4)  ; 实现策略
(def ^:const sig-wrappers  5)  ; 包装器列表

(def ^:private primitive-type-map
  "将原始类型符号映射到对应的 Class 符号，供 into-array 使用。"
  {'int 'Integer/TYPE
   'boolean 'Boolean/TYPE
   'byte 'Byte/TYPE
   'short 'Short/TYPE
   'long 'Long/TYPE
   'char 'Character/TYPE
   'float 'Float/TYPE
   'double 'Double/TYPE})

;; ================ Resolve Java Type ==================================
(defn- java-arity [params]
  (let [without-this (rest params)
        fixed (take-while #(not= '& %) without-this)]
    (count fixed)))

(defonce ^:private object-method-names
         (->> (.getMethods Object)
              (map #(.getName ^Method %))
              set))

(defn- extract-methods [cls]
  (.getMethods cls))

(defn- filter-object-methods [methods]
  (remove (comp object-method-names #(.getName ^Method %)) methods))

(defn- static-method? [^Method m]
  (Modifier/isStatic (.getModifiers m)))

(defn- filter-static-methods [methods]
  (remove static-method? methods))

(defn- java-method->method-sig [^Method m]
  (let [jname    (symbol (.getName m))
        arity    (.getParameterCount m)
        varargs? (.isVarArgs m)
        params   (if varargs?
                   (vec (concat ['this] (repeatedly (dec arity) #(gensym "arg")) ['& (gensym "rest")]))
                   (vec (cons 'this (repeatedly arity #(gensym "arg")))))
        vararg-type (when varargs?
                      (let [last-param-type (last (.getParameterTypes m))]
                        (when (.isArray last-param-type)
                          (let [component-type (.getComponentType last-param-type)
                                type-sym (symbol (.getName component-type))]
                            (get primitive-type-map type-sym type-sym)))))] ; 原始类型转包装类 TYPE
    [jname jname params vararg-type]))

(defn resolve-type
  "解析 Java 类型。接受：
   - 类对象（如 java.util.Date）
   - 符号（如 'Date、'java.util.Date）
   - 字符串（如 \"java.util.Date\"）
   返回包含 ::spec/type-name 和 ::spec/method-sigs 的 map。"
  [type-spec]
  (let [cls (cond
              (class? type-spec) type-spec
              (symbol? type-spec) (or (resolve type-spec)
                                      (throw (IllegalArgumentException. (str "Symbol cannot be resolved: " type-spec))))
              (string? type-spec) (resolve-type (symbol type-spec)) ; 字符串转符号递归
              :else (throw (IllegalArgumentException. (str "无效的类型参数: " type-spec))))]
    (when-not cls
      (throw (IllegalArgumentException. (str "Dont known how to handle type:" type-spec))))
    (let [class-name-sym (symbol (.getName ^Class cls))]
      {::spec/type-name class-name-sym
       ::spec/method-sigs (vec (->> cls
                                    extract-methods
                                    filter-object-methods
                                    filter-static-methods
                                    (mapv java-method->method-sig)))})))

;; ── 重命名 ──
(defn rename-methods-in-type-def
  [type-def rename-map rename-fn]
  (let [newname (fn [orig-name]
                  (or (get rename-map orig-name)
                      (when rename-fn (rename-fn orig-name))
                      orig-name))]
    (update type-def ::spec/method-sigs
            (fn [sigs]
              (mapv (fn [[proto java params vararg-type]]
                      [(newname proto) java params vararg-type])
                    sigs)))))

;; ── 前缀 ──
(defn add-prefix-to-type-def
  [type-def prefix]
  (if prefix
    (update type-def ::spec/method-sigs
            (fn [sigs]
              (mapv (fn [[proto java params vararg-type]]
                      [(symbol (str prefix (name proto))) java params vararg-type])
                    sigs)))
    type-def))

;; ── 过滤 ──

(defn normalize-method-entries
  "将用户提供的过滤条目统一为 `[name arity]` 向量序列。
   每个条目可以是一个符号（仅按名称过滤，arity 为 nil），
   或一个 `[name arity]` 向量（按名称和参数个数过滤）。
   返回一个向量，其中每个元素为 `[name arity]`。"
  [entries]
  (mapv (fn [entry]
          (if (vector? entry)
            [(first entry) (second entry)]
            [entry nil]))
        entries))

(defn filter-methods-in-type-def
  [type-def default-include include-norm exclude-norm]
  (let [
        matches? (fn [specs java-name arity]
                   (some (fn [[n a]]
                           (and (= n java-name)
                                (or (nil? a) (= a arity))))
                         specs))]
    (update type-def ::spec/method-sigs
            (fn [sigs]
              (filterv (fn [[_ java params]]
                         (let [arity (java-arity params)]
                           (if default-include
                             (not (matches? exclude-norm java arity))
                             (matches? include-norm java arity))))
                       sigs)))))

;; ── 危险标记 ──
(defn mark-dangerous-in-type-def
  [type-def danger-set & {:keys [setter-danger?] :or {setter-danger? true}}]
  (update type-def ::spec/method-sigs
          (fn [sigs]
            (mapv (fn [[proto java params vararg-type]]
                    (if (and (not (str/ends-with? (name proto) "!"))
                             (or (contains? danger-set java)
                                 (and setter-danger? (util/setter-name? java))))
                      [(symbol (str (name proto) "!")) java params vararg-type]
                      [proto java params vararg-type]))
                  sigs))))

;; ── 去重 ──

(defn- dedupe-method-sigs
  "给定方法名和它的签名列表，根据 `explicit-arities` 决定保留哪些签名。
   - 若方法名在 `explicit-arities` 中，保留所有参数个数匹配的签名（支持重载）。
   - 否则，只保留参数个数最大的一个签名。"
  [proto sigs explicit-arities]
  (if-let [arities (get explicit-arities proto)]
    (filter (fn [[_ _ params]]
              (contains? arities (java-arity params)))
            sigs)
    (let [max-sig (reduce (fn [s1 s2]
                            (if (> (dec (count (nth s2 2)))
                                   (dec (count (nth s1 2))))
                              s2
                              s1))
                          (first sigs)
                          (rest sigs))]
      [max-sig])))

(defn dedupe-methods [type-def]
  (update type-def ::spec/method-sigs
          (fn [sigs]
            (vec (vals (reduce (fn [m [proto java params vararg-type :as sig]]
                                 (let [arity (java-arity params)]
                                   (if-let [existing (get m proto)]
                                     (let [existing-arity (dec (count (nth existing 2)))]
                                       (if (> arity existing-arity)
                                         (assoc m proto sig)
                                         m))
                                     (assoc m proto sig))))
                               {}
                               sigs))))))

;; ── 合并额外方法 ──
(defn merge-type-def-with-custom-methods
  "Merges user-defined custom method signatures into the type-def."
  [type-def custom-config]
  (update type-def ::spec/method-sigs
          into (for [[proto-fn arity custom-fn & [param-types]] custom-config
                     :let [proto-fn (util/->sym proto-fn)
                           params (if param-types
                                    (into ['this] param-types)
                                    (into ['this] (repeat (dec arity) 'java.lang.Object)))]]
                 [proto-fn proto-fn params nil])))

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
                    (let [pcount (.getParameterCount %)]
                      (if (.isVarArgs %)
                        (= (dec pcount) arity)    ;; 变参方法，比较固定参数个数
                        (= pcount arity))))       ;; 普通方法
           %)
        (.getMethods cls)))

(defn resolve-conflict-methods
  [sigs method-name]
  (let [candidates (filter #(= (second %) method-name) sigs)
        _ (when (empty? candidates)
            (throw (ex-info (str "未找到方法 " method-name) {})))
        groups (group-by (fn [[_ _ params _]] (java-arity params)) candidates)
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
                    (util/->sym entry)
                    entry)
            entry (if (symbol? entry) [entry] entry)
            entry (mapv util/->sym entry)
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
              (let [[_ target-java target-params target-vararg-type] target-sig
                    new-params (cons 'this (rest target-params))]
                ;; proto-name 暂用 target-method（原始 Java 名），后续由 rename 处理
                [[target-method target-java new-params target-vararg-type]])))
          delegate-config)]
    (merge-type-def host-type-def
                    {::spec/type-name ::merge-placeholder
                     ::spec/method-sigs (vec extra-sigs)})))



(defn- ensure-explicit-arities
  "根据用户通过 `:only` 明确指定的参数个数（arity），向 type-def 中添加缺失的方法签名。
   信任用户配置，若方法被过滤或不存在，则静默跳过（后续代码生成阶段会报错）。"
  [type-def only-entries]
  (let [arities-map (reduce (fn [m [name arity]]
                              (if arity
                                (update m name (fnil conj #{}) arity)
                                m))
                            {}
                            only-entries)]
    (if (seq arities-map)
      (update type-def ::spec/method-sigs
              (fn [sigs]
                (let [by-name (group-by first sigs)]
                  (mapcat (fn [[method arities]]
                            (let [existing (get by-name method [])]
                              (concat existing
                                      ;; 仅在存在至少一个现有签名时才补充缺失的 arity
                                      (when-let [template (first existing)]
                                        (for [a arities
                                              :when (not (contains? (set (map (comp java-arity #(nth % 2)) existing)) a))]
                                          (let [java-name (second template)
                                                params (vec (cons 'this (repeatedly a #(gensym "arg"))))]
                                            [method java-name params (nth template 3 nil)]))))))
                          arities-map))))
      type-def)))

(defn build-type-def
  [java-class-sym & {:keys [only except
                            rename rename-fn dangerous setter-danger? prefix
                            delegate custom ]
                     :or {rename {} dangerous #{} setter-danger? true delegate [] custom [] prefix nil}}]
  (let [only  (util/deep-unquote only)
        except (util/deep-unquote except)
        default-include (nil? only)
        only-entries   (normalize-method-entries (or only []))
        except-entries (normalize-method-entries (or except []))
        dangerous (util/deep-unquote dangerous)
        delegate  (util/deep-unquote delegate)
        custom    (util/deep-unquote custom)
        rename    (util/deep-unquote rename)
        rename-fn (or rename-fn util/bean-name->kebab-name)
        ;; resolve
        type-def (resolve-type java-class-sym)
        _ (println "After resolve, methods:" (mapv first (::spec/method-sigs type-def)))
        delegate-entries (normalize-delegate-config java-class-sym delegate)
        type-def (if (seq delegate-entries)
                   (merge-type-def-with-delegates type-def delegate-entries)
                   type-def)
        _ (println "After delegate merge, methods:" (mapv first (::spec/method-sigs type-def)))
        type-def (merge-type-def-with-custom-methods type-def custom)
        _ (println "After custom merge, methods:" (mapv first (::spec/method-sigs type-def)))
        ;; filter

        _ (println "Filtering: default-include =" default-include ", only =" only ", except =" except)
        type-def (filter-methods-in-type-def type-def default-include only-entries except-entries)
        _ (println "After filter, methods:" (mapv first (::spec/method-sigs type-def)))
        type-def (dedupe-methods type-def)
        _ (println "After dedupe, methods:" (mapv first (::spec/method-sigs type-def)))
        type-def (ensure-explicit-arities type-def only-entries)
        _ (println "After ensure-explicit-arities , methods:" (mapv first (::spec/method-sigs type-def)))
        ;; rename
        type-def (rename-methods-in-type-def type-def rename rename-fn)
        _ (println "After rename, methods:" (mapv first (::spec/method-sigs type-def)))
        type-def (add-prefix-to-type-def type-def prefix)
        _ (println "After prefix, methods:" (mapv first (::spec/method-sigs type-def)))
        type-def (mark-dangerous-in-type-def type-def dangerous :setter-danger? setter-danger?)
        _ (println "After dangerous, methods:" (mapv first (::spec/method-sigs type-def)))
        ]
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

(defn type-def->protocol-def [type-def proto-name]
  (let [sigs (filter-core-conflicts (::spec/method-sigs type-def))]
    {::spec/protocol-name (util/->sym proto-name)
     ::spec/protocol-method-sigs
     (mapv (fn [sig]
             (let [proto (first sig)
                   ;; 如果缓存存在（长度 >= 7），则使用缓存的包装器参数，否则用原始参数
                   params (if (>= (count sig) 7) (nth sig 6) (nth sig 2))]
               [proto params]))
           sigs)}))

(defn- group-method-sigs-by-name
  "将 [[method params] ...] 转换为 {method [params ...]}，保持原顺序。"
  [flat-sigs]
  (let [grouped (reduce (fn [m [name params]]
                          (update m name (fnil conj []) params))
                        {}
                        flat-sigs)]
    (println "[group-method-sigs-by-name] input:" (mapv (fn [[m p]] [m (vec p)]) flat-sigs))
    (println "[group-method-sigs-by-name] output:" (into {} (map (fn [[k v]] [k (mapv vec v)]) grouped)))
    grouped))

(defn emit-defprotocol [proto-name method-specs]
  "生成 defprotocol 表单。
   proto-name: 协议名符号
   method-specs: [[method params] ...] 标准化列表，可能含重载。"
  (let [grouped (group-method-sigs-by-name method-specs)]
    `(defprotocol ~proto-name
       ~@(mapcat (fn [[method paramlists]]
                   `((~method ~@paramlists)))
                 grouped))))

(defn default-protocol-name
  [class-sym]
  (let [simple (-> (str class-sym)
                   (str/replace #"^.*\." "")
                   (str/replace #"^I" ""))]
    (symbol (str "I" simple))))

;; ── 顶层宏：生成协议 ──
(defmacro defprotocol-from-type [java-class & opts]
  (let [opts-map (apply hash-map opts)
        java-class-sym (util/->sym java-class)
        proto-name (util/->sym (or (:protocol-name opts-map) (default-protocol-name java-class-sym)))
        type-def (apply build-type-def java-class-sym (mapcat identity opts-map))
        ;; 构建标准化方法-参数列表（原始参数，无包装器调整）
        method-specs (mapv (fn [sig] [(first sig) (nth sig 2)])
                           (::spec/method-sigs type-def))]
    `(emit-defprotocol ~proto-name ~method-specs)))

;; ── 实现注入与包装器 ──
(defn- unwrap-danger [method-sym]
  (let [s (name method-sym)]
    (if (str/ends-with? s "!")
      (symbol (subs s 0 (dec (count s))))
      method-sym)))

(defn direct-impl-policy [& {:keys [rename-inverse]}]
  (fn [[proto java params vararg-type :as sig] cls]
    (when (find-method-by-name-and-arity cls java (java-arity params))
      [proto java params vararg-type {:delegate java}])))

(defn delegate-impl-policy [delegate-config]
  (let [mapping (into {} (map (fn [[proto-fn getter target-method]]
                                [proto-fn {:delegate {:getter getter :method target-method}}])
                              delegate-config))]
    (fn [[proto java params vararg-type :as sig] cls]
      (when-let [impl (get mapping proto)]
        [proto java params vararg-type impl]))))

(defn custom-impl-policy
  "返回一个策略函数，根据用户提供的自定义方法配置来提供实现。
   配置格式：[[method arity? fn] ...] 或 [[method fn] ...]
   内部会根据 rename-map 和 rename-fn 将原始方法名转换为协议方法名。"
  [custom-config resolve-name]
  (let [;; 解析配置条目，标准化为 [原始名 arity fn] 三元组
        parse-entry (fn [entry]
                      (if (and (vector? entry) (>= (count entry) 2))
                        (let [[method arity-or-fn impl-fn] entry]
                          (if (or (number? arity-or-fn) (nil? arity-or-fn))
                            [method arity-or-fn (if (>= (count entry) 3) impl-fn arity-or-fn)]
                            [method nil arity-or-fn])) ;; 当第二个元素不是数字时，视为函数
                        (throw (IllegalArgumentException. "Custom entry must be a vector [method arity? fn]"))))
        entries (mapv parse-entry custom-config)
        ;; 构建查找映射：{ [协议名 fixed-arity] {:custom fn} } 或 { 协议名 {:custom fn} }
        mapping (reduce (fn [m [orig-name arity impl-fn]]
                          (let [proto-name (resolve-name orig-name)
                                key (if arity [proto-name arity] proto-name)]
                            (assoc m key {:custom impl-fn})))
                        {}
                        entries)]
    (fn [[proto java params vararg-type :as sig] cls]
      (let [fixed-arity (java-arity params)
            result (or (get mapping [proto fixed-arity])
                       (get mapping proto))]
        (when result
          [proto java params vararg-type result])))))

(defn smart-delegate-policy [& {:keys [rename-inverse delegate-classes]}]
  (fn [[proto java params vararg-type :as sig] cls]
    (let [java-name java
          arity (java-arity params)
          getters (if delegate-classes
                    (filter #(contains? delegate-classes (.getReturnType ^Method %))
                            (.getMethods cls))
                    (.getMethods cls))]
      (some (fn [^Method g]
              (when (and (zero? (.getParameterCount g))
                         (not= java.lang.Void/TYPE (.getReturnType g)))
                (when-let [target (find-method-by-name-and-arity (.getReturnType g) java-name arity)]
                  [proto java params vararg-type
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

(defn wrap-expr [wrappers inner-expr this-sym arg-syms]
  (reduce (fn [expr wrapper]
            (if (symbol? wrapper)
              ;; 符号：展开为 (wrapper (fn [this & args] expr))
              `(~wrapper (fn [~this-sym ~@arg-syms] ~expr))
              ;; 函数字面量或其他表达式：直接嵌入
              (list wrapper `(fn [~this-sym ~@arg-syms] ~expr))))
          inner-expr
          wrappers))


(defn- resolve-wrappers
  "从方法包装器配置中解析出应用于指定方法和参数个数的包装器列表。
   `method-wrappers` 是 :methods 部分的 map，键可以是符号（方法名，匹配所有 arity）或
   向量 [方法名 参数个数]（精确匹配）。"
  [method-wrappers java-method arity]
  (reduce-kv (fn [acc k wrappers]
               (if (and (vector? k)
                        (= (first k) java-method)
                        (= (second k) arity))
                 (vec (concat acc wrappers))         ;; 精确匹配
                 (if (and (symbol? k) (= k java-method))
                   (vec (concat acc wrappers))       ;; 符号匹配所有 arity
                   acc)))
             []
             method-wrappers))

(defn wrap-impl-in-type-def [type-def wrappers-config]
  (let [global-wrappers (get wrappers-config :global [])
        method-wrappers (get wrappers-config :methods {})]
    (update type-def ::spec/method-sigs
            (fn [sigs]
              (mapv (fn [sig]
                      (let [java-method (second sig)
                            arity       (dec (count (nth sig 2)))
                            specific-wrappers (resolve-wrappers method-wrappers java-method arity)
                            all-wrappers (vec (concat global-wrappers specific-wrappers))]
                        (conj (vec sig) all-wrappers)))
                    sigs)))))

(defn- infer-param-count
  "反射获取函数对象的参数最大个数。"
  [f]
  (when (fn? f)
    (let [methods (.getDeclaredMethods (class f))]
      (->> methods
           (filter #(= "invoke" (.getName %)))
           (map #(count (.getParameterTypes %)))
           (reduce max 0)))))

(defn apply-wrapper-args [method-sig]
  (let [wrappers (nth method-sig 5 nil)]
    (println "[apply-wrapper-args] wrappers:" wrappers)
    (if (sequential? wrappers)
      (let [wrapper-fns (map util/->fn wrappers)]
        (println "[apply-wrapper-args] resolved fns:" wrapper-fns)
        (if (every? fn? wrapper-fns)
          (try
            ;; 修复 1: placeholder 必须有函数体
            (let [placeholder (fn [& _] nil)
                  combined-fn (reduce (fn [f w] (w f)) placeholder wrapper-fns)
                  _ (println "[apply-wrapper-args] combined-fn class:" (class combined-fn))
                  _ (println "[apply-wrapper-args] combined-fn meta:" (meta combined-fn))
                  this-sym (first (nth method-sig 2))]
              ;; 1) 优先从组合后函数的元数据获取
              (if-let [combined-meta (:arglists (meta combined-fn))]
                (do
                  (println "[apply-wrapper-args] Using combined-fn meta")
                  (let [returned-args (or (second combined-meta) (first combined-meta))]
                    (if returned-args
                      (let [extra-args (rest returned-args)
                            amp-idx (util/index-of (vec extra-args) '&)]
                        (println "[apply-wrapper-args] extra-args:" extra-args " amp-idx:" amp-idx)
                        (if (pos? amp-idx)
                          (let [fixed (take amp-idx extra-args)
                                vararg (drop (inc amp-idx) extra-args)]
                            (println "[apply-wrapper-args] -> varargs:" fixed '& vararg)
                            (assoc method-sig 2 (vec (concat [this-sym] fixed ['&] vararg))))
                          (let [new-params (vec (cons this-sym extra-args))]
                            (println "[apply-wrapper-args] -> fixed params:" new-params)
                            (assoc method-sig 2 new-params))))
                      method-sig)))
                ;; 2) 无组合元数据 → 尝试最后一个包装器的元数据
                (do
                  (println "[apply-wrapper-args] No combined meta, trying last wrapper meta")
                  (if-let [last-meta (:arglists (meta (last wrapper-fns)))]
                    (do
                      (println "[apply-wrapper-args] Using last wrapper meta")
                      (let [returned-args (or (second last-meta) (first last-meta))]
                        (if returned-args
                          (let [extra-args (rest returned-args)
                                amp-idx (util/index-of (vec extra-args) '&)]
                            (println "[apply-wrapper-args] extra-args:" extra-args " amp-idx:" amp-idx)
                            (if (pos? amp-idx)
                              (let [fixed (take amp-idx extra-args)
                                    vararg (drop (inc amp-idx) extra-args)]
                                (println "[apply-wrapper-args] -> varargs from last wrapper:" fixed '& vararg)
                                (assoc method-sig 2 (vec (concat [this-sym] fixed ['&] vararg))))
                              (let [new-params (vec (cons this-sym extra-args))]
                                (println "[apply-wrapper-args] -> fixed params from last wrapper:" new-params)
                                (assoc method-sig 2 new-params)))))
                        method-sig)))
                  ;; 3) 反射推断
                  (do
                    (println "[apply-wrapper-args] Falling back to reflection")
                    (let [param-count (infer-param-count combined-fn)]
                      (println "[apply-wrapper-args] inferred param-count:" param-count)
                      (if (> param-count 0)
                        (let [extra-syms (repeatedly (dec param-count) #(gensym "arg"))]
                          (println "[apply-wrapper-args] -> reflected params:" (cons this-sym extra-syms))
                          (assoc method-sig 2 (vec (cons this-sym extra-syms))))
                        method-sig))))))
            ;; 修复 2: 捕获异常，安全回退到原始签名
            (catch Exception e
              (println "[apply-wrapper-args] Exception caught:" (.getMessage e))
              method-sig))
          ;; wrapper-fns 中不全是函数，直接返回
          method-sig))
      method-sig)))

(defn expand-method-sigs
  "对协议方法签名列表进行重载展开。
   - sigs            : [[method-name params] ...] （已包含包装器调整后的参数）
   - varargs-max     : 默认最大额外参数个数
   - varargs-per     : {method-name max-n} 按方法覆盖的最大额外参数个数
   返回展开后的扁平列表，可能含有同一方法名的多个签名。"
  [sigs varargs-max varargs-per]
  (mapcat
    (fn [[method params]]
      (if (some '#{&} params)
        ;; 变参：强制展开
        (let [fixed   (take-while #(not= '& %) (rest params))
              this-sym (first params)
              max-n    (get varargs-per method varargs-max)]
          (for [extra (range (inc max-n))]
            (let [extra-syms (repeatedly extra #(gensym "arg"))]
              [method (vec (cons this-sym (concat fixed extra-syms)))])))
        ;; 非变参：保留原签名
        [[method params]]))
    sigs))

(defn emit-extend-type [type-sym proto-sym method-specs arity-lookup]
  (let [grouped (group-method-sigs-by-name method-specs)
        clauses
        (for [[method-name paramlists] grouped
              :let [overload-clauses
                    (for [params paramlists
                          :let [fixed-arity (dec (count params))
                                sig (get arity-lookup [method-name fixed-arity])
                                _ (when-not sig
                                    (throw (ex-info (str "No implementation for " method-name
                                                         " with arity " fixed-arity) {})))
                                [_ java orig-params vararg-type impl wrappers] sig
                                this-sym   (first params)
                                arg-syms   (rest params)
                                orig-arg-syms (rest orig-params)
                                params-vec (vec params)
                                raw-body   (if (some '#{&} orig-arg-syms)
                                             (let [fixed (take-while #(not= '& %) orig-arg-syms)
                                                   vararg (last orig-arg-syms)]
                                               (case (first impl)
                                                 :delegate (if (symbol? (second impl))
                                                             `(. ~this-sym ~(second impl) ~@fixed (into-array ~vararg-type ~vararg))
                                                             (let [{:keys [getter method]} (second impl)]
                                                               `(let [obj# (. ~this-sym ~getter)]
                                                                  (. obj# ~method ~@fixed (into-array ~vararg-type ~vararg)))))
                                                 :custom `(~(second impl) ~this-sym ~@fixed ~vararg)
                                                 `(. ~this-sym ~java ~@fixed (into-array ~vararg-type ~vararg))))
                                             (impl-expr impl this-sym orig-arg-syms))
                                inner-fn   `(fn [~this-sym ~@orig-arg-syms] ~raw-body)
                                combined   (reduce (fn [acc w] `(~w ~acc)) inner-fn wrappers)
                                body       `(~combined ~this-sym ~@arg-syms)]]
                      `(~params-vec ~body))]]
          `(~method-name ~@overload-clauses))]
    `(extend-type ~type-sym ~proto-sym ~@clauses)))

(defn apply-wrappers-to-sigs [sigs]
  (mapv (fn [sig]
          (let [wrappers (nth sig sig-wrappers nil)
                _ (println "Sig wrappers:" wrappers " for method" (first sig))
                updated (apply-wrapper-args sig)
                new-params (nth updated 2)
                _ (println "Adjusted params to:" new-params)]
            new-params))
        sigs))

(defn- build-method-param-specs
  "从原始方法签名和包装器调整后的参数向量构建协议方法签名列表。
   返回 [[协议方法名 调整后参数] ...]"
  [sigs adjusted-params]
  (mapv (fn [sig params]
          [(first sig) params])
        sigs
        adjusted-params))

(defn expand-varargs
  "将标准化的方法-参数列表中的可变参数展开为固定参数重载。
   输入：sigs 是 [[method params] ...] 向量，params 可能含 `&`。
   返回：展开后的 [[method params] ...] 向量，所有 params 均为固定参数。"
  [sigs varargs-max varargs-per]
  (mapcat
    (fn [[method params]]
      (if (some '#{&} params)
        (let [fixed      (take-while #(not= '& %) (rest params))
              this-sym   (first params)
              max-n      (get varargs-per method varargs-max)]
          (for [extra (range (inc max-n))]
            (let [extra-syms (repeatedly extra #(gensym "arg"))]
              [method (vec (cons this-sym (concat fixed extra-syms)))])))
        [[method params]]))
    sigs))

(defn- filter-method-param-specs
  "从标准化方法-参数列表中移除与 clojure.core 冲突的方法名。
   输入：[[method params] ...] 向量
   返回：过滤后的向量。"
  [param-specs]
  (let [core-syms (core-publics)
        conflicts (filter #(contains? core-syms (first %)) param-specs)]
    (when (seq conflicts)
      (binding [*out* *err*]
        (println "警告：以下协议方法名与 clojure.core 冲突，已自动从协议中移除："
                 (str/join ", " (map (comp name first) conflicts)))))
    (remove #(contains? core-syms (first %)) param-specs)))

;; ── 顶层宏：生成协议和实现 ──
(defmacro use-class [java-class & opts]
  (let [opts-map (apply hash-map opts)
        class-sym (util/->sym java-class)
        proto-sym (util/->sym (or (:protocol-name opts-map)
                                  (default-protocol-name class-sym)))
        rename      (util/deep-unquote (:rename opts-map {}))
        rename-inverse (zipmap (vals rename) (keys rename))
        type-def (apply build-type-def class-sym (mapcat identity opts-map))

        ;; 从 type-def 中提取 java名 -> 协议名 的映射
        java->proto (into {} (map (fn [[proto java]] [java proto])
                                  (::spec/method-sigs type-def)))
        ;; 使用该映射转换 delegate/custom/wrappers 中的方法名
        delegate    (util/deep-unquote (:delegate opts-map []))
        ;; 原始 custom 配置：[[method arity? impl-fn] ...] 或 [[method impl-fn] ...]
        custom (util/deep-unquote (:custom opts-map []))
        wrappers    (util/deep-unquote (:wrappers opts-map {}))
        delegate-classes (util/deep-unquote (:delegate-classes opts-map nil))
        ;; 将配置中的原始名转换为协议名（如果映射中没有则保持原样）
        resolve-name (fn [orig-name] (get java->proto orig-name orig-name))

        normalized-delegate (normalize-delegate-config class-sym delegate)
        delegate-config (mapv (fn [[target-method getter]]
                                [(resolve-name target-method) getter target-method])
                              normalized-delegate)

        policies (merge-impl-policies
                   (custom-impl-policy custom resolve-name)
                   (delegate-impl-policy delegate-config)
                   (direct-impl-policy :rename-inverse rename-inverse)
                   (smart-delegate-policy :rename-inverse rename-inverse :delegate-classes delegate-classes))
        type-def (inject-impl-in-type-def type-def policies)
        type-def (wrap-impl-in-type-def type-def wrappers)
        ;; ★ 计算包装器参数（只取修改后的参数向量，不改变 type-def(不要引入不纯的操作)）
        proto-params-list (apply-wrappers-to-sigs (::spec/method-sigs type-def))
        ;proto-def (type-def->protocol-def type-def proto-sym)

        ;; 可变参数配置
        varargs-max  (or (:varargs-max opts-map) 10)
        varargs-per-raw  (util/deep-unquote (:varargs opts-map {}))
        varargs-per (reduce-kv (fn [m k v]                  ;; 可变参数展开时候使用转换后的名称
                                 (let [proto-name (get java->proto k k)]
                                   (assoc m proto-name v)))
                               {}
                               varargs-per-raw)
        ;; 构建标准化列表（可能含 &）
        method-param-specs (build-method-param-specs
                             (::spec/method-sigs type-def)
                             proto-params-list)

        ;; 展开变参
        expanded-specs (expand-varargs method-param-specs varargs-max varargs-per)

        ;; 过滤冲突
        filtered-specs (filter-method-param-specs expanded-specs)

        ;; [java-method, arity]->impl
        ;; 构建 [协议方法名, 固定参数个数] → 原始签名 的查找表
        ;; 对于变参方法，将其从 base-arity 到 base-arity+max-n 的所有 arity 都映射到同一条原始签名
        ;; 基于包装器调整后的参数构建 arity-lookup
        arity-lookup
        (reduce (fn [m [sig adjusted-params]]
                  (let [proto-name (first sig)
                        base-arity (java-arity adjusted-params)      ;; 调整后的固定参数个数
                        is-varargs? (some '#{&} adjusted-params)]
                    (if is-varargs?
                      (let [max-extra (get varargs-per proto-name varargs-max)]
                        (reduce (fn [m2 extra]
                                  (assoc m2 [proto-name (+ base-arity extra)] sig))
                                m
                                (range (inc max-extra))))
                      (assoc m [proto-name base-arity] sig))))
                {}
                (map vector (::spec/method-sigs type-def) proto-params-list))
        ]
    `(do
       ~(emit-defprotocol proto-sym filtered-specs)
       ~(emit-extend-type class-sym proto-sym filtered-specs arity-lookup))))