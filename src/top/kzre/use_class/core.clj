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
(def ^:const sig-return    3)  ; 返回类型
(def ^:const sig-impl      4)  ; 实现策略
(def ^:const sig-wrappers  5)  ; 包装器列表


;; ================ Resolve Java Type ==================================
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
  (let [java-method       (symbol (.getName m))
        arity       (.getParameterCount m)
        param-vec      (vec (cons 'this (repeatedly arity #(gensym "arg"))))
        return-sym      (symbol (.getName (.getReturnType m)))]
    [java-method                                            ;; 暂且用 java名称占位
     java-method param-vec return-sym]))

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

(defn normalize-filter-entries
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
                         (let [arity (dec (count params))]
                           (if default-include
                             (not (matches? exclude-norm java arity))
                             (matches? include-norm java arity))))
                       sigs)))))

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

(defn- dedupe-method-sigs
  "给定方法名和它的签名列表，根据 `explicit-arities` 决定保留哪些签名。
   - 若方法名在 `explicit-arities` 中，保留所有参数个数匹配的签名（支持重载）。
   - 否则，只保留参数个数最大的一个签名。"
  [proto sigs explicit-arities]
  (if-let [arities (get explicit-arities proto)]
    (filter (fn [[_ _ params]]
              (contains? arities (dec (count params))))
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
              (let [[_ target-java target-params target-return] target-sig
                    new-params (cons 'this (rest target-params))]
                ;; proto-name 暂用 target-method（原始 Java 名），后续由 rename 处理
                [[target-method target-java new-params target-return]])))
          delegate-config)]
    (merge-type-def host-type-def
                    {::spec/type-name ::merge-placeholder
                     ::spec/method-sigs (vec extra-sigs)})))

(defn- ensure-explicit-arities
  "根据用户通过 `:only` 明确指定的参数个数（arity），向 type-def 中添加缺失的方法签名。"
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
                            (let [existing (get by-name method [])
                                  existing-arities (set (map (comp dec count #(nth % 2)) existing))]
                              (concat existing
                                      (for [a arities
                                            :when (not (contains? existing-arities a))]
                                        (let [template (first existing)
                                              java-name (second template)
                                              params (vec (cons 'this (repeatedly a #(gensym "arg"))))]
                                          [method java-name params (nth template 3 nil)])))))
                          arities-map))))
      type-def)))

(defn build-type-def
  [java-class-sym & {:keys [only except
                        rename rename-fn dangerous setter-danger? prefix
                        delegate custom ]
                 :or {rename {} dangerous #{} setter-danger? true delegate [] custom [] prefix nil}}]
  (let [only  (util/deep-unquote only)
        except (util/deep-unquote except)
        only-entries   (normalize-filter-entries (or only []))
        except-entries (normalize-filter-entries (or except []))
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
        default-include (nil? only)
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
        type-def (mark-dangerous-in-type-def type-def dangerous :setter-danger? setter-danger?)
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
  (reduce (fn [m [name params]]
            (update m name (fnil conj []) params))
          {}
          flat-sigs))

(defn emit-defprotocol [protocol-def]
  (let [proto-name (::spec/protocol-name protocol-def)
        sigs (::spec/protocol-method-sigs protocol-def)
        grouped (group-method-sigs-by-name sigs)]        ;; {method [params...]}
    `(defprotocol ~proto-name
       ~@(mapcat (fn [[method paramlists]]
                   `((~method ~@paramlists)))             ;; (method [p1] [p2] ...)
                 grouped))))

(defn default-proto-sym
  [class-sym]
  (let [simple (-> (str class-sym)
                   (str/replace #"^.*\." "")
                   (str/replace #"^I" ""))]
    (symbol (str "I" simple))))

;; ── 顶层宏：生成协议 ──
(defmacro defprotocol-from-type [java-class & opts]
  (let [opts-map (apply hash-map opts)
        java-class-sym (util/->sym java-class)
        proto-name (util/->sym (or (:protocol-name opts-map) (default-proto-sym java-class-sym)))
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
   - explicit-arities: #{method-name ...} 用户显式指定了 arity 的方法，跳过展开
   - varargs-max     : 默认最大额外参数个数
   - varargs-per     : {method-name max-n} 按方法覆盖的最大额外参数个数
   返回展开后的扁平列表，可能含有同一方法名的多个签名。"
  [sigs explicit-arities varargs-max varargs-per]
  (mapcat
    (fn [[method params]]
      (if (or (contains? explicit-arities method)
              (not (some '#{&} params)))
        ;; 用户已指定 arity 或非变参：保持原样
        [[method params]]
        ;; 变参方法且非显式：展开为 0..max-n 个固定重载
        (let [fixed      (take-while #(not= '& %) (rest params))
              this-sym   (first params)
              max-n      (get varargs-per method varargs-max)]
          (for [extra (range (inc max-n))]
            (let [extra-syms (repeatedly extra #(gensym "arg"))]
              [method (vec (cons this-sym (concat fixed extra-syms)))])))))
    sigs))

(defn emit-extend-type [type-def proto-sym proto-params-list]
  (let [type-sym (::spec/type-name type-def)
        sigs     (::spec/method-sigs type-def)          ;; 原始签名（每个方法一条）
        ;; 按协议方法名分组，每个方法对应多个参数向量（来自 proto-params-list）
        grouped  (reduce (fn [m [sig params]]
                           (let [method-name (first sig)]
                             (update m method-name (fnil conj []) [sig params])))
                         {}
                         (map vector sigs proto-params-list))
        clauses
        (for [[method-name overloads] grouped
              :let [overload-clauses
                    (for [[sig adjusted-params] overloads
                          :let [[_ java orig-params _ impl wrappers] sig
                                this-sym   (first adjusted-params)
                                arg-syms   (rest adjusted-params)
                                orig-arg-syms (rest orig-params)
                                params-vec (vec adjusted-params)
                                raw-body   (impl-expr impl this-sym orig-arg-syms)
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

(defn- build-protocol-method-sigs
  "从原始方法签名和包装器调整后的参数向量构建协议方法签名列表。
   返回 [(协议方法名 调整后参数) ...]"
  [sigs adjusted-params]
  (mapv (fn [sig params]
          [(first sig) params])
        sigs
        adjusted-params))

;; ── 顶层宏：生成协议和实现 ──
(defmacro use-class [java-class & opts]
  (let [opts-map (apply hash-map opts)
        java-class-sym (util/->sym java-class)
        proto-sym (util/->sym (or (:protocol-name opts-map)
                                  (default-proto-sym java-class-sym)))
        type-def (apply build-type-def java-class-sym (mapcat identity opts-map))

        rename      (util/deep-unquote (:rename opts-map {}))
        delegate    (util/deep-unquote (:delegate opts-map []))
        custom      (util/deep-unquote (:custom opts-map []))
        wrappers    (util/deep-unquote (:wrappers opts-map {}))
        delegate-classes (util/deep-unquote (:delegate-classes opts-map nil))
        rename-fn   (or (:rename-fn opts-map) util/bean-name->kebab-name)
        resolve-name (fn [orig-name]
                       (let [renamed (or (get rename orig-name) orig-name)]
                         (if rename-fn (rename-fn renamed) renamed)))
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
        type-def (wrap-impl-in-type-def type-def wrappers)
        ;; ★ 计算包装器参数（只取修改后的参数向量，不改变 type-def）
        proto-params-list (apply-wrappers-to-sigs (::spec/method-sigs type-def))
        proto-def (type-def->protocol-def type-def proto-sym)
        ;; ★ 替换协议方法签名
        proto-def (assoc proto-def ::spec/protocol-method-sigs
                                   (build-protocol-method-sigs
                                     (::spec/method-sigs type-def)
                                     proto-params-list))]
    `(do
       ~(emit-defprotocol proto-def)
       ~(emit-extend-type type-def proto-sym proto-params-list))))