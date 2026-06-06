(ns top.kzre.use-class.core
  (:require [top.kzre.use-class.spec :as spec]
            [clojure.string :as str])
  (:import [java.lang.reflect Method]))

;; ── 常量 ──
(defonce ^:private object-method-names
         (->> (.getMethods Object)
              (map #(.getName ^Method %))
              set))
(defn ->sym
  "将字符串、关键字、符号或引号形式 (quote sym) 转换为符号。"
  [x]
  (cond
    (symbol? x) x
    (keyword? x) (symbol (name x))
    (string? x) (symbol x)
    (and (seq? x) (= (first x) 'quote) (= (count x) 2))
    (->sym (second x))              ; 处理 'sym 形式
    :else (throw (IllegalArgumentException. (str "无法转换为符号: " x)))))

(defn- kebab->camel-safe
  "安全地将 kebab-case 方法名转换为 camelCase。仅处理标准 bean 前缀 get/set/is。
   例如：get-time → getTime，set-time → setTime，is-ready → isReady。
   无法转换时返回 nil。"
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

;; ── 工具函数 ──
(defn bean-name->kebab-name
  "将 Java 方法名符号转为 Clojure kebab-case 符号，仅风格转换。
   例如：getTime → get-time，setTime → set-time，isReady → is-ready。"
  [method-sym]
  (let [name   (name method-sym)
        [prefix body]
        (cond
          (re-find #"^get[A-Z]" name) [nil (subs name 3)]
          (re-find #"^set[A-Z]" name) ["set" (subs name 3)]
          (re-find #"^is[A-Z]" name)  [nil (str (subs name 2))]
          :else                        [nil name])
        hyphenated (-> body
                       (str/replace #"([A-Z])" #(str "-" (.toLowerCase (second %))))
                       (str/replace #"^-" ""))
        full (str (when prefix (str prefix "-")) hyphenated)]
    (symbol full)))



;; ── 步骤 1：解析 Java 类型 ──
(defn- extract-methods [cls]
  (.getMethods cls))

(defn- filter-object-methods [methods]
  (remove (comp object-method-names #(.getName ^Method %))
          methods))

(defn- method->sig [^Method m]
  (let [proto-name  (symbol (.getName m))              ; 保留原始 Java 名称
        param-types (.getParameterTypes m)
        params      (->> param-types
                         (map #(symbol (.getName ^Class %)))
                         (cons 'this)
                         vec)
        return      (symbol (.getName (.getReturnType m)))]
    [proto-name params return]))

(defn resolve-type
  "解析 Java 类/接口，返回符合 ::spec/type-def 的中间数据。"
  [type-sym]
  (let [cls (resolve type-sym)]
    (when-not cls
      (throw (IllegalArgumentException.
               (str "无法解析的 Java 类型: " type-sym))))
    {::spec/type-name type-sym
     ::spec/method-sigs (->> cls
                             extract-methods
                             filter-object-methods
                             (mapv method->sig))}))

(defn filter-methods-in-type-def
  "根据 default-include 策略过滤 type-def 中的方法签名。
   - default-include: true → 默认全部保留，排除 :exclude 中的方法
                     false → 默认全部排除，只保留 :include 中的方法
   - include: 要保留的方法名集合（default-include = false 时有效）
   - exclude: 要排除的方法名集合（default-include = true 时有效）"
  [type-def default-include include exclude]
  (let [include-set (set include)
        exclude-set (set exclude)
        pred (if default-include
               #(not (contains? exclude-set %))
               #(contains? include-set %))]
    (update type-def ::spec/method-sigs
            (fn [sigs]
              (filterv (fn [[method-sym & _]]
                         (pred method-sym))
                       sigs)))))


(defn rename-methods-in-type-def
  "对 type-def 中的方法名进行重命名。
   - rename-map: {原始方法名符号 -> 新方法名符号}
   - rename-fn:   函数，接受原始方法名符号，返回新方法名符号。
                  当 rename-map 中不存在时使用，可为 nil，则保持原名。"
  [type-def rename-map rename-fn]
  (let [newname (fn [orig-name]
                  (or (get rename-map orig-name)
                      (when rename-fn (rename-fn orig-name))
                      orig-name))]
    (update type-def ::spec/method-sigs
            (fn [sigs]
              (mapv (fn [[method-name params return]]
                      [(newname method-name) params return])
                    sigs)))))



(defn- setter-name?
  "判断符号是否代表一个 setter 风格的方法名（以 set- 开头且后面有内容）"
  [sym]
  (let [s (name sym)]
    (and (str/starts-with? s "set-")
         (> (count s) 4))))

(defn mark-dangerous-in-type-def
  "对 type-def 中的方法名进行危险标记（加 ! 后缀）。
   - danger-set: 显式危险方法名的符号集合。
   - :setter-danger? (默认 true) 是否自动将 setter 方法（名如 set-*）也标记为危险。
   注意：方法名已有 ! 结尾时不重复添加。"
  [type-def danger-set & {:keys [setter-danger?] :or {setter-danger? true}}]
  (update type-def ::spec/method-sigs
          (fn [sigs]
            (mapv (fn [[method-name :as sig]]
                    (if (and (not (str/ends-with? (name method-name) "!"))
                             (or (contains? danger-set method-name)
                                 (and setter-danger? (setter-name? method-name))))
                      [(symbol (str (name method-name) "!")) (nth sig 1) (nth sig 2)]
                      sig))
                  sigs))))


(defn merge-type-def
  "合并多个 type-def，保留第一个的类型名，合并方法签名。
   若方法名重复，保留首次出现的签名，忽略后续重复项。"
  [base-type-def & more-type-defs]
  (let [all-sigs (reduce (fn [sigs td]
                           (reduce (fn [acc sig]
                                     (let [name (first sig)]
                                       (if (some #(= (first %) name) acc)
                                         acc
                                         (conj acc sig))))
                                   sigs
                                   (::spec/method-sigs td)))
                         (::spec/method-sigs base-type-def)
                         more-type-defs)]
    (assoc base-type-def ::spec/method-sigs (vec all-sigs))))

(defn extract-delegate-type-def
  "根据宿主类和委托条目，提取委托目标类型的 type-def。
   host-class-sym: 宿主类符号（如 'java.util.Calendar）
   delegate-entry: 委托条目，形如 [proto-fn getter] 或 [proto-fn getter target-method]"
  [host-class-sym delegate-entry]
  (let [cls (resolve host-class-sym)
        _ (when-not cls (throw (IllegalArgumentException. (str "无法解析类型: " host-class-sym))))
        getter-name (name (second delegate-entry))
        getter-method (first (filter #(= (.getName ^Method %) getter-name) (.getMethods cls)))
        _ (when-not getter-method
            (throw (ex-info (str "在 " host-class-sym " 中找不到方法 " getter-name) {})))
        ret-class (.getReturnType ^Method getter-method)
        ret-class-sym (symbol (.getName ret-class))]
    (resolve-type ret-class-sym)))

(defn resolve-conflict-methods
  "从 sigs 中选出名为 method-name 的所有方法，
   按参数个数（去掉 this）降序排列，返回参数个数最多的签名。
   若个数最多的不唯一，返回第一个。"
  [sigs method-name]
  (let [candidates (filter #(= (first %) method-name) sigs)
        _ (when (empty? candidates)
            (throw (ex-info (str "未找到方法 " method-name) {})))
        ;; 按参数个数分组
        groups (group-by (fn [[_ params _]] (dec (count params))) candidates)
        max-arity (apply max (keys groups))
        top-group (get groups max-arity)]
    (first top-group)))  ;; 直接取第一个，忽略类型歧义


(defn merge-type-def-with-delegates
  "根据宿主 type-def 和 delegate-config 合并委托目标类型的方法签名。
   返回新的 type-def，包含原始方法和委托方法，检测名称冲突。"
  [host-type-def delegate-config]
  (let [host-class-sym (::spec/type-name host-type-def)
        extra-sigs
        (mapcat
          (fn [entry]
            (let [proto-fn      (first entry)
                  getter        (second entry)
                  target-method (if (= (count entry) 3) (nth entry 2) proto-fn)
                  host-cls      (resolve host-class-sym)
                  getter-method (first (filter #(= (.getName ^Method %) (name getter))
                                               (.getMethods host-cls)))
                  _             (when-not getter-method
                                  (throw (ex-info (str "Getter " getter " not found in " host-class-sym) {})))
                  ret-class     (.getReturnType ^Method getter-method)
                  ret-class-sym (symbol (.getName ret-class))
                  target-td     (resolve-type ret-class-sym)
                  target-sig    (resolve-conflict-methods (::spec/method-sigs target-td) target-method)
                  _             (when-not target-sig
                                  (throw (ex-info (str "Target method " target-method
                                                       " not found in " ret-class-sym)
                                                  {})))]
              ;; 构造新的方法签名：重命名为 proto-fn，参数列表调整为 [this & rest-args]
              (let [[_ original-params return] target-sig
                    new-params (cons 'this (rest original-params))]
                [[proto-fn new-params return]])))
          delegate-config)]
    (merge-type-def host-type-def
                    {::spec/type-name ::merge-placeholder   ; 类型名会被忽略，因为我们用 merge-type-def 的 base
                     ::spec/method-sigs (vec extra-sigs)})))

(defn gen-delegate-entries
  "根据宿主类和 getter 名称，自动生成该 getter 返回类型上所有方法的委托条目。
   每个条目为 [proto-fn getter]，其中 proto-fn 为目标方法名（Java 原名）。
   返回 delegate-config 向量，可直接用于 merge-type-def-with-delegates。"
  [host-class-sym getter-sym]
  (let [host-cls      (resolve host-class-sym)
        _             (when-not host-cls (throw (IllegalArgumentException. (str "无法解析类型: " host-class-sym))))
        getter-method (first (filter #(= (.getName ^Method %) (name getter-sym))
                                     (.getMethods host-cls)))
        _             (when-not getter-method
                        (throw (ex-info (str "Getter " getter-sym " not found in " host-class-sym) {})))
        ret-class     (.getReturnType ^Method getter-method)
        ret-class-sym (symbol (.getName ret-class))
        target-td     (resolve-type ret-class-sym)
        method-names  (map first (::spec/method-sigs target-td))]
    (mapv (fn [m] [m getter-sym]) method-names)))


(defn normalize-delegate-config
  [host-class-sym raw-config]
  (mapcat
    (fn [entry]
      ;; 整体去引号：将 '(getTime) 这种形式转为符号 getTime
      (let [entry (if (and (seq? entry) (= (first entry) 'quote) (= (count entry) 2))
                    (->sym entry)     ; 变成符号
                    entry)
            ;; 如果 entry 是单独符号，包装成向量以便统一处理
            entry (if (symbol? entry) [entry] entry)
            entry (mapv ->sym entry)  ; 再对所有元素标准化
            cnt (count entry)]
        (case cnt
          1 (gen-delegate-entries host-class-sym (first entry))
          2 (if (symbol? (first entry))
              [entry]
              (let [[getter mappings] entry]
                (mapv (fn [[proto-fn method]]
                        [proto-fn getter method])
                      mappings)))
          3 [entry]
          (throw (ex-info (str "无效的委托条目: " entry) {})))))
    raw-config))

(defn dedupe-methods
  "对 type-def 中的方法签名去重：同名方法保留参数个数最多的，若相同则保留第一个。"
  [type-def]
  (update type-def ::spec/method-sigs
          (fn [sigs]
            (vals (reduce (fn [m sig]
                            (let [name (first sig)
                                  arity (dec (count (second sig)))]
                              (if-let [existing (get m name)]
                                (let [existing-arity (dec (count (second existing)))]
                                  (if (> arity existing-arity)
                                    (assoc m name sig)
                                    m))
                                (assoc m name sig))))
                          {}
                          sigs)))))

(defn build-type-def
  "综合所有配置选项，生成最终的 type-def。"
  [java-class & {:keys [rename rename-fn only except dangerous setter-danger? delegate]
                 :or {rename {} dangerous #{} setter-danger? true delegate []}}]
  (let [type-def (resolve-type java-class)
        ;; 标准化 rename 映射
        rename (into {} (map (fn [[k v]] [(->sym k) (->sym v)]) rename))
        ;; only / except 保留 nil，仅在有值时标准化为符号向量
        only (when only (mapv ->sym only))
        except (when except (mapv ->sym except))
        dangerous (set (map ->sym dangerous))
        delegate (mapv (fn [entry] (mapv ->sym entry)) delegate)
        ;; 委托处理
        delegate-entries (normalize-delegate-config java-class delegate)
        type-def (if (seq delegate-entries)
                   (merge-type-def-with-delegates type-def delegate-entries)
                   type-def)
        ;; 重命名
        type-def (rename-methods-in-type-def type-def rename rename-fn)
        ;; 去重：消除桥接方法造成的同名方法
        type-def (dedupe-methods type-def)
        ;; 过滤：若用户提供了 :only，则默认排除其余方法
        default-include (nil? only)
        type-def (filter-methods-in-type-def type-def default-include only except)
        ;; 危险标记
        type-def (mark-dangerous-in-type-def type-def dangerous :setter-danger? setter-danger?)]
    type-def))


(defn- core-publics []
  (set (keys (ns-publics 'clojure.core))))

(defn filter-core-conflicts
  "从方法签名中移除与 clojure.core 冲突的方法，并打印警告。"
  [sigs]
  (let [core-syms (core-publics)
        conflicts (filter #(contains? core-syms (first %)) sigs)]
    (when (seq conflicts)
      (binding [*out* *err*]
        (println "警告：以下协议方法名与 clojure.core 冲突，已自动从协议中移除："
                 (str/join ", " (map (comp name first) conflicts)))))
    (remove #(contains? core-syms (first %)) sigs)))

(defn type-def->protocol-def
  "将 type-def 转换为 protocol-def，自动过滤核心冲突方法。"
  [type-def proto-name]
  (let [sigs (filter-core-conflicts (::spec/method-sigs type-def))]
    {::spec/protocol-name (->sym proto-name)
     ::spec/protocol-method-sigs
     (mapv (fn [[method-name params _]]
             (let [arity (dec (count params))]
               [(->sym method-name) (into ['this] (repeatedly arity #(gensym "arg")))]))
           sigs)}))


(defn emit-defprotocol
  "从 protocol-def 生成 defprotocol 表单。"
  [protocol-def]
  (let [proto-name (::spec/protocol-name protocol-def)
        sigs (::spec/protocol-method-sigs protocol-def)]
    `(defprotocol ~proto-name
       ~@(for [[name params] sigs]
           `(~name ~params)))))

(defn derive-protocol-name
  "从 Java 类名推导协议名：简单类名前加 I，避免 II 前缀。
   例如：java.util.Date → IDate"
  [class-sym]
  (let [simple (-> (str class-sym)
                   (str/replace #"^.*\." "")
                   (str/replace #"^I" ""))]
    (symbol (str "I" simple))))

(defmacro defprotocol-from-type [java-class & opts]
  (let [java-class-sym (->sym java-class)
        opts-map (apply hash-map opts)
        proto-name (->sym (or (:protocol-name opts-map) (derive-protocol-name java-class-sym)))
        type-def (apply build-type-def java-class-sym (mapcat identity opts-map))
        proto-def (type-def->protocol-def type-def proto-name)]
    (emit-defprotocol proto-def)))





;; use-class=========================================

;; 工具：根据类、方法名、参数个数查找 Method
(defn- find-method-by-name-and-arity [cls method-name arity]
  (some #(when (and (= (.getName ^Method %) (name method-name))
                    (= (.getParameterCount ^Method %) arity))
           %)
        (.getMethods cls)))

;; direct-impl-policy
(defn direct-impl-policy [& {:keys [rename-inverse]}]
  (fn [[method-name params return] cls]
    (let [protocol-name (if (str/ends-with? (name method-name) "!")
                          (symbol (subs (name method-name) 0 (dec (count (name method-name)))))
                          method-name)
          java-name (or (kebab->camel-safe protocol-name)
                        (get rename-inverse protocol-name)
                        protocol-name)
          arity (dec (count params))]
      (when (find-method-by-name-and-arity cls java-name arity)
        [method-name params return {:delegate java-name}]))))

;; delegate-impl-policy
(defn delegate-impl-policy [delegate-config]
  (let [mapping (into {} (map (fn [entry]
                                (let [proto-fn (first entry)
                                      getter (second entry)
                                      target (if (= (count entry) 3) (nth entry 2) proto-fn)]
                                  [proto-fn {:delegate {:getter getter :method target}}])))
                      delegate-config)]
    (fn [[method-name params return] cls]
      (when-let [impl (get mapping method-name)]
        [method-name params return impl]))))

;; custom-impl-policy
(defn custom-impl-policy [custom-config]
  (let [mapping (into {} (map (fn [[k _ f]] [k {:custom f}]) custom-config))]
    (fn [[method-name params return] cls]
      (when-let [impl (get mapping method-name)]
        [method-name params return impl]))))

;; 合并策略：按顺序尝试，返回第一个非 nil
(defn merge-impl-policies [& policies]
  (fn [sig cls]
    (some #(% sig cls) policies)))


(defn impl-expr
  "将实现映射转换为 Clojure 互操作表达式。
   impl-map 格式: {:delegate java-method} 或 {:delegate {:getter g :method m}} 或 {:custom f}"
  [impl-map this-sym arg-syms]
  (let [[type val] (first impl-map)]
    (case type
      :delegate (if (symbol? val)
                  ;; 直接委托：(. this method args)
                  `(. ~this-sym ~val ~@arg-syms)
                  ;; 间接委托：先获取委托对象，再调用
                  (let [getter (:getter val)
                        method (:method val)]
                    `(let [obj# (. ~this-sym ~getter)]
                       (. obj# ~method ~@arg-syms))))
      :custom `(~val ~this-sym ~@arg-syms))))


(defn wrap-expr
  "将包装器列表应用到表达式上。包装器是高阶函数，接收一个函数，返回新函数。"
  [wrappers inner-expr this-sym arg-syms]
  (reduce (fn [expr wrapper-sym]
            `(~wrapper-sym (fn [~this-sym ~@arg-syms] ~expr)))
          inner-expr
          wrappers))


(defn emit-extend-type
  "从 type-def 生成 extend-type 表单。需要与 defprotocol 参数签名一致。"
  [type-def protocol-def]
  (let [type-sym (::spec/type-name type-def)
        proto-sym (::spec/protocol-name protocol-def)
        sigs (::spec/method-sigs type-def)
        proto-method-map (into {} (::spec/protocol-method-sigs protocol-def))
        clauses (for [[name _ _ impl wrappers] sigs
                      :let [proto-params (get proto-method-map name)
                            this-sym (first proto-params)
                            arg-syms (rest proto-params)
                            params-vec (vec proto-params)
                            body (wrap-expr wrappers
                                            (impl-expr impl this-sym arg-syms)
                                            this-sym arg-syms)]]
                  `(~name ~params-vec ~body))]
    `(extend-type ~type-sym ~proto-sym ~@clauses)))

