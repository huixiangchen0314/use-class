(ns top.kzre.use-class.core
  "数据驱动的 Java 实现复用引擎。"
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [top.kzre.use-class.spec :as spec])
  (:import (java.lang.reflect Method)
           (java.util Optional)))

;; ============================================================
;; 内部工具：Java 反射分析
;; ============================================================
(defn- bean-method->fn-symbol
  "默认命名规则：getXxx -> xxx, setXxx -> set-xxx!, isXxx -> xxx?, 其余驼峰转连字符。"
  [^Method m]
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

(defn- method-params
  "返回方法参数名向量 (arg0 arg1 ...)，长度等于参数个数（不含 this）。"
  [^Method m]
  (let [cnt (.getParameterCount m)]
    (mapv (fn [i] (symbol (str "arg" i))) (range cnt))))

(defn- analyze-java-class*
  "反射分析 Java 类/接口，返回方法信息 map 序列。"
  [class-sym]
  (let [cls (resolve class-sym)
        _ (when-not cls (throw (Exception. (str "Class not found: " class-sym))))
        methods (.getMethods cls)]
    (for [^Method m methods
          :when (not (contains? #{:equals :hashCode :toString :getClass :notify :notifyAll :wait}
                                (keyword (.getName m))))]
      {:java-name   (.getName m)
       :clj-name    (bean-method->fn-symbol m)
       :return-type (.getReturnType m)
       :param-types (vec (.getParameterTypes m))
       :param-count (.getParameterCount m)})))

;; ============================================================
;; 方法条目数据结构
;; ============================================================
(defn- method-info->entry
  [{:keys [clj-name java-name param-count return-type param-types]}]
  {:sym clj-name
   :impl (symbol (str "." java-name))
   :arity (inc param-count)
   :return-type return-type
   :param-types param-types
   :java-name java-name})

;; ============================================================
;; 高阶函数：分析 Java 类
;; ============================================================
(defn analyze-class
  "分析 Java 类/接口，返回自动映射的方法条目向量。"
  [class-sym]
  (mapv method-info->entry (analyze-java-class* class-sym)))

;; ============================================================
;; 高阶函数：穿透委托 (via)
;; ============================================================
(defn- protocol-method->java-getter
  "从协议方法名推导 Java getter 方法名（驼峰式 + get 前缀）。"
  [sym]
  (let [parts (str/split (name sym) #"-")
        capitalized (map str/capitalize parts)
        camel (apply str capitalized)]
    (symbol (str "get" camel))))

;; via-methods 现在需要 host-class 参数
(defn via-methods
  "host-class: Java 类符号，getter: 该类上的无参方法符号，entries: 同前。
   示例: (via-methods 'com.example.Foo :getHelper [[:helper-method]])"
  [host-class getter entries]
  (let [host-cls (resolve host-class)
        _ (when-not host-cls
            (throw (Exception. (str "Host class not found: " host-class))))
        getter-method (some #(when (= (.getName %) (name getter)) %)
                            (.getMethods host-cls))
        _ (when-not getter-method
            (throw (Exception. (str "Getter method " getter " not found in " host-class))))
        target-class (.getReturnType getter-method)
        _ (when-not target-class
            (throw (Exception. (str "Cannot determine return type of getter " getter " in " host-class))))]
    (mapv (fn [entry]
            (let [[proto-method target-method] (if (= (count entry) 1)
                                                 [(first entry) (protocol-method->java-getter (first entry))]
                                                 entry)
                  method-obj (some #(when (= (.getName %) (name target-method)) %)
                                   (.getMethods target-class))
                  _ (when-not method-obj
                      (throw (Exception. (str "Method " target-method " not found in " (.getName target-class)))))
                  arity (inc (.getParameterCount method-obj))
                  return-type (.getReturnType method-obj)
                  param-types (vec (.getParameterTypes method-obj))
                  param-names (method-params method-obj)]
              {:sym proto-method
               :impl `(fn [~'this ~@param-names]
                        (let [obj# (~(symbol (str "." (name getter))) ~'this)]
                          (~(symbol (str "." (name target-method))) obj# ~@param-names)))
               :arity arity
               :return-type return-type
               :param-types param-types}))
          entries)))

;; ============================================================
;; 高阶函数：自定义方法
;; ============================================================
(defn custom-entry [sym arity f]
  {:sym sym :impl f :arity arity :return-type nil :param-types []})

(defn custom-methods [specs]
  (mapv (fn [[sym arity f]] (custom-entry sym arity f)) specs))

;; ============================================================
;; 高阶函数：组合与过滤
;; ============================================================
(defn merge-methods [& colls]
  (let [all (apply concat colls)]
    (vals (reduce (fn [m e] (assoc m (:sym e) e)) {} all))))

(defn filter-methods [methods & {:keys [except only]}]
  (cond
    only   (filter #(contains? (set only) (:sym %)) methods)
    except (remove #(contains? (set except) (:sym %)) methods)
    :else methods))

(defn rename-methods [methods mapping]
  (mapv (fn [m] (if-let [new-sym (get mapping (:sym m))]
                  (assoc m :sym new-sym)
                  m))
        methods))

;; ============================================================
;; 高阶函数：后处理
;; ============================================================
(defn mark-dangerous [methods dangerous-set]
  (let [dset (set (map name dangerous-set))]
    (mapv (fn [m]
            (let [sname (name (:sym m))]
              (if (and (contains? dset sname)
                       (not (str/ends-with? sname "!")))
                (assoc m :sym (symbol (str sname "!")))
                m)))
          methods)))

(defn apply-name-mapper [methods mapper-fn]
  (mapv (fn [m] (assoc m :sym (mapper-fn (:sym m)))) methods))

;; ============================================================
;; 返回值自动转换
;; ============================================================
(defonce ^:private result-converters (atom {}))

(defn register-result-converter! [class converter]
  (let [clz (if (class? class) class (resolve class))]
    (when clz (swap! result-converters assoc clz converter))))

(defn- builtin-result-converters []
  (register-result-converter! Optional (fn [^Optional opt] (when (.isPresent opt) (.get opt)))))

(builtin-result-converters)

(defn auto-wrap-results [methods]
  (mapv (fn [{:keys [return-type impl arity] :as m}]
          (if-let [converter (and return-type (get @result-converters return-type))]
            (let [param-names (map (fn [i] (symbol (str "arg" i))) (range (dec arity)))]
              (assoc m :impl
                       `(fn [~'this ~@param-names]
                          (let [result# (~impl ~'this ~@param-names)]
                            (~converter result#)))))
            m))
        methods))

(defn wrap-results [methods mapping]
  (mapv (fn [{:keys [sym impl arity] :as m}]
          (if-let [converter (get mapping sym)]
            (let [param-names (map (fn [i] (symbol (str "arg" i))) (range (dec arity)))]
              (assoc m :impl
                       `(fn [~'this ~@param-names]
                          (let [result# (~impl ~'this ~@param-names)]
                            (~converter result#)))))
            m))
        methods))

(defn wrap-args [methods specs]
  (mapv (fn [{:keys [sym impl arity] :as m}]
          (if-let [arg-specs (get specs sym)]
            (let [param-names (map (fn [i] (symbol (str "arg" i))) (range (dec arity)))
                  converted-args (map-indexed (fn [i pname]
                                                (if-let [[_ converter] (some #(when (= (first %) i) %) arg-specs)]
                                                  `(~converter ~pname)
                                                  pname))
                                              param-names)]
              (assoc m :impl
                       `(fn [~'this ~@param-names]
                          (~impl ~'this ~@converted-args))))
            m))
        methods))

;; ============================================================
;; 终结宏
;; ============================================================
(defmacro extend-by-methods
  "接受字面量方法条目向量，生成 extend-type 形式。
   用法：
   (extend-by-methods String MyProto
     [{:sym 'hello, :impl (fn [this] \"world\"), :arity 1}])"
  [java-class protocol-name methods-vec]
  ;; 直接使用字面量向量，不需要 eval
  `(extend-type ~java-class
     ~protocol-name
     ~@(mapcat (fn [{:keys [sym impl arity]}]
                 (let [params (repeatedly (dec arity) #(gensym "arg"))]
                   `((~sym [~'this ~@params] (~impl ~'this ~@params)))))
               methods-vec)))


(defn extend-by-methods*
  "运行时版本：根据方法条目向量动态扩展类型。
   用法：(extend-by-methods* MyClass MyProto methods-vec)"
  [java-class protocol-name methods-vec]
  (eval `(extend-type ~java-class
           ~protocol-name
           ~@(mapcat (fn [{:keys [sym impl arity]}]
                       (let [params (repeatedly (dec arity) #(gensym "arg"))]
                         `((~sym [~'this ~@params] (~impl ~'this ~@params)))))
                     methods-vec))))

;; ============================================================
;; 快捷宏：define-class 和 use-class（修复 + spec 集成）
;; ============================================================
(defn- apply-name-mapper-and-dangerous [methods-info {:keys [name-mapper dangerous]}]
  (let [dangerous-set (set (map name dangerous))
        mapper-fn (when name-mapper (deref (resolve name-mapper)))]
    (mapv (fn [m]
            (let [clj (if mapper-fn (mapper-fn m) (:clj-name m))
                  clj (if (and dangerous-set (contains? dangerous-set (name clj))
                               (not (str/ends-with? (name clj) "!")))
                        (symbol (str (name clj) "!"))
                        clj)]
              (assoc m :clj-name clj)))
          methods-info)))

(defmacro define-class
  [java-interface protocol-name & opts]
  (let [;; 使用 spec 校验选项
        _ (s/assert ::spec/adapter-options (apply hash-map opts))
        {:keys [name-mapper dangerous]} (apply hash-map opts)
        methods-info (analyze-java-class* java-interface)
        methods-info (apply-name-mapper-and-dangerous methods-info {:name-mapper name-mapper :dangerous dangerous})
        method-sigs (for [m methods-info]
                      ;; 修复：基于 :param-count 生成参数
                      (list (:clj-name m) ['this] (repeatedly (:param-count m) (fn [] (gensym "arg")))))]
    `(defprotocol ~protocol-name ~@method-sigs)))

(defmacro use-class
  [java-class protocol-name & opts]
  (let [;; spec 校验
        _ (s/assert ::spec/adapter-options (apply hash-map opts))
        {:keys [except rename custom only name-mapper dangerous via]
         :or {except [] rename {} custom {} only nil via []}} (apply hash-map opts)
        methods-info (analyze-java-class* java-class)
        methods-info (apply-name-mapper-and-dangerous methods-info {:name-mapper name-mapper :dangerous dangerous})
        via-method-names (set (map first via))
        except-set (set (map name except))
        methods-info (cond
                       only   (filter #(contains? (set (map name only)) (name (:clj-name %))) methods-info)
                       :else  (remove #(or (contains? except-set (name (:clj-name %)))
                                           (contains? via-method-names (name (:clj-name %))))
                                      methods-info))
        auto-impls (for [m methods-info
                         :let [clj-name (or (get rename (:java-name m)) (:clj-name m))
                               params (repeatedly (:param-count m) (fn [] (gensym "arg")))]]
                     (if-let [cfn (get custom clj-name)]
                       `(~clj-name [~'this ~@params] (~cfn ~'this ~@params))
                       (let [jcall (symbol (str "." (:java-name m)))]
                         `(~clj-name [~'this ~@params] (~jcall ~'this ~@params)))))
        via-impls (mapcat (fn [entry]
                            (let [[proto-method getter target-method] (if (= (count entry) 2)
                                                                        [(first entry) (second entry) (protocol-method->java-getter (first entry))]
                                                                        entry)
                                  target-class-sym (some-> (resolve getter) .getReturnType .getName symbol)
                                  target-class (resolve target-class-sym)
                                  method-obj (some #(when (= (.getName %) (name target-method)) %) (.getMethods target-class))
                                  params (repeatedly (.getParameterCount method-obj) (fn [] (gensym "arg")))]
                              `((~proto-method [~'this ~@params]
                                  (let [obj# (~(symbol (str "." (name getter))) ~'this)]
                                    (~(symbol (str "." (name target-method))) obj# ~@params))))))
                          via)
        extra-custom (for [[cname cfn] custom
                           :when (not (some #(= cname (first %)) auto-impls))]
                       `(~cname [~'this ~'& args#] (apply ~cfn ~'this args#)))]
    `(extend-type ~java-class
       ~protocol-name
       ~@(apply concat (concat auto-impls via-impls extra-custom)))))

;; 调试工具（保持不变）
(defn inspect-java-class [class-sym]
  (let [methods (analyze-java-class* class-sym)]
    (println "=== Java Class:" class-sym "===")
    (doseq [m methods]
      (printf "  %-30s -> %s\n" (:java-name m) (:clj-name m)))
    (println)))