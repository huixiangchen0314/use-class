(ns top.kzre.use-class.varargs-test
  (:require
   [clojure.string :as s]
   [clojure.test :refer :all]
   [top.kzre.use-class.core :as core])
  (:import
   (java.util Formatter)))

;; 定义带有变参元数据的包装器（模拟 query-entities-wrapper）
(defn preserve-varargs-wrapper {:arglists '([f] [this & args])} [f]
  (with-meta
    (fn [this & args] (apply f this args))
    {:arglists '([this & args])}))

;; 定义不带变参元数据的包装器
(defn fixed-args-wrapper [f]
  (fn [this k] (f this k)))

;; 定义增加参数个数的包装器（无元数据）
(defn extra-args-wrapper [f]
  (fn [this x y] (f this x y)))

;; 测试：有变参元数据 → 保留变参
(deftest test-varargs-preserved-with-metadata
  (let [sig ['query-entities 'queryEntities ['this '& 'keys] 'Object nil
             [preserve-varargs-wrapper]]
        updated (core/apply-wrapper-args sig)]
    (is (= ['this '& 'keys] (nth updated 2)))))

;; 测试：无元数据 → 生成固定参数（原始变参被忽略）
(deftest test-varargs-removed-without-metadata
  (let [sig ['query-entities 'queryEntities ['this '& 'keys] 'Object nil
             [fixed-args-wrapper]]
        updated (core/apply-wrapper-args sig)
        params (nth updated 2)]
    (is (= 2 (count params)) "应只有 this 和 k 两个参数")
    (is (= 'this (first params)))
    (is (not (some '#{&} params)) "不应包含变参标记")))

;; 测试：无包装器 → 保留原始签名
(deftest test-no-wrapper-keeps-original
  (let [sig ['foo 'foo ['this '& 'args] 'void nil []]]
    (let [updated (core/apply-wrapper-args sig)]
      (is (= ['this '& 'args] (nth updated 2))))))

;; 测试：原始无变参，包装器增加参数 → 生成固定参数
(deftest test-wrapper-without-varargs-produces-fixed-params
  (let [sig ['bar 'bar ['this] 'void nil [extra-args-wrapper]]
        updated (core/apply-wrapper-args sig)
        params (nth updated 2)]
    (is (= 3 (count params)))
    (is (= 'this (first params)))))


;; 变参包装器：返回设置的命令列表
;; 修正包装器：format 返回 Formatter，用 .toString 获取字符串再 upper-case
(defn format-wrapper {:arglists '([f] [this fmt-str & args])} [f]
  (with-meta
    (fn [this fmt-str & args]
      (s/upper-case
        (str (apply f this fmt-str args))))    ;; f 返回 Formatter，str 调用 .toString
    {:arglists '([this fmt-str & args])}))

;; 使用 (var format-wrapper) 让宏能可靠获取函数与元数据
(core/use-class Formatter
                :protocol-name 'IFormatTest
                :only [['format 2]]     ;; 精确选择两参数版本
                :rename {'format 'fmt}
                :wrappers {:methods {'format [(var format-wrapper)]}})

(deftest test-formatter-varargs
  (let [obj (java.util.Formatter.)]
    (is (= "TEST A B"
           (fmt obj "Test %s %s" "A" "B")))))