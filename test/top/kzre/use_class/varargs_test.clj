(ns top.kzre.use-class.varargs-test
  (:require
    [clojure.string :as s]
    [clojure.test :refer :all]
    [top.kzre.use-class.core :as core])
  (:import
    (clojure.lang ArityException)
    (java.util Formatter)))

(defn format-wrapper [f]
  (with-meta
    (fn [this fmt-str & args]
      (s/upper-case
        (str (apply f this fmt-str args))))
    {:arglists '([this fmt-str & args])}))

;; 测试默认展开（上限 20）
(core/use-class Formatter
                :protocol-name 'IFormatterTest
                :only [['format 1]]
                :rename {'format 'fmt}
                :wrappers {:methods {'format [format-wrapper]}})

(deftest test-varargs-expansion-default
  (testing "0 extra args"
    (let [f (Formatter.)]
      (is (= "TEST" (fmt f "Test")))))
  (testing "1 extra arg"
    (let [f (Formatter.)]
      (is (= "TEST A" (fmt f "Test %s" "A")))))
  (testing "3 extra args"
    (let [f (Formatter.)]
      (is (= "TEST A B C" (fmt f "Test %s %s %s" "A" "B" "C"))))))

; 测试自定义上限
(core/use-class Formatter
                :protocol-name 'IFormatterLimit
                :varargs {'format 2}   ;; 只展开到 2 个额外参数
                :rename {'format 'fmt2}
                :wrappers {:methods {'format [format-wrapper]}})

;(deftest test-varargs-custom-limit
;  (let [f (Formatter.)]
;    (is (thrown? clojure.lang.ArityException
;                 (fmt2 f "Test %s %s %s" "A" "B" "C"))))) ;; 3 额外参数应失败



;; 包装器声明变参，应展开
(defn my-varargs-wrapper [f]
  (with-meta
    (fn [this fmt-str & args]
      (str (apply f this fmt-str (reverse args))))  ;; 将结果转为字符串
    {:arglists '([this fmt-str & args])}))

(core/use-class Formatter
                :protocol-name 'IFormatterWrapper
                :only [['format 1]]
                :rename {'format 'fmt4}
                :wrappers {:methods {'format [my-varargs-wrapper]}})

(deftest test-wrapper-varargs-expansion
  (let [f (Formatter.)]
    (is (= "C B A" (fmt4 f "%s %s %s" "A" "B" "C")))))

;; 运行所有测试
(clojure.test/run-tests)