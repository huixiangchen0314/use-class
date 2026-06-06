(ns top.kzre.use-class.wrapper-clean-test
  (:require [clojure.test :refer :all]
            [top.kzre.use-class.core :as core])
  (:import [java.util Date]))

(defn add-100-wrapper [f]
  (fn [this] (+ (f this) 100)))

;; ★ 正确的高阶形式：只接收 f，返回带额外参数的函数
;; ★ 必须通过元数据声明最终函数的 arglists
(defn add-n-wrapper {:arglists '([f] [this n])} [f]
  (fn [this n] (+ (f this 5) n)))

(defn mul-n-wrapper [f]
  (fn [this n] (* (f this) n)))


(core/use-class Date
                :protocol-name 'IDateArithWrapper
                :only ['getTime]
                :wrappers {:methods {'getTime ['add-100-wrapper
                                               mul-n-wrapper
                                               'add-n-wrapper]}})

(deftest test-protocol-available
  (let [proto @(resolve 'IDateArithWrapper)]
    (is (some? proto) "协议应已定义")))

;(deftest test-signature
;  (let [proto @(resolve 'IDateArithWrapper)
;        method-info (get (:sigs proto) :get-time)]
;    (is method-info)
;    ;; 签名应为 [this n]
;    (is (= '([this n]) (:arglists method-info)))))

(deftest test-behavior
  (let [d (Date. 2)]

    (is (= 515 (get-time d 5)))))

(clojure.test/run-tests)