(ns top.kzre.use-class.prefix-test
  (:require [clojure.test :refer :all]
            [top.kzre.use-class.core :as core])
  (:import [java.util Date]))

;; 使用 Date 类测试，只取一个方法 getTime，添加前缀 "pfx-"
(core/use-class Date
                :protocol-name 'IPrefixTest
                :only ['getTime]
                :prefix "pfx-")

(deftest test-prefix-protocol-method-name
  (let [v (resolve 'IPrefixTest)]
    (is (some? v) "协议应已定义")
    (let [proto @v
          method-sigs (:sigs proto)
          method-name (ffirst method-sigs)]
      (is (= 'pfx-get-time method-name)
          "方法名应带有前缀 pfx-，并保持 kebab-case 转换"))))

(deftest test-prefix-function-call
  ;; 确保带前缀的函数可以正常调用
  (let [d (Date. 12345)]
    (is (= 12345 (pfx-get-time d)))))

;; 清理
(ns-unmap *ns* 'IPrefixTest)
(ns-unmap *ns* 'pfx-get-time)