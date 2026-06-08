(ns top.kzre.use-class.prefix-test
  (:require [clojure.test :refer :all]
            [top.kzre.use-class.core :as core])
  (:import [java.util Date]))

;; 使用 Date 类测试，只取一个方法 getTime，添加前缀 "pfx-"
(core/use-class Date
                :protocol-name 'IPrefixTest
                :only ['getTime]
                :prefix "pfx-")


(deftest test-prefix-function-call
  ;; 确保带前缀的函数可以正常调用
  (let [d (Date. 12345)]
    (is (= 12345 (pfx-get-time d)))))

;; 清理
(ns-unmap *ns* 'IPrefixTest)
(ns-unmap *ns* 'pfx-get-time)