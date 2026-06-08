(ns top.kzre.use-class.optional-wrapper-test
  (:require [clojure.test :refer :all]
            [top.kzre.use-class.core :as core])
  (:import [java.util Optional]))

;; ── 定义一个简单的接口和一个实现类，用于提供返回 Optional 的方法 ──
(definterface OptionalFetcher
  (^java.util.Optional fetchValue []))

(deftype OptionalProvider [^java.util.Optional val]
  OptionalFetcher
  (fetchValue [_] val))

;; ── 高阶包装器：自动拆包 Optional ──
(defn optional-wrapper [f]
  (fn [this & args]
    (let [result (apply f this args)]
      (if (instance? Optional result)
        (.orElse result nil)
        result))))

;; ── 使用宏生成协议与实现 ──
(core/use-class OptionalProvider
                :protocol-name 'IOptionalFetcher
                :only ['fetchValue]
                :wrappers {:methods {'fetchValue ['optional-wrapper]}})

;; ── 测试 ──
(deftest test-optional-wrapper-present
  (let [provider (OptionalProvider. (Optional/of "hello"))
        result   (fetch-value provider)]
    (is (= "hello" result) "Optional 中有值时应返回内部值")))

(deftest test-optional-wrapper-empty
  (let [provider (OptionalProvider. (Optional/empty))
        result   (fetch-value provider)]
    (is (nil? result) "Optional 为空时应返回 nil")))

