(ns top.kzre.use-class.integration-test
  (:require [clojure.test :refer :all]
            [top.kzre.use-class.core :as core])
  (:import (java.util Date)))

(defn epoch-seconds-impl [this]
  (quot (.getTime ^Date this) 1000))

(defn log-call-wrapper [f]
  (fn [this & args]
    (println "Calling" (class this) "with" args)
    (apply f this args)))

;; 所有方法名、协议名等符号都不加引号，只有 Var 引用加引号
(core/use-class Date
                :protocol-name IDateFullTest
                :rename {getTime full-get-time, setTime full-set-time}
                :only [full-get-time full-set-time]
                :dangerous #{full-set-time}
                :wrappers {:global ['top.kzre.use-class.integration-test/log-call-wrapper]})

(core/use-class java.util.Calendar
                :protocol-name ICalUseTest
                :rename {getTime cal-get-time}
                :only [cal-get-time]
                :delegate [[cal-get-time getTime getTime]])    ;; getTime 无引号

(core/use-class Date
                :protocol-name IDateEpochTest
                :rename {getTime epoch-get-time}
                :only [epoch-get-time epoch-seconds]           ;; epoch-seconds 无引号
                :custom [[epoch-seconds 1 'top.kzre.use-class.integration-test/epoch-seconds-impl]])

(core/defprotocol-from-type java.util.Optional
                            :protocol-name IOptionalBasicTest
                            :rename {get opt-get, isPresent opt-is-present}
                            :only [opt-get opt-is-present])

(core/defprotocol-from-type java.util.concurrent.atomic.AtomicInteger
                            :protocol-name IAtomicDanger
                            :rename {get atomic-get, set atomic-set}
                            :only [atomic-get atomic-set]
                            :dangerous #{atomic-set})

(core/defprotocol-from-type java.util.Calendar
                            :protocol-name ICalDelegate
                            :rename {getTime cal-delegate-get-time}
                            :only [cal-delegate-get-time]
                            :delegate [[cal-delegate-get-time getTime]])    ;; getTime 无引号

(core/defprotocol-from-type Date
                            :protocol-name IDateCustom
                            :rename {getTime custom-get-time}
                            :only [custom-get-time custom-epoch-seconds]
                            :custom [[custom-epoch-seconds 1 'top.kzre.use-class.integration-test/epoch-seconds-impl]])

(deftest test-defprotocol-from-type-basic
  (let [proto @(resolve 'IOptionalBasicTest)]
    (is (contains? (:sigs proto) 'opt-get))
    (is (contains? (:sigs proto) 'opt-is-present))))

(deftest test-defprotocol-from-type-danger
  (let [proto @(resolve 'IAtomicDanger)]
    (is (contains? (:sigs proto) 'atomic-get))
    (is (contains? (:sigs proto) 'atomic-set!))))

(deftest test-defprotocol-from-type-delegate
  (let [proto @(resolve 'ICalDelegate)]
    (is (contains? (:sigs proto) 'cal-delegate-get-time))
    (is (= 1 (count (:sigs proto))))))

(deftest test-defprotocol-from-type-custom
  (let [proto @(resolve 'IDateCustom)]
    (is (contains? (:sigs proto) 'custom-epoch-seconds))
    (is (contains? (:sigs proto) 'custom-get-time))))

(deftest test-use-class-full
  (let [d (Date. 12345)]
    (is (= 12345 (full-get-time d)))
    (full-set-time! d 67890)
    (is (= 67890 (.getTime d)))))

(deftest test-use-class-via
  (let [cal (java.util.Calendar/getInstance)
        d (Date. 55555)]
    (.setTime cal d)
    (is (= 55555 (cal-get-time cal)))))

(deftest test-use-class-custom
  (let [d (Date. 12345)]
    (is (= 12 (epoch-seconds d)))))

(deftest test-name-conflict
  (is (thrown? Exception
               (core/defprotocol-from-type Date
                                           :protocol-name IConflictTest))))