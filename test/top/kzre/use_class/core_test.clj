(ns top.kzre.use-class.core-test
  (:require [clojure.test :refer :all]
            [top.kzre.use-class.core :as core]
            [top.kzre.use-class.spec :as spec])
  (:import (java.util Date)))

;; ── resolve-type ──
(deftest test-resolve-type-date
  (let [td (core/resolve-type Date)]
    (is (= 'java.util.Date (::spec/type-name td)))
    (is (vector? (::spec/method-sigs td)))
    (is (seq (::spec/method-sigs td)))))

;; ── rename ──
(deftest test-rename-methods-in-type-def
  (let [td (core/resolve-type Date)]
    (testing "rename map"
      (let [r (core/rename-methods-in-type-def td {'getTime 'fetch} nil)]
        (is (some #(= 'fetch %) (map first (::spec/method-sigs r))))))
    (testing "rename-fn"
      (let [r (core/rename-methods-in-type-def td {} #(symbol (str "x-" (name %))))]
        (is (some #(= 'x-getTime %) (map first (::spec/method-sigs r))))))))

;; ── filter ──
(deftest test-filter-methods-in-type-def
  (let [td (core/resolve-type Date)]
    (testing "include"
      (let [r (core/filter-methods-in-type-def td false '[getTime] [])]
        (is (= #{'getTime} (set (map first (::spec/method-sigs r)))))))
    (testing "exclude"
      (let [r (core/filter-methods-in-type-def td true [] '[getTime])]
        (is (not (some #(= 'getTime %) (map first (::spec/method-sigs r)))))))))

;; ── danger ──
(deftest test-mark-dangerous-in-type-def
  (let [td (core/rename-methods-in-type-def (core/resolve-type Date) {'getTime 'get-time 'setTime 'set-time} nil)]
    (testing "auto setter"
      (let [r (core/mark-dangerous-in-type-def td #{})]
        (is (some #(= 'set-time! %) (map first (::spec/method-sigs r))))))
    (testing "explicit danger"
      (let [r (core/mark-dangerous-in-type-def td #{'get-time} :setter-danger? false)]
        (is (some #(= 'get-time! %) (map first (::spec/method-sigs r))))))))

;; ── build-type-def ──
(deftest test-build-type-def-basic
  (let [td (core/build-type-def Date)]
    (is (some #(= 'get-time %) (map first (::spec/method-sigs td))))
    (is (some #(= 'set-time! %) (map first (::spec/method-sigs td))))))

(deftest test-build-type-def-with-only
  (let [td (core/build-type-def Date :only ['getTime 'setTime])]
    (is (= #{'get-time 'set-time!} (set (map first (::spec/method-sigs td)))))))

(deftest test-build-type-def-with-delegate
  (let [td (core/build-type-def 'java.util.Calendar :delegate [['getTime]])]
    (let [names (set (map first (::spec/method-sigs td)))]
      (is (contains? names 'get-time))
      (is (contains? names 'get-month))
      (is (contains? names 'get-calendar-type)))))

(deftest test-build-type-def-combined
  ;; 委托 getTime，重命名为 time-accessor，宿主同名也被重命名，冲突后保留宿主
  (let [td (core/build-type-def 'java.util.Calendar
                                :delegate [['getTime 'getTime]]
                                :rename {'getTime 'time-accessor}
                                :only ['getTime]
                                :dangerous #{'time-accessor})]
    (is (= #{'time-accessor!} (set (map first (::spec/method-sigs td)))))))

;; ── 包装器相关测试 ──

;; 无包装器时签名不变
(deftest test-apply-wrapper-args-no-wrapper
  (let [sig ['my-method 'myJavaMethod '[this arg1] 'void nil []]]
    (let [updated (core/apply-wrapper-args sig)]
      (is (= '[this arg1] (nth updated 2))))))

;; 高阶包装器签名推断（无额外参数）
(deftest test-apply-wrapper-args-high-order-no-extra
  (let [wrapper (fn [f] (fn [this] (f this)))
        sig ['m 'j '[this] 'v nil [wrapper]]]
    (let [updated (core/apply-wrapper-args sig)]
      (is (= '[this] (nth updated 2))))))

;; 高阶包装器签名推断（带额外参数，通过反射推断）
(deftest test-apply-wrapper-args-high-order-extra
  (let [wrapper (fn [f] (fn [this x y] (f this x y)))
        sig ['m 'j '[this] 'v nil [wrapper]]]
    (let [updated (core/apply-wrapper-args sig)]
      ;; 应推断出 3 个参数：this, arg1, arg2
      (is (= 3 (count (nth updated 2))))
      (is (= 'this (first (nth updated 2)))))))

;; wrap-impl-in-type-def
(deftest test-wrap-impl-in-type-def-with-java-name
  (let [td {::spec/method-sigs
            [['get-time 'getTime ['this] 'long {:delegate 'getTime}]
             ['set-time! 'setTime ['this 'long] 'void {:delegate 'setTime}]]}
        config {:global ['global-logger]
                :methods {'getTime ['cache]
                          'setTime ['validate]}}
        result (core/wrap-impl-in-type-def td config)]
    (is (= ['global-logger 'cache]
           (last (first (::spec/method-sigs result)))))
    (is (= ['global-logger 'validate]
           (last (second (::spec/method-sigs result)))))))

;; emit-extend-type 生成代码结构（单个包装器）
(deftest test-emit-extend-type-with-wrapper-final
  (let [type-def {::spec/type-name 'java.util.Date
                  ::spec/method-sigs
                  [['get-time 'getTime ['this] 'long {:delegate 'getTime} ['my-wrapper]]]}
        proto-def {::spec/protocol-name 'ITime
                   ::spec/protocol-method-sigs [['get-time ['this]]]}
        form (core/emit-extend-type type-def proto-def)]
    (is (seq? form))
    (is (= 'clojure.core/extend-type (first form)))
    (let [[_ _ _ & clauses] form
          [method-name params body] (first clauses)]
      (is (= 'get-time method-name))
      (is (= '[this] params))
      ;; body 的结构现在是 ((my-wrapper (fn [this] ...)) this)
      (is (seq? body))
      (is (= 2 (count body)))
      (let [wrapper-call (first body)   ;; (my-wrapper (fn [this] ...))
            this-sym      (second body)] ;; this
        (is (seq? wrapper-call))
        (is (= 'my-wrapper (first wrapper-call)))    ;; 直接调用包装器
        (let [inner-fn (second wrapper-call)]        ;; (fn [this] ...)
          (is (seq? inner-fn))
          (is (= 'clojure.core/fn (first inner-fn)))
          (is (= '[this] (second inner-fn)))
          (is (seq? (nth inner-fn 2)))
          (is (= '. (first (nth inner-fn 2)))))))))

;; ── 集成测试：使用 use-class 与高阶包装器 ──

;; 简单的身份包装器（不修改行为）
(defn my-identity [f] f)

(core/use-class Date
                :protocol-name 'IDateWrapperTest
                :only ['getTime]
                :wrappers {:methods {'getTime ['top.kzre.use-class.core-test/my-identity]}})

(deftest test-use-class-wrapper-identity
  (let [d (Date. 12345)]
    (is (= 12345 (get-time d)))))




(clojure.test/run-tests)


;; 测试完成后清理（避免污染后续可能的测试）
(ns-unmap *ns* 'IDateWrapperTest)
(ns-unmap *ns* 'get-time)
(ns-unmap *ns* 'IDateArithWrapper)
