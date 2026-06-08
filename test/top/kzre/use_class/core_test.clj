(ns top.kzre.use-class.core-test
  (:require [clojure.test :refer :all]
            [top.kzre.use-class.core :as core]
            [top.kzre.use-class.spec :as spec])
  (:import (java.util Date Calendar)))

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
    (testing "include by name (symbol -> normalized)"
      (let [entries (core/normalize-filter-entries '[getTime])
            r (core/filter-methods-in-type-def td false entries [])]
        (is (= #{'getTime} (set (map first (::spec/method-sigs r)))))))
    (testing "exclude by name"
      (let [entries (core/normalize-filter-entries '[getTime])
            r (core/filter-methods-in-type-def td true [] entries)]
        (is (not (some #(= 'getTime %) (map first (::spec/method-sigs r)))))))
    (testing "include by name and arity"
      (let [entries (core/normalize-filter-entries '[[setTime 1]])
            r (core/filter-methods-in-type-def td false entries [])]
        (is (some #(= 'setTime %) (map first (::spec/method-sigs r))))
        (is (= 1 (dec (count (nth (first (::spec/method-sigs r)) 2)))))))))

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

(deftest test-build-type-def-with-explicit-arities
  (let [td (core/build-type-def Date :only [['getTime 0] ['setTime 1]])]
    (is (= #{'get-time 'set-time!} (set (map first (::spec/method-sigs td)))))))

(deftest test-build-type-def-with-delegate
  (let [td (core/build-type-def 'java.util.Calendar :delegate [['getTime]])]
    (let [names (set (map first (::spec/method-sigs td)))]
      (is (contains? names 'get-time))
      (is (contains? names 'get-month))
      (is (contains? names 'get-calendar-type)))))

(deftest test-build-type-def-combined
  (let [td (core/build-type-def 'java.util.Calendar
                                :delegate [['getTime 'getTime]]
                                :rename {'getTime 'time-accessor}
                                :only ['getTime]
                                :dangerous #{'time-accessor})]
    (is (= #{'time-accessor!} (set (map first (::spec/method-sigs td)))))))

;; ── 包装器签名推断 ──
(deftest test-apply-wrapper-args-no-wrapper
  (let [sig ['my-method 'myJavaMethod '[this arg1] 'void nil []]]
    (let [updated (core/apply-wrapper-args sig)]
      (is (= '[this arg1] (nth updated 2))))))

(deftest test-apply-wrapper-args-with-meta-no-extra
  (let [wrapper (fn [f]
                  (with-meta (fn [this] (f this))
                             {:arglists '([this])}))
        sig ['m 'j '[this] 'v nil [wrapper]]]
    (let [updated (core/apply-wrapper-args sig)]
      (is (= '[this] (nth updated 2))))))

(deftest test-apply-wrapper-args-with-meta-extra
  (let [wrapper (fn [f]
                  (with-meta (fn [this x y] (f this x y))
                             {:arglists '([this x y])}))
        sig ['m 'j '[this] 'v nil [wrapper]]]
    (let [updated (core/apply-wrapper-args sig)]
      ;; 应推断出 3 个参数：this, arg1, arg2
      (is (= 3 (count (nth updated 2))))
      (is (= 'this (first (nth updated 2)))))))

;; ── wrap-impl-in-type-def ──
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

;; ── emit-extend-type（三参数版本）──
(deftest test-emit-extend-type-with-wrapper
  (let [type-def {::spec/type-name 'java.util.Date
                  ::spec/method-sigs
                  [['get-time 'getTime ['this] 'long {:delegate 'getTime} ['my-wrapper]]]}
        proto-params [['this]]      ;; 包装器调整后的参数列表
        form (core/emit-extend-type type-def 'ITime proto-params)]
    (is (seq? form))
    (is (= 'clojure.core/extend-type (first form)))
    (let [[_ _ _ & clauses] form]
      (is (= 1 (count clauses)))
      (let [method-entry (first clauses)
            method-name  (first method-entry)
            overloads    (rest method-entry)]
        (is (= 'get-time method-name))
        (is (= 1 (count overloads)))
        (let [[params body] (first overloads)]
          (is (= '[this] params))
          (is (seq? body))
          (is (= 2 (count body)))
          (let [comb-call (first body)]
            (is (seq? comb-call))
            (is (= 'my-wrapper (first comb-call)))
            (let [inner-fn (second comb-call)]
              (is (seq? inner-fn))
              (is (= 'clojure.core/fn (first inner-fn)))
              (is (= '[this] (second inner-fn)))
              (is (seq? (nth inner-fn 2)))
              (is (= '. (first (nth inner-fn 2)))))))))))

;; ── 集成测试：包装器 identity ──
(defn my-identity [f] f)

(core/use-class Date
                :protocol-name 'IDateWrapperTest
                :only ['getTime]
                :wrappers {:methods {'getTime ['top.kzre.use-class.core-test/my-identity]}})

(deftest test-use-class-wrapper-identity
  (let [d (Date. 12345)]
    (is (= 12345 (get-time d)))))

(run-tests)
(ns-unmap *ns* 'IDateWrapperTest)
(ns-unmap *ns* 'get-time)