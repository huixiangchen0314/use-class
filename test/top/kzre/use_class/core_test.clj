(ns top.kzre.use-class.core-test
  (:require [clojure.test :refer :all]
            [top.kzre.use-class.core :as core]
            [top.kzre.use-class.spec :as spec]))

;; ── resolve-type ──
(deftest test-resolve-type-date
  (let [td (core/resolve-type 'java.util.Date)]
    (is (= 'java.util.Date (::spec/type-name td)))
    (is (vector? (::spec/method-sigs td)))
    (is (seq (::spec/method-sigs td)))))

;; ── rename ──
(deftest test-rename-methods-in-type-def
  (let [td (core/resolve-type 'java.util.Date)]
    (testing "rename map"
      (let [r (core/rename-methods-in-type-def td {'getTime 'fetch} nil)]
        (is (some #(= 'fetch %) (map first (::spec/method-sigs r))))))
    (testing "rename-fn"
      (let [r (core/rename-methods-in-type-def td {} #(symbol (str "x-" (name %))))]
        (is (some #(= 'x-getTime %) (map first (::spec/method-sigs r))))))))

;; ── filter ──
(deftest test-filter-methods-in-type-def
  (let [td (core/resolve-type 'java.util.Date)]
    (testing "include"
      (let [r (core/filter-methods-in-type-def td false '[getTime] [])]
        (is (= #{'getTime} (set (map first (::spec/method-sigs r)))))))
    (testing "exclude"
      (let [r (core/filter-methods-in-type-def td true [] '[getTime])]
        (is (not (some #(= 'getTime %) (map first (::spec/method-sigs r)))))))))

;; ── danger ──
(deftest test-mark-dangerous-in-type-def
  (let [td (core/rename-methods-in-type-def (core/resolve-type 'java.util.Date) {'getTime 'get-time 'setTime 'set-time} nil)]
    (testing "auto setter"
      (let [r (core/mark-dangerous-in-type-def td #{})]
        (is (some #(= 'set-time! %) (map first (::spec/method-sigs r))))))
    (testing "explicit danger"
      (let [r (core/mark-dangerous-in-type-def td #{'get-time} :setter-danger? false)]
        (is (some #(= 'get-time! %) (map first (::spec/method-sigs r))))))))

;; ── build-type-def ──
(deftest test-build-type-def-basic
  (let [td (core/build-type-def 'java.util.Date)]
    (is (some #(= 'get-time %) (map first (::spec/method-sigs td))))
    (is (some #(= 'set-time! %) (map first (::spec/method-sigs td))))))

(deftest test-build-type-def-with-only
  (let [td (core/build-type-def 'java.util.Date :only ['getTime 'setTime])]
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