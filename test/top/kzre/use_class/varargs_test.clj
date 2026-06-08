(ns top.kzre.use-class.varargs-test
  (:require [clojure.test :refer :all]
            [top.kzre.use-class.core :as core]))

(deftest test-varargs-preserved-when-wrapper-does-not-change-arity
  (let [;; 原始 method-sig 有变参: [this & keys]
        sig ['query-entities 'queryEntities ['this '& 'keys] 'Object nil
             ;; 包装器: 简单返回与原始相同变参的函数
             [(fn [f] (fn [this & args] (apply f this args)))]]
        updated (core/apply-wrapper-args sig)]
    (is (= ['this '& 'keys] (nth updated 2)))))

(deftest test-varargs-removed-when-wrapper-changes-arity
  (let [;; 原始方法有变参，但包装器返回只有 this 和一个固定参数
        sig ['query-entities 'queryEntities ['this '& 'keys] 'Object nil
             [(fn [f] (fn [this k] (f this k)))]]
        updated (core/apply-wrapper-args sig)]
    ;; 现在参数应该是 [this p1]（固定2个参数）
    (is (= 2 (count (nth updated 2))))
    (is (= 'this (first (nth updated 2))))
    (is (not (some '#{&} (nth updated 2))))))

(deftest test-no-wrapper-keeps-original
  (let [sig ['foo 'foo ['this '& 'args] 'void nil []]]
    (let [updated (core/apply-wrapper-args sig)]
      (is (= ['this '& 'args] (nth updated 2))))))


(deftest test-wrapper-without-varargs-produces-fixed-params
  (let [sig ['bar 'bar ['this] 'void nil [(fn [f] (fn [this x y] (f this x y)))]]
        updated (core/apply-wrapper-args sig)
        params (nth updated 2)]
    (is (= 3 (count params)))          ; this + p1 + p2 = 3
    (is (= 'this (first params)))))