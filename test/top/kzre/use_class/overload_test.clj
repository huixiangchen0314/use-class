(ns top.kzre.use-class.overload-test
  (:require [clojure.test :refer :all]
            [top.kzre.use-class.core :as core]
            [top.kzre.use-class.spec :as spec])
  (:import [java.util Calendar]))

;; ── 辅助：获取协议方法签名列表 ──
(defn- method-sigs [proto-def]
  (::spec/protocol-method-sigs proto-def))

;; ── 基本重载测试：通过 :only 指定多个 arity ──
(deftest test-explicit-arities-only
  (let [td (core/build-type-def 'java.util.Calendar
                                :only [['set 2] ['set 3]])
        ;; td 的 ::spec/method-sigs 应包含两个 set 方法，参数个数分别为 2 和 3（含 this）
        sigs (::spec/method-sigs td)]
    (is (= 2 (count sigs)))
    (is (every? #(= 'set %) (map first sigs)))
    (let [arities (map (comp dec count #(nth % 2)) sigs)]
      (is (contains? (set arities) 2))
      (is (contains? (set arities) 3)))))

;; ── 集成测试：use-class 生成的重载协议 ──
(core/use-class Calendar
                :protocol-name 'ICalOverload
                :only [['set 2] ['set 3]]
                :rename {'set 'set-cal})

(deftest test-use-class-overload-protocol
  (let [cal (Calendar/getInstance)]
    (testing "2-arg set"
      (is (nil? (set-cal! cal Calendar/YEAR 2020)))
      (is (= 2020 (.get cal Calendar/YEAR))))
    (testing "3-arg set"
      (is (nil? (set-cal! cal 2021 5 10)))
      (is (= 2021 (.get cal Calendar/YEAR)))
      (is (= 5 (.get cal Calendar/MONTH)))
      (is (= 10 (.get cal Calendar/DAY_OF_MONTH))))))

;; ── 测试默认只保留最大 arity ──
(deftest test-default-max-arity
  (let [td (core/build-type-def 'java.util.Calendar
                                :only ['set])]
    ;; set 有多个重载，默认只保留最大 arity 的一个
    (let [sigs (::spec/method-sigs td)]
      (is (= 1 (count sigs)))
      (is (= 'set (first (first sigs))))
      ;; 最大 arity：set(int year, int month, int date, int hourOfDay, int minute, int second) 有 6 个参数 (不含 this 是 5)
      (let [arity (dec (count (nth (first sigs) 2)))]
        (is (>= arity 5))))))

;; ── 测试 except 排除特定 arity 后仍保留最大 ──
(deftest test-except-arity
  (let [td (core/build-type-def 'java.util.Calendar
                                :except [['set 2]])]
    ;; 排除 2 参数版本后，其他重载保留最大 arity
    (let [sigs (::spec/method-sigs td)
          set-sigs (filter #(= 'set (first %)) sigs)]
      (is (= 1 (count set-sigs)))
      (let [arity (dec (count (nth (first set-sigs) 2)))]
        (is (>= arity 5))
        (is (not= arity 2))))))

;; ── 测试 exclude 不带 arity 则排除所有重载 ──
(deftest test-except-all-arities
  (let [td (core/build-type-def 'java.util.Calendar
                                :except ['set])]
    ;; 排除 set 方法，不应有任何 set 签名
    (let [sigs (::spec/method-sigs td)
          set-sigs (filter #(= 'set (first %)) sigs)]
      (is (empty? set-sigs)))))