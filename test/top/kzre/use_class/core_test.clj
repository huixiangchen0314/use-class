(ns top.kzre.use-class.core-test
  (:require [clojure.test :refer :all]
            [top.kzre.use-class.core :as core])
  (:import (java.util Optional)))

;; ----------------------------------------------------------------------
;; 辅助：测试用的 Java 接口
;; ----------------------------------------------------------------------
(definterface ITestInterface
  (^int getCount [])
  (^void setCount [^int c])
  (^String getName [])
  (^java.util.Optional findById [^long id]))

(defn test-obj []
  (let [state (atom {:count 0 :name "test"})]
    (proxy [Object ITestInterface] []
      (getCount [] (:count @state))
      (setCount [c] (swap! state assoc :count c))
      (getName [] (:name @state))
      (findById [id] (java.util.Optional/of (str "entity-" id))))))

;; ----------------------------------------------------------------------
;; 顶层协议（供测试用，编译期可见）
;; ----------------------------------------------------------------------
(defprotocol EmptyProto
  (hello [this]))

(defprotocol ITestProto
  (count* [this])
  (set-count! [this c])
  (name* [this])
  (find-by-id [this id]))

;; ----------------------------------------------------------------------
;; analyze-class
;; ----------------------------------------------------------------------
(deftest analyze-class-test
  (testing "returns entries with correct shape"
    (let [entries (core/analyze-class 'java.util.Date)]
      (is (vector? entries))
      (is (seq entries))
      (doseq [e entries]
        (is (contains? e :sym))
        (is (contains? e :impl))
        (is (contains? e :arity))
        (is (integer? (:arity e)))
        (is (pos? (:arity e))))))
  (testing "getXxx -> xxx"
    (let [entries (core/analyze-class 'java.util.Date)
          time-entry (first (filter #(= "time" (name (:sym %))) entries))]
      (is time-entry)
      (is (= 'time (:sym time-entry)))))
  (testing "setXxx -> set-xxx!"
    (let [entries (core/analyze-class 'java.util.Date)
          set-entry (first (filter #(= "set-time!" (name (:sym %))) entries))]
      (is set-entry)
      (is (= 'set-time! (:sym set-entry)))))
  (testing "arity for no-arg method"
    (let [entries (core/analyze-class 'java.util.Date)
          get-time (first (filter #(= 'time (:sym %)) entries))]
      (is (= 1 (:arity get-time))))))

;; ----------------------------------------------------------------------
;; via-methods (with host-class)
;; ----------------------------------------------------------------------
(deftest via-methods-test
  (let [host 'top.kzre.use_class.core_test.ITestInterface]
    (testing "explicit target method"
      (let [entries (core/via-methods host 'getName [['entity-name 'startsWith]])]
        (is (= 1 (count entries)))
        (let [e (first entries)]
          (is (= 'entity-name (:sym e)))
          (is (= 2 (:arity e)))   ; startsWith takes one String -> arity 2
          (is (fn? (eval (:impl e)))))))
    (testing "auto-derived target fails if method missing"
      (is (thrown? Exception (core/via-methods host 'getName [['nonexistent]]))))))

;; ----------------------------------------------------------------------
;; custom-methods
;; ----------------------------------------------------------------------
(deftest custom-methods-test
  (let [e (core/custom-entry 'my-method 3 (fn [this a b] (+ a b)))]
    (is (= 'my-method (:sym e)))
    (is (= 3 (:arity e)))
    (is (fn? (:impl e))))
  (let [entries (core/custom-methods [['foo 2 (fn [this x] x)]
                                      ['bar 3 (fn [this x y] (+ x y))]])]
    (is (= 2 (count entries)))
    (is (= 'foo (:sym (first entries))))
    (is (= 'bar (:sym (second entries))))))

;; ----------------------------------------------------------------------
;; merge / filter / rename
;; ----------------------------------------------------------------------
(deftest method-pipeline-test
  (let [e1 (core/custom-entry 'a 1 identity)
        e2 (core/custom-entry 'b 1 identity)
        e3 (core/custom-entry 'a 2 (fn [_] 42))]
    (testing "merge overwrites duplicates"
      (let [merged (core/merge-methods [e1 e2] [e3])]
        (is (= 2 (count merged)))
        (is (= 2 (:arity (first (filter #(= 'a (:sym %)) merged)))))))
    (testing "filter :except"
      (let [filtered (core/filter-methods [e1 e2] :except ['a])]
        (is (= 1 (count filtered)))
        (is (= 'b (:sym (first filtered))))))
    (testing "filter :only"
      (let [filtered (core/filter-methods [e1 e2] :only ['b])]
        (is (= 1 (count filtered)))
        (is (= 'b (:sym (first filtered))))))
    (testing "rename"
      (let [renamed (core/rename-methods [e1] {'a 'aa})]
        (is (= 'aa (:sym (first renamed))))))))

;; ----------------------------------------------------------------------
;; mark-dangerous / apply-name-mapper
;; ----------------------------------------------------------------------
(deftest post-processing-test
  (let [entries [(core/custom-entry 'destroy 1 (fn [_] nil))
                 (core/custom-entry 'fire 1 (fn [_] nil))
                 (core/custom-entry 'update 1 (fn [_] nil))]]
    (testing "mark-dangerous adds !"
      (let [marked (core/mark-dangerous entries #{'destroy 'fire})]
        (is (= 'destroy! (:sym (first marked))))
        (is (= 'fire! (:sym (second marked))))
        (is (= 'update (:sym (nth marked 2))))))
    (testing "apply-name-mapper"
      (let [mapped (core/apply-name-mapper entries (fn [s] (symbol (str (name s) "-custom"))))]
        (is (= 'destroy-custom (:sym (first mapped))))))))

;; ----------------------------------------------------------------------
;; conversion
;; ----------------------------------------------------------------------
(deftest conversion-test
  (testing "wrap-results with explicit converter (代替 auto-wrap)"
    (let [entries [(core/custom-entry 'opt 1 (fn [this] (java.util.Optional/of 42)))]
          wrapped (core/wrap-results entries {'opt (fn [^java.util.Optional opt] (.get opt))})
          f (eval (:impl (first wrapped)))]
      (is (fn? f))
      (is (= 42 (f nil)))))
  (testing "wrap-results with inc converter"
    (let [entries [(core/custom-entry 'num 1 (fn [this] 5))]
          wrapped (core/wrap-results entries {'num inc})
          f (eval (:impl (first wrapped)))]
      (is (= 6 (f nil)))))
  (testing "wrap-args converts arguments (correct indices)"
    (let [entries [(core/custom-entry 'add 3 (fn [this a b] (+ a b)))]
          wrapped (core/wrap-args entries {'add [[0 inc] [1 inc]]})  ; position 0 and 1
          f (eval (:impl (first wrapped)))]
      (is (= 8 (f nil 2 4))))))   ; 2→3, 4→5, 3+5=8

;; ----------------------------------------------------------------------
;; macro expansion (only define-class and use-class, using safe interface)
;; ----------------------------------------------------------------------
(deftest macro-expansion-test
  (testing "define-class expands to defprotocol (one level)"
    (let [form (macroexpand-1 `(core/define-class top.kzre.use_class.core_test.ITestInterface DateProtocol))]
      (is (= 'clojure.core/defprotocol (first form)))
      (is (= 'top.kzre.use-class.core-test/DateProtocol (second form)))))
  (testing "use-class expands to extend-type (one level)"
    (let [form (macroexpand-1 `(core/use-class top.kzre.use_class.core_test.ITestInterface DateProtocol :except [getClass]))]
      (is (= 'clojure.core/extend-type (first form)))
      (is (= 'top.kzre.use_class.core_test.ITestInterface (second form))))))



;; extend-by-methods 行为测试（用 extend 函数，无 eval）
(deftest extend-by-methods-behavior
  (let [obj (Object.)]
    (extend (class obj)
      EmptyProto
      {:hello (fn [this] "world")})
    (is (= "world" (hello obj)))))

;; 集成测试（用 extend 函数，无 eval）
(deftest integration-test
  (let [obj (test-obj)]
    (extend (class obj)
      ITestProto
      {:count*      (fn [this] (.getCount this))
       :set-count!  (fn [this c] (.setCount this c))
       :name*       (fn [this] (.getName this))
       :find-by-id  (fn [this id]
                      (let [^java.util.Optional opt (.findById this id)]
                        (when (.isPresent opt) (.get opt))))})
    (testing "protocol method calls"
      (is (= 0 (count* obj)))
      (set-count! obj 5)
      (is (= 5 (count* obj)))
      (is (= "test" (name* obj)))
      (is (= "entity-42" (find-by-id obj 42))))))