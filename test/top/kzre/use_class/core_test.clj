(ns top.kzre.use-class.core-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [top.kzre.use-class.core :as core]
            [top.kzre.use-class.spec :as spec])
  (:import (clojure.lang ExceptionInfo)))

;; ── resolve-type ──
(deftest test-resolve-type-date
  (let [type-def (core/resolve-type 'java.util.Date)]
    (is (map? type-def))
    (is (= 'java.util.Date (::spec/type-name type-def)))
    (let [sigs (::spec/method-sigs type-def)]
      (is (vector? sigs))
      (is (seq sigs))
      (doseq [sig sigs]
        (is (vector? sig))
        (is (<= 4 (count sig)))   ; 至少 4 个元素
        (let [[proto java params return] sig]
          (is (symbol? proto))
          (is (symbol? java))
          (is (vector? params))
          (is (symbol? return))
          (is (= 'this (first params)))))
      (let [proto-names (into #{} (map first sigs))]
        (doseq [object-method '[equals hashCode toString getClass notify notifyAll wait]]
          (is (not (contains? proto-names object-method))
              (str "不应包含 Object 方法: " object-method))))
      (let [get-time-sig (first (filter #(-> % first name (= "getTime")) sigs))]
        (is get-time-sig "应存在 getTime 方法")
        (is (= '[this] (nth get-time-sig 2)))
        (is (= 'long (nth get-time-sig 3)))))))

;; ── rename ──
(deftest test-rename-methods-in-type-def
  (let [date-td (core/resolve-type 'java.util.Date)
        orig-sigs (::spec/method-sigs date-td)]
    (testing "使用 rename-map 直接覆盖"
      (let [renamed (core/rename-methods-in-type-def date-td {'getTime 'fetch-time} nil)
            proto-names (map first (::spec/method-sigs renamed))]
        (is (some #(= 'fetch-time %) proto-names))
        (is (not (some #(= 'getTime %) proto-names)))))
    (testing "使用 rename-fn 自定义转换（无映射时）"
      (let [kebab-fn (fn [s]
                       (let [sname (name s)]
                         (if (re-find #"^get[A-Z]" sname)
                           (-> (subs sname 3)
                               (str/replace #"([A-Z])" #(str "-" (.toLowerCase (second %))))
                               (str/replace #"^-" "")
                               symbol)
                           s)))
            renamed (core/rename-methods-in-type-def date-td {} kebab-fn)
            proto-names (map first (::spec/method-sigs renamed))]
        (is (some #(= 'time %) proto-names))
        (is (not (some #(= 'getTime %) proto-names)))))
    (testing "映射优先于函数"
      (let [renamed (core/rename-methods-in-type-def date-td
                                                     {'getTime 'my-get-time}
                                                     (fn [_] 'should-not-be-used))
            proto-names (map first (::spec/method-sigs renamed))]
        (is (some #(= 'my-get-time %) proto-names))
        (is (not (some #(= 'getTime %) proto-names)))
        (is (some #(= 'should-not-be-used %) proto-names))
        (is (every? #(or (= 'my-get-time %) (= 'should-not-be-used %)) proto-names))))
    (testing "无映射且无函数时保持原名"
      (let [renamed (core/rename-methods-in-type-def date-td {} nil)]
        (is (= (set (map first orig-sigs))
               (set (map first (::spec/method-sigs renamed)))))))))

;; ── filter ──
(deftest test-filter-methods-in-type-def
  (let [date-td (core/resolve-type 'java.util.Date)
        all-names (map first (::spec/method-sigs date-td))]
    (testing "default-include true 无 exclude → 保留全部"
      (let [res (core/filter-methods-in-type-def date-td true [] [])]
        (is (= (set all-names)
               (set (map first (::spec/method-sigs res)))))))
    (testing "default-include true 配合 exclude 排除 getTime"
      (let [res (core/filter-methods-in-type-def date-td true [] '[getTime])]
        (is (not (some #(= 'getTime %) (map first (::spec/method-sigs res)))))
        (is (some #(= 'setTime %) (map first (::spec/method-sigs res))))))
    (testing "default-include false 无 include → 空集合"
      (let [res (core/filter-methods-in-type-def date-td false [] [])]
        (is (empty? (::spec/method-sigs res)))))
    (testing "default-include false 配合 include 只保留指定方法"
      (let [res (core/filter-methods-in-type-def date-td false '[getTime setTime] [])]
        (is (= #{'getTime 'setTime}
               (set (map first (::spec/method-sigs res))))))
      (testing "default-include false 时 ignore exclude"
        (let [res (core/filter-methods-in-type-def date-td false '[getTime] '[setTime])]
          (is (= #{'getTime}
                 (set (map first (::spec/method-sigs res))))))))))

  ;; ── danger ──
  (deftest test-mark-dangerous-in-type-def
    (let [date-td (core/resolve-type 'java.util.Date)
          renamed (core/rename-methods-in-type-def date-td
                                                   {'getTime 'get-time 'setTime 'set-time} nil)
          proto-names (set (map first (::spec/method-sigs renamed)))]
      (testing "指定方法添加 !"
        (let [danger-set #{'get-time 'set-time}
              result (core/mark-dangerous-in-type-def renamed danger-set :setter-danger? false)
              result-names (map first (::spec/method-sigs result))]
          (is (some #(= 'get-time! %) result-names))
          (is (some #(= 'set-time! %) result-names))
          (is (not (some #(= 'get-time %) result-names)))
          (is (not (some #(= 'set-time %) result-names)))))
      (testing "已带 ! 的方法不会重复添加 !"
        (let [td (assoc date-td ::spec/method-sigs [['already! 'already! ['this] 'void]])
              danger-set #{'already!}
              result (core/mark-dangerous-in-type-def td danger-set :setter-danger? false)
              result-names (map first (::spec/method-sigs result))]
          (is (= 'already! (first result-names)))
          (is (not= 'already!! (first result-names)))))
      (testing "非危险方法不变"
        (let [danger-set #{'get-time}
              result (core/mark-dangerous-in-type-def renamed danger-set :setter-danger? false)
              result-names (map first (::spec/method-sigs result))
              safe-method (first (remove (fn [n] (or (= 'get-time n) (str/starts-with? (name n) "set-"))) proto-names))]
          (is (some #(= safe-method %) result-names))
          (is (not (some #(= (symbol (str (name safe-method) "!")) %) result-names)))))
      (testing "自动 setter 危险标记（默认开启）"
        (let [result (core/mark-dangerous-in-type-def renamed #{})
              result-names (map first (::spec/method-sigs result))]
          (is (some #(= 'set-time! %) result-names))
          (is (not (some #(= 'set-time %) result-names)))
          (is (some #(= 'get-time %) result-names))
          (is (not (some #(= 'get-time! %) result-names)))))
      (testing "显式关闭自动 setter 标记"
        (let [result (core/mark-dangerous-in-type-def renamed #{} :setter-danger? false)
              result-names (map first (::spec/method-sigs result))]
          (is (some #(= 'set-time %) result-names))
          (is (not (some #(= 'set-time! %) result-names)))))
      (testing "自动 setter 与显式 danger-set 可叠加"
        (let [result (core/mark-dangerous-in-type-def renamed #{'get-time} :setter-danger? true)
              result-names (map first (::spec/method-sigs result))]
          (is (some #(= 'get-time! %) result-names))
          (is (some #(= 'set-time! %) result-names))
          (is (not (some #(= 'get-time %) result-names)))
          (is (not (some #(= 'set-time %) result-names)))))))

  ;; ── build-type-def 系列 ──
  (deftest test-build-type-def-basic
    (let [type-def (core/build-type-def 'java.util.Date)]
      (is (= 'java.util.Date (::spec/type-name type-def)))
      (let [sigs (::spec/method-sigs type-def)]
        ;(is (vector? sigs))
        (is (seq sigs))
        (doseq [sig sigs]
          (let [proto (first sig)]
            (is (not (contains? #{'equals 'hashCode 'toString 'getClass 'notify 'notifyAll 'wait}
                                proto))))))))

  (deftest test-build-type-def-with-delegate-single
    (let [type-def (core/build-type-def 'java.util.Calendar :delegate [['getTime]])]
      (is (contains? (set (map first (::spec/method-sigs type-def))) 'getTime))
      (is (contains? (set (map first (::spec/method-sigs type-def))) 'getMonth))
      (is (contains? (set (map first (::spec/method-sigs type-def))) 'getCalendarType))))

  (deftest test-build-type-def-with-rename
    (let [type-def (core/build-type-def 'java.util.Date :rename {'getTime 'fetch-time})]
      (is (contains? (set (map first (::spec/method-sigs type-def))) 'fetch-time))
      (is (not (contains? (set (map first (::spec/method-sigs type-def))) 'getTime)))))

  (deftest test-build-type-def-with-only
    (let [type-def (core/build-type-def 'java.util.Date :only ['getTime 'setTime])]
      (is (= 2 (count (::spec/method-sigs type-def))))
      (is (= #{'getTime 'setTime} (set (map first (::spec/method-sigs type-def)))))))

  (deftest test-build-type-def-with-except
    (let [type-def (core/build-type-def 'java.util.Date :except ['getTime])]
      (is (not (contains? (set (map first (::spec/method-sigs type-def))) 'getTime)))
      (is (contains? (set (map first (::spec/method-sigs type-def))) 'setTime))))

  (deftest test-build-type-def-with-dangerous
    (let [type-def (core/build-type-def 'java.util.Date
                                        :rename {'getTime 'get-time 'setTime 'set-time}
                                        :only ['get-time 'set-time]
                                        :dangerous #{'set-time})]
      (is (contains? (set (map first (::spec/method-sigs type-def))) 'get-time))
      (is (contains? (set (map first (::spec/method-sigs type-def))) 'set-time!))
      (is (not (contains? (set (map first (::spec/method-sigs type-def))) 'set-time)))))

  (deftest test-build-type-def-combined
    (let [type-def (core/build-type-def 'java.util.Calendar
                                        :delegate [['cal-get-time 'getTime 'getTime]]
                                        :rename {'getTime 'time-accessor}
                                        :only ['time-accessor 'cal-get-time]
                                        :dangerous #{'time-accessor})]
      (let [sigs (::spec/method-sigs type-def)
            names (map first sigs)]
        (is (= 2 (count sigs)))
        (is (contains? (set names) 'time-accessor!))
        (is (contains? (set names) 'cal-get-time)))))

  (deftest test-build-type-def-conflict
    (let [type-def (core/build-type-def 'java.util.Calendar
                                        :delegate [['getTime 'getTime 'getTime]]
                                        :rename {})]
      (let [sigs (::spec/method-sigs type-def)
            get-time-sigs (filter #(= (first %) 'getTime) sigs)]
        (is (= 1 (count get-time-sigs)))
        (is (= '[this] (nth (first get-time-sigs) 2))))))

  ;; ── emit-defprotocol ──
  (deftest test-emit-defprotocol
    (let [proto-def {::spec/protocol-name 'ITest
                     ::spec/protocol-method-sigs [['get-time ['this]] ['set-time ['this 'long]]]}
          form (core/emit-defprotocol proto-def)]
      (is (= 'clojure.core/defprotocol (first form)))
      (is (= 'ITest (second form)))
      (is (= 2 (count (nth form 2))))))

  ;; ── 实现注入策略测试（四元组） ──
  (deftest test-direct-impl-policy
    (let [policy (core/direct-impl-policy)
          sig ['getTime 'getTime ['this] 'long]
          cls java.util.Date]
      (is (= ['getTime 'getTime ['this] 'long {:delegate 'getTime}] (policy sig cls)))))

  (deftest test-delegate-impl-policy
    (let [policy (core/delegate-impl-policy [['cal-get-time 'getTime 'getTime]])
          sig ['cal-get-time 'getTime ['this] 'long]
          cls java.util.Calendar]
      (is (= ['cal-get-time 'getTime ['this] 'long {:delegate {:getter 'getTime :method 'getTime}}]
             (policy sig cls)))))

  (deftest test-custom-impl-policy
    (let [policy (core/custom-impl-policy [['epoch-seconds 1 'my-epoch-impl]])
          sig ['epoch-seconds 'epoch-seconds ['this] 'long]
          cls java.util.Date]
      (is (= ['epoch-seconds 'epoch-seconds ['this] 'long {:custom 'my-epoch-impl}] (policy sig cls)))))

  (deftest test-merge-impl-policies
    (let [direct (core/direct-impl-policy)
          delegate (core/delegate-impl-policy [['cal-get-time 'getTime 'getTime]])
          custom (core/custom-impl-policy [['cal-get-time 1 'override-impl]])
          merged (core/merge-impl-policies custom delegate direct)
          sig ['cal-get-time 'getTime ['this] 'long]
          cls java.util.Calendar]
      (is (= {:custom 'override-impl} (nth (merged sig cls) 4)))))

  ;; ── impl-expr 测试 ──
  (deftest test-impl-expr-direct
    (let [expr (core/impl-expr {:delegate 'getTime} 'this '[])]
      (is (= '. (first expr)))
      (is (= 'this (second expr)))
      (is (= 'getTime (nth expr 2)))))

  (deftest test-impl-expr-indirect
    (let [expr (core/impl-expr {:delegate {:getter 'getCalendar :method 'getTime}} 'this '[x])]
      (is (= 'clojure.core/let (first expr)))
      (let [bind-vec (second expr)
            bind-sym (first bind-vec)
            bind-expr (second bind-vec)]
        (is (symbol? bind-sym))
        (is (str/starts-with? (name bind-sym) "obj"))
        (is (= '. (first bind-expr)))
        (is (= 'this (second bind-expr)))
        (is (= 'getCalendar (nth bind-expr 2)))
        (let [body (nth expr 2)]
          (is (= '. (first body)))
          (is (= 'getTime (nth body 2)))
          (is (= 'x (nth body 3)))))))

  (deftest test-impl-expr-custom
    (let [expr (core/impl-expr {:custom 'my-fn} 'this '[a b])]
      (is (= 'my-fn (first expr)))
      (is (= 'this (second expr)))
      (is (= 'a (nth expr 2)))
      (is (= 'b (nth expr 3)))))

  ;; ── wrap-expr 测试 ──
  (deftest test-wrap-expr-empty
    (let [wrapped (core/wrap-expr [] '(+ 1 2) 'this '[])]
      (is (= '(+ 1 2) wrapped))))

  (deftest test-wrap-expr-single
    (let [wrapped (core/wrap-expr ['log-call] '(. this getTime) 'this '[])]
      (is (= 'log-call (first wrapped)))
      (let [inner-fn (second wrapped)]
        (is (= 'clojure.core/fn (first inner-fn)))
        (is (= ['this] (second inner-fn)))
        (is (= 'getTime (nth (nth inner-fn 2) 2))))))

  (deftest test-wrap-expr-multiple
    (let [wrapped (core/wrap-expr ['cache 'log-call] '(. this getTime) 'this '[x])]
      (is (= 'log-call (first wrapped)))
      (let [outer-fn (second wrapped)]
        (is (= 'clojure.core/fn (first outer-fn)))
        (is (= ['this 'x] (second outer-fn)))
        (let [outer-body (nth outer-fn 2)]
          (is (= 'cache (first outer-body)))
          (let [inner-fn (second outer-body)]
            (is (= 'clojure.core/fn (first inner-fn)))
            (is (= ['this 'x] (second inner-fn)))
            (is (= 'getTime (nth (nth inner-fn 2) 2))))))))

  ;; ── emit-extend-type 测试 ──
  (deftest test-emit-extend-type-basic
    (let [form (core/emit-extend-type
                 {::spec/type-name 'java.util.Date
                  ::spec/method-sigs [['get-time 'getTime ['this] 'long {:delegate 'getTime} []]]}
                 {::spec/protocol-name 'ITime
                  ::spec/protocol-method-sigs [['get-time ['this]]]})]
      (is (sequential? form))
      (is (= 'clojure.core/extend-type (first form)))
      (is (= 'java.util.Date (second form)))
      (is (= 'ITime (nth form 2)))
      (let [clause (nth form 3)]
        (is (= 'get-time (first clause)))
        (is (= '[this] (second clause)))
        (is (= 'getTime (nth (nth clause 2) 2))))))

  (deftest test-emit-extend-type-indirect
    (let [form (core/emit-extend-type
                 {::spec/type-name 'java.util.Calendar
                  ::spec/method-sigs [['cal-get-time 'getTime ['this] 'long
                                       {:delegate {:getter 'getTime :method 'getTime}} []]]}
                 {::spec/protocol-name 'ICalTime
                  ::spec/protocol-method-sigs [['cal-get-time ['this]]]})]
      (is (sequential? form))
      (let [body (nth (nth form 3) 2)]
        (is (= 'clojure.core/let (first body)))
        (is (symbol? (first (second body)))))))

  (deftest test-emit-extend-type-custom
    (let [form (core/emit-extend-type
                 {::spec/type-name 'java.util.Date
                  ::spec/method-sigs [['epoch-seconds 'epoch-seconds ['this] 'long {:custom 'my-impl} []]]}
                 {::spec/protocol-name 'IEpoch
                  ::spec/protocol-method-sigs [['epoch-seconds ['this]]]})]
      (is (sequential? form))
      (let [body (nth (nth form 3) 2)]
        (is (= 'my-impl (first body))))))

  (deftest test-emit-extend-type-with-wrapper
    (let [form (core/emit-extend-type
                 {::spec/type-name 'java.util.Date
                  ::spec/method-sigs [['get-time 'getTime ['this] 'long {:delegate 'getTime} ['log-call]]]}
                 {::spec/protocol-name 'ITime
                  ::spec/protocol-method-sigs [['get-time ['this]]]})]
      (is (sequential? form))
      (let [body (nth (nth form 3) 2)]
        (is (= 'log-call (first body))))))

  (deftest test-emit-extend-type-multiple-methods
    (let [form (core/emit-extend-type
                 {::spec/type-name 'java.util.Date
                  ::spec/method-sigs [['get-time 'getTime ['this] 'long {:delegate 'getTime} []]
                                      ['set-time 'setTime ['this 'long] 'void {:delegate 'setTime} []]]}
                 {::spec/protocol-name 'ITimeFull
                  ::spec/protocol-method-sigs [['get-time ['this]] ['set-time ['this 'arg1]]]})]
      (is (sequential? form))
      (is (= 'get-time (first (nth form 3))))
      (is (= 'set-time (first (nth form 4))))))

  ;; ── inject-impl-in-type-def 测试 ──
  (deftest test-inject-impl-in-type-def
    (let [type-def (core/build-type-def 'java.util.Date :only ['getTime])
          policy (core/direct-impl-policy)
          result (core/inject-impl-in-type-def type-def policy)]
      (is (seq (::spec/method-sigs result)))
      (let [sig (first (::spec/method-sigs result))]
        (is (= 'getTime (first sig)))
        (is (= {:delegate 'getTime} (nth sig 4)))))
    (testing "策略失败应抛出异常"
      (let [type-def {::spec/type-name 'java.util.Date
                      ::spec/method-sigs [['nonexistent 'nonexistent ['this] 'void]]}
            policy (core/direct-impl-policy)]
        (is (thrown? ExceptionInfo
                     (core/inject-impl-in-type-def type-def policy))))))

  ;; ── wrap-impl-in-type-def 测试 ──
  (deftest test-wrap-impl-in-type-def
    (let [td {::spec/method-sigs [['get-time 'getTime ['this] 'long {:delegate 'getTime}]]}
          result (core/wrap-impl-in-type-def td {:global ['log-call]})]
      (is (= ['log-call] (last (first (::spec/method-sigs result)))))))