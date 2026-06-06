(ns top.kzre.use-class.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [top.kzre.use-class.core :as core]
            [top.kzre.use-class.spec :as spec])
  (:import (clojure.lang ExceptionInfo)))


;; ── resolve-type ──
(deftest test-resolve-type-date
  (let [type-def (core/resolve-type 'java.util.Date)]
    (is (map? type-def))
    (is (= 'java.util.Date (::spec/type-name type-def)))
    (is (vector? (::spec/method-sigs type-def)))
    (let [sigs (::spec/method-sigs type-def)]
      (is (seq sigs) "应该有至少一个方法")
      (doseq [sig sigs]
        (is (vector? sig))
        (is (= 3 (count sig)))
        (let [[method-name params return] sig]
          (is (symbol? method-name))
          (is (vector? params))
          (is (symbol? return))
          (is (= 'this (first params)) "所有协议方法第一个参数应为 this")))
      (let [method-names (into #{} (map first sigs))]
        (doseq [object-method '[equals hashCode toString getClass notify notifyAll wait]]
          (is (not (contains? method-names object-method))
              (str "不应包含 Object 方法: " object-method))))
      (let [get-time-sig (first (filter #(-> % first name (= "getTime")) sigs))]
        (is get-time-sig "应存在 getTime 方法")
        (is (= '[this] (second get-time-sig)))
        (is (= 'long (nth get-time-sig 2)))))))

;; ── rename ──
(deftest test-rename-methods-in-type-def
  (let [date-td (core/resolve-type 'java.util.Date)
        orig-sigs (::spec/method-sigs date-td)]
    (testing "使用 rename-map 直接覆盖"
      (let [renamed (core/rename-methods-in-type-def date-td {'getTime 'fetch-time} nil)
            renamed-names (map first (::spec/method-sigs renamed))]
        (is (some #(= 'fetch-time %) renamed-names))
        (is (not (some #(= 'getTime %) renamed-names)))))
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
            renamed-names (map first (::spec/method-sigs renamed))]
        (is (some #(= 'time %) renamed-names))
        (is (not (some #(= 'getTime %) renamed-names)))))
    (testing "映射优先于函数（未映射的方法使用函数命名）"
      (let [renamed (core/rename-methods-in-type-def date-td
                                                     {'getTime 'my-get-time}
                                                     (fn [_] 'should-not-be-used))
            renamed-names (map first (::spec/method-sigs renamed))]
        (is (some #(= 'my-get-time %) renamed-names) "getTime 应被映射为 my-get-time")
        (is (not (some #(= 'getTime %) renamed-names)) "原始 getTime 不应出现")
        (is (some #(= 'should-not-be-used %) renamed-names) "未映射的方法应使用 rename-fn 重命名")
        (is (every? #(or (= 'my-get-time %) (= 'should-not-be-used %)) renamed-names)
            "所有方法名要么是 my-get-time（映射覆盖），要么是 should-not-be-used（函数生成）")))
    (testing "无映射且无函数时保持原名"
      (let [renamed (core/rename-methods-in-type-def date-td {} nil)]
        (is (= (set (map first orig-sigs))
               (set (map first (::spec/method-sigs renamed)))))))))

;; ── filter ──
(deftest test-filter-methods-in-type-def
  (let [date-td (core/resolve-type 'java.util.Date)
        all-names (map first (::spec/method-sigs date-td))]
    (testing "default-include true 且无 exclude → 保留全部"
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
      (testing "default-include false 时 ignore exclude（设计决策）"
        (let [res (core/filter-methods-in-type-def date-td false '[getTime] '[setTime])]
          (is (= #{'getTime}
                 (set (map first (::spec/method-sigs res))))))))))

  ;; ── danger ──
  (deftest test-mark-dangerous-in-type-def
    (let [date-td (core/resolve-type 'java.util.Date)
          renamed (core/rename-methods-in-type-def date-td
                                                   {'getTime 'get-time 'setTime 'set-time} nil)
          renamed-names (set (map first (::spec/method-sigs renamed)))]
      (testing "指定方法添加 ! （关闭自动 setter 标记）"
        (let [danger-set #{'get-time 'set-time}
              result (core/mark-dangerous-in-type-def renamed danger-set :setter-danger? false)
              result-names (map first (::spec/method-sigs result))]
          (is (some #(= 'get-time! %) result-names))
          (is (some #(= 'set-time! %) result-names))
          (is (not (some #(= 'get-time %) result-names)))
          (is (not (some #(= 'set-time %) result-names)))))
      (testing "已带 ! 的方法不会重复添加 !"
        (let [td (assoc date-td ::spec/method-sigs [['already! ['this] 'void]])
              danger-set #{'already!}
              result (core/mark-dangerous-in-type-def td danger-set :setter-danger? false)
              result-names (map first (::spec/method-sigs result))]
          (is (= 'already! (first result-names)))
          (is (not= 'already!! (first result-names)))))
      (testing "非危险方法不变（关闭自动 setter）"
        (let [danger-set #{'get-time}
              result (core/mark-dangerous-in-type-def renamed danger-set :setter-danger? false)
              result-names (map first (::spec/method-sigs result))
              safe-method (first (remove (fn [n] (or (= 'get-time n) (str/starts-with? (name n) "set-"))) renamed-names))]
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



;; ── 基本解析 ──
(deftest test-build-type-def-basic
  (let [type-def (core/build-type-def 'java.util.Date)]
    (is (= 'java.util.Date (::spec/type-name type-def)))
    (is (vector? (::spec/method-sigs type-def)))
    (is (seq (::spec/method-sigs type-def)))
    ;; 不应包含 Object 方法
    (doseq [sig (::spec/method-sigs type-def)]
      (is (not (contains? #{'equals 'hashCode 'toString 'getClass 'notify 'notifyAll 'wait}
                          (first sig)))))))

;; ── 委托单元素 ──
(deftest test-build-type-def-with-delegate-single
  ;; 委托 Calendar 的 getTime 返回的 Date 类型上的所有方法，使用引号
  (let [type-def (core/build-type-def 'java.util.Calendar :delegate [['getTime]])]
    (is (contains? (set (map first (::spec/method-sigs type-def))) 'getTime))
    (is (contains? (set (map first (::spec/method-sigs type-def))) 'getMonth))
    ;; 确保原始方法仍然存在
    (is (contains? (set (map first (::spec/method-sigs type-def))) 'getCalendarType))))


;; ── 重命名 ──
(deftest test-build-type-def-with-rename
  (let [type-def (core/build-type-def 'java.util.Date :rename {'getTime 'fetch-time})]
    (is (contains? (set (map first (::spec/method-sigs type-def))) 'fetch-time))
    (is (not (contains? (set (map first (::spec/method-sigs type-def))) 'getTime)))))


;; ── 过滤 only ──
(deftest test-build-type-def-with-only
  (let [type-def (core/build-type-def 'java.util.Date :only ['getTime 'setTime])]
    (is (= 2 (count (::spec/method-sigs type-def))))
    (is (= #{'getTime 'setTime} (set (map first (::spec/method-sigs type-def)))))))

;; ── 过滤 except ──
(deftest test-build-type-def-with-except
  (let [type-def (core/build-type-def 'java.util.Date :except ['getTime])]
    (is (not (contains? (set (map first (::spec/method-sigs type-def))) 'getTime)))
    (is (contains? (set (map first (::spec/method-sigs type-def))) 'setTime))))

;; ── 危险标记 ──
(deftest test-build-type-def-with-dangerous
  (let [type-def (core/build-type-def 'java.util.Date
                                      :rename {'getTime 'get-time 'setTime 'set-time}
                                      :only ['get-time 'set-time]
                                      :dangerous #{'set-time})]
    (is (contains? (set (map first (::spec/method-sigs type-def))) 'get-time))
    (is (contains? (set (map first (::spec/method-sigs type-def))) 'set-time!))
    (is (not (contains? (set (map first (::spec/method-sigs type-def))) 'set-time)))))


;; ── 综合测试：委托 + 重命名 + 过滤 + 危险 ──
(deftest test-build-type-def-combined
  (let [type-def (core/build-type-def 'java.util.Calendar
                                      :delegate [['cal-get-time 'getTime 'getTime]]   ; 三元素
                                      :rename {'getTime 'time-accessor}
                                      :only ['time-accessor 'cal-get-time]
                                      :dangerous #{'time-accessor})]
    (let [sigs (::spec/method-sigs type-def)
          names (map first sigs)]
      (is (= 2 (count sigs)))
      (is (contains? (set names) 'time-accessor!))
      (is (contains? (set names) 'cal-get-time)))))


;; ── 冲突行为：重复方法名保留第一个（即宿主方法） ──
(deftest test-build-type-def-conflict
  ;; 委托的 getTime 与宿主 Calendar 已有 getTime 同名，应保留宿主的方法签名
  (let [type-def (core/build-type-def 'java.util.Calendar
                                      :delegate [['getTime 'getTime 'getTime]]
                                      :rename {})]
    (let [sigs (::spec/method-sigs type-def)
          names (map first sigs)
          get-time-sigs (filter #(= (first %) 'getTime) sigs)]
      ;; 只有一个 getTime，且来自宿主（参数列表为 [this]）
      (is (= 1 (count get-time-sigs)))
      (is (= '[this] (second (first get-time-sigs)))))))


(deftest test-emit-defprotocol
  (let [proto-def {::spec/protocol-name 'ITest
                   ::spec/protocol-method-sigs [['get-time ['this]] ['set-time ['this 'long]]]}
        form (core/emit-defprotocol proto-def)]
    (is (= 'clojure.core/defprotocol (first form)))
    (is (= 'ITest (second form)))
    (is (= 2 (count (nth form 2))))))





;; ── 实现注入策略测试 ──
(deftest test-direct-impl-policy
  (let [policy (core/direct-impl-policy)
        sig ['getTime ['this] 'long]
        cls java.util.Date]
    (is (= ['getTime ['this] 'long {:delegate 'getTime}] (policy sig cls))))
  (let [policy (core/direct-impl-policy :rename-inverse {'fetch-time 'getTime})
        sig ['fetch-time ['this] 'long]
        cls java.util.Date]
    (is (= ['fetch-time ['this] 'long {:delegate 'getTime}] (policy sig cls)))))

(deftest test-delegate-impl-policy
  (let [policy (core/delegate-impl-policy [['cal-get-time 'getTime 'getTime]])
        sig ['cal-get-time ['this] 'long]
        cls java.util.Calendar]
    (is (= ['cal-get-time ['this] 'long {:delegate {:getter 'getTime :method 'getTime}}] (policy sig cls))))
  ;; 未配置的方法返回 nil
  (let [policy (core/delegate-impl-policy [['other 'getTime]])
        sig ['getTime ['this] 'long]]
    (is (nil? (policy sig java.util.Date)))))

(deftest test-custom-impl-policy
  (let [policy (core/custom-impl-policy [['epoch-seconds 1 'my-epoch-impl]])
        sig ['epoch-seconds ['this] 'long]
        cls java.util.Date]
    (is (= ['epoch-seconds ['this] 'long {:custom 'my-epoch-impl}] (policy sig cls))))
  (let [policy (core/custom-impl-policy [])
        sig ['getTime ['this] 'long]]
    (is (nil? (policy sig java.util.Date)))))

(deftest test-merge-impl-policies
  (let [direct (core/direct-impl-policy)
        delegate (core/delegate-impl-policy [['cal-get-time 'getTime 'getTime]])
        custom (core/custom-impl-policy [['cal-get-time 1 'override-impl]])
        merged (core/merge-impl-policies custom delegate direct)
        sig ['cal-get-time ['this] 'long]
        cls java.util.Calendar]
    (is (= {:custom 'override-impl} (nth (merged sig cls) 3))))
  (let [direct (core/direct-impl-policy)
        delegate (core/delegate-impl-policy [['cal-get-time 'getTime 'getTime]])
        merged (core/merge-impl-policies delegate direct)
        sig ['cal-get-time ['this] 'long]
        cls java.util.Calendar]
    (is (= {:delegate {:getter 'getTime :method 'getTime}} (nth (merged sig cls) 3))))
  (let [direct (core/direct-impl-policy)
        merged (core/merge-impl-policies direct)
        sig ['getTime ['this] 'long]
        cls java.util.Date]
    (is (= {:delegate 'getTime} (nth (merged sig cls) 3)))))


;; ── impl-expr 测试 ──
(deftest test-impl-expr-direct
  (let [expr (core/impl-expr {:delegate 'getTime} 'this '[])]
    (is (= '. (first expr)) "应为 . 调用")
    (is (= 'this (second expr)) "第二个元素应为 this")
    (is (= 'getTime (nth expr 2)) "第三个元素应为 getTime")))



(deftest test-impl-expr-indirect
  (let [expr (core/impl-expr {:delegate {:getter 'getCalendar :method 'getTime}} 'this '[x])]
    (is (= 'clojure.core/let (first expr)) "应为 let 形式")
    ;; 绑定向量 [obj# (. this getCalendar)]
    (let [bind-vec (second expr)
          bind-sym (first bind-vec)
          bind-expr (second bind-vec)]
      (is (symbol? bind-sym) "绑定符号应是符号")
      (is (str/starts-with? (name bind-sym) "obj") "绑定符号应以 obj 开头")
      (is (= '. (first bind-expr)) "getter 调用应为 . 形式")
      (is (= 'this (second bind-expr)) "getter 调用目标为 this")
      (is (= 'getCalendar (nth bind-expr 2)) "getter 方法名为 getCalendar"))
    ;; let 体：(. obj# getTime x)
    (let [body (nth expr 2)]
      (is (= '. (first body)) "委托调用应为 . 形式")
      (is (= 'getTime (nth body 2)) "委托方法名为 getTime")
      (is (= 'x (nth body 3)) "参数为 x"))))

(deftest test-impl-expr-custom
  (let [expr (core/impl-expr {:custom 'my-fn} 'this '[a b])]
    (is (= 'my-fn (first expr)) "应为函数调用")
    (is (= 'this (second expr)) "第一个参数应为 this")
    (is (= 'a (nth expr 2)) "第二个参数应为 a")
    (is (= 'b (nth expr 3)) "第三个参数应为 b")))

(deftest test-wrap-expr-empty
  (let [wrapped (core/wrap-expr [] '(+ 1 2) 'this '[])]
    (is (= '(+ 1 2) wrapped) "无包装器时原样返回")))

(deftest test-wrap-expr-single
  (let [wrapped (core/wrap-expr ['log-call] '(. this getTime) 'this '[])]
    (is (= 'log-call (first wrapped)) "最外层是包装器")
    (let [inner-fn (second wrapped)]
      (is (= 'clojure.core/fn (first inner-fn)) "包装器参数是匿名函数")
      (is (= ['this] (second inner-fn)) "函数参数是 this")
      (is (= 'getTime (nth (nth inner-fn 2) 2)) "函数体内是原始调用"))))

(deftest test-wrap-expr-multiple
  (let [wrapped (core/wrap-expr ['cache 'log-call] '(. this getTime) 'this '[x])]
    ;; 最外层 log-call
    (is (= 'log-call (first wrapped)) "最外层是列表中最后的包装器")
    (let [outer-fn (second wrapped)]
      (is (= 'clojure.core/fn (first outer-fn)) "第二层是 fn")
      (is (= ['this 'x] (second outer-fn)) "函数参数是 this x")
      ;; 外层函数体是 (cache ...)
      (let [outer-body (nth outer-fn 2)]
        (is (= 'cache (first outer-body)) "第三层是 cache（列表中第一个包装器）")
        (let [inner-fn (second outer-body)]
          (is (= 'clojure.core/fn (first inner-fn)) "第四层是 fn")
          (is (= ['this 'x] (second inner-fn)) "内层函数参数也是 this x")
          ;; 内层函数体是原始表达式
          (is (= 'getTime (nth (nth inner-fn 2) 2)) "最内层是原始调用 (. this getTime)"))))))


(deftest test-emit-extend-type-basic
  (let [form (core/emit-extend-type
               {::spec/type-name 'java.util.Date
                ::spec/method-sigs [['get-time ['this] 'long {:delegate 'getTime} []]]}
               {::spec/protocol-name 'ITime
                ::spec/protocol-method-sigs [['get-time ['this]]]})]
    (is (sequential? form))
    (is (= 'clojure.core/extend-type (first form)))
    (is (= 'java.util.Date (second form)))
    (is (= 'ITime (nth form 2)))
    (let [clause (nth form 3)]          ;; 直接就是第一个方法子句
      (is (= 'get-time (first clause)))
      (is (= '[this] (second clause)))
      (let [body (nth clause 2)]
        (is (= 'getTime (nth body 2)))))))

(deftest test-emit-extend-type-indirect
  (let [form (core/emit-extend-type
               {::spec/type-name 'java.util.Calendar
                ::spec/method-sigs [['cal-get-time ['this] 'long
                                     {:delegate {:getter 'getTime :method 'getTime}} []]]}
               {::spec/protocol-name 'ICalTime
                ::spec/protocol-method-sigs [['cal-get-time ['this]]]})]
    (is (sequential? form))
    (let [clause (nth form 3)
          body (nth clause 2)]
      (is (= 'clojure.core/let (first body)))
      (is (symbol? (first (second body)))))))

(deftest test-emit-extend-type-custom
  (let [form (core/emit-extend-type
               {::spec/type-name 'java.util.Date
                ::spec/method-sigs [['epoch-seconds ['this] 'long {:custom 'my-impl} []]]}
               {::spec/protocol-name 'IEpoch
                ::spec/protocol-method-sigs [['epoch-seconds ['this]]]})]
    (is (sequential? form))
    (let [body (nth (nth form 3) 2)]
      (is (= 'my-impl (first body))))))

(deftest test-emit-extend-type-with-wrapper
  (let [form (core/emit-extend-type
               {::spec/type-name 'java.util.Date
                ::spec/method-sigs [['get-time ['this] 'long {:delegate 'getTime} ['log-call]]]}
               {::spec/protocol-name 'ITime
                ::spec/protocol-method-sigs [['get-time ['this]]]})]
    (is (sequential? form))
    (let [body (nth (nth form 3) 2)]
      (is (= 'log-call (first body))))))

(deftest test-emit-extend-type-multiple-methods
  (let [form (core/emit-extend-type
               {::spec/type-name 'java.util.Date
                ::spec/method-sigs [['get-time ['this] 'long {:delegate 'getTime} []]
                                    ['set-time ['this 'long] 'void {:delegate 'setTime} []]]}
               {::spec/protocol-name 'ITimeFull
                ::spec/protocol-method-sigs [['get-time ['this]] ['set-time ['this 'arg1]]]})]
    (is (sequential? form))
    (let [clause1 (nth form 3)
          clause2 (nth form 4)]
      (is (= 'get-time (first clause1)))
      (is (= 'set-time (first clause2))))))

;; ── inject-impl-in-type-def 测试 ──
(deftest test-inject-impl-in-type-def
  (let [type-def (core/build-type-def 'java.util.Date :only ['getTime])
        policy (core/direct-impl-policy)
        result (core/inject-impl-in-type-def type-def policy)]
    (is (seq (::spec/method-sigs result)))
    (let [sig (first (::spec/method-sigs result))]
      (is (= 'getTime (first sig)))
      (is (= {:delegate 'getTime} (nth sig 3)))))
  (testing "策略失败应抛出异常"
    (let [type-def {::spec/type-name 'java.util.Date
                    ::spec/method-sigs [['nonexistent ['this] 'void]]}
          policy (core/direct-impl-policy)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (core/inject-impl-in-type-def type-def policy))))))

;; ── wrap-impl-in-type-def 测试 ──
(deftest test-wrap-impl-in-type-def
  (let [td {::spec/method-sigs [['get-time ['this] 'long {:delegate 'getTime}]]}
        result (core/wrap-impl-in-type-def td {:global ['log-call]})]
    (is (= ['log-call] (last (first (::spec/method-sigs result)))))))