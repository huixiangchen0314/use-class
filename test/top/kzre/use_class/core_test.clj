(ns top.kzre.use-class.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [top.kzre.use-class.core :as core]
            [top.kzre.use-class.spec :as spec]))

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
                 (set (map first (::spec/method-sigs res)))))))))

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

  ;; ── impl inject ──
  (deftest test-inject-impl-in-type-def
    (let [date-td (core/resolve-type 'java.util.Date)
          only-two (assoc date-td
                     ::spec/method-sigs
                     (filterv #(#{'getTime 'setTime} (first %))
                              (::spec/method-sigs date-td)))
          find-sig (fn [sigs name] (first (filter #(= name (first %)) sigs)))]

      (testing "direct fallback with default policy"
        (let [result (core/inject-impl-in-type-def only-two core/default-impl-policy)
              sigs (::spec/method-sigs result)]
          (is (= {:delegate 'getTime} (nth (find-sig sigs 'getTime) 3)))
          (is (= {:delegate 'setTime} (nth (find-sig sigs 'setTime) 3)))))

      (testing "custom overrides direct"
        (let [policy (core/merge-impl-policies
                       (core/custom-impl-policy [['getTime 1 'my-get-time]])
                       (core/direct-impl-policy))
              result (core/inject-impl-in-type-def only-two policy)
              sigs (::spec/method-sigs result)]
          (is (= {:custom 'my-get-time} (nth (find-sig sigs 'getTime) 3)))
          (is (= {:delegate 'setTime} (nth (find-sig sigs 'setTime) 3)))))

      (testing "explicit delegate mapping"
        (let [policy (core/merge-impl-policies
                       (core/delegate-impl-policy [['getTime 'getCalendar 'getTime]])
                       (core/direct-impl-policy))
              result (core/inject-impl-in-type-def only-two policy)
              sigs (::spec/method-sigs result)]
          (is (= {:delegate {:getter 'getCalendar :method 'getTime}} (nth (find-sig sigs 'getTime) 3)))
          (is (= {:delegate 'setTime} (nth (find-sig sigs 'setTime) 3)))))

      (testing "smart delegate finds getter for missing method"
        (let [cal-td (-> (core/resolve-type 'java.util.Calendar)
                         (update ::spec/method-sigs conj '[getTime [this] long]))  ;; 使用原始名称
              policy (core/merge-impl-policies
                       (core/direct-impl-policy)
                       (core/smart-delegate-policy))
              result (core/inject-impl-in-type-def cal-td policy)
              sigs (::spec/method-sigs result)
              get-time-sig (first (filter #(= 'getTime (first %)) sigs))]  ;; 查找原始名称
          (is get-time-sig "Should find a mapping for getTime")
          (is (:delegate (nth get-time-sig 3)))))))

  ;; ── wrap ──
  (deftest test-wrap-impl-in-type-def
    (let [date-td (core/resolve-type 'java.util.Date)
          injected (core/inject-impl-in-type-def
                     date-td
                     (core/merge-impl-policies (core/direct-impl-policy)))
          two-methods (assoc injected
                        ::spec/method-sigs
                        (filterv #(#{'getTime 'setTime} (first %))
                                 (::spec/method-sigs injected)))]

      (testing "全局包装器被应用到所有方法"
        (let [wrapped (core/wrap-impl-in-type-def
                        two-methods
                        {:global ['wrap-log]})
              sigs (::spec/method-sigs wrapped)]
          (is (every? #(= ['wrap-log] (last %)) sigs))))

      (testing "按方法包装器"
        (let [wrapped (core/wrap-impl-in-type-def
                        two-methods
                        {:methods {'getTime ['wrap-cache]}})
              sigs (::spec/method-sigs wrapped)
              getTime-sig (first (filter #(= 'getTime (first %)) sigs))
              setTime-sig (first (filter #(= 'setTime (first %)) sigs))]
          (is (= ['wrap-cache] (last getTime-sig)))
          (is (empty? (last setTime-sig)))))

      (testing "全局 + 方法包装器合并（全局在前）"
        (let [wrapped (core/wrap-impl-in-type-def
                        two-methods
                        {:global ['global-wrapper]
                         :methods {'getTime ['method-wrapper]}})
              sigs (::spec/method-sigs wrapped)
              getTime-sig (first (filter #(= 'getTime (first %)) sigs))]
          (is (= ['global-wrapper 'method-wrapper] (last getTime-sig))))))))

(deftest test-type-def->protocol-def-conflict
  (testing "正常无冲突"
    (let [td {::spec/type-name 'java.util.Date
              ::spec/method-sigs [['get-time ['this] 'long]
                                  ['set-time ['this 'long] 'void]]}
          pd (core/type-def->protocol-def td 'ITest)]
      (is (= 'ITest (::spec/protocol-name pd)))
      (is (= 2 (count (::spec/protocol-method-sigs pd))))))

  (testing "名称冲突抛出异常"
    (let [td {::spec/type-name 'java.util.Date
              ::spec/method-sigs [['get-time ['this] 'long]
                                  ['get-time ['this 'int] 'int]]}]
      (is (thrown? clojure.lang.ExceptionInfo
                   (core/type-def->protocol-def td 'ITest)))
      (try
        (core/type-def->protocol-def td 'ITest)
        (catch clojure.lang.ExceptionInfo e
          (is (str/includes? (.getMessage e) "get-time"))
          (is (str/includes? (.getMessage e) "rename")))))))