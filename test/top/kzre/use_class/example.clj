(ns top.kzre.use-class.example
  (:require [clojure.test :refer :all]))



(require '[top.kzre.use-class.core :as core] :reload)

;; 1. 直接委托
(core/use-class 'java.util.Date :protocol-name 'IDateUse :only ['getTime])
#_{:clj-kondo/ignore [:unresolved-symbol]}
(getTime (java.util.Date. 12345))

;; 2. 委托穿透
(core/use-class 'java.util.Calendar :protocol-name 'ICalUse
                :only ['cal-get-time]
                :delegate [['cal-get-time 'getTime 'getTime]])
(let [cal (doto (java.util.Calendar/getInstance) (.setTime (java.util.Date. 55555)))]
  #_{:clj-kondo/ignore [:unresolved-symbol]}
  (cal-get-time cal))

;; 3. 自定义实现
(defn my-epoch [this] (quot (.getTime ^java.util.Date this) 1000))
(core/use-class 'java.util.Date :protocol-name 'IEpochUse
                :only ['epoch-sec]
                :custom [['epoch-sec 1 'user/my-epoch]])
#_{:clj-kondo/ignore [:unresolved-symbol]}
(epoch-sec (java.util.Date. 5000))

;; 4. 包装器
(defn log-call [f]
  (fn [this & args]
    (println "Calling" args)
    (apply f this args)))
(core/use-class 'java.util.Date :protocol-name 'ILogUse
                :rename {'getTime 'log-get-time}
                :only ['log-get-time]
                :wrappers {:global ['user/log-call]})
#_{:clj-kondo/ignore [:unresolved-symbol]}
(log-get-time (java.util.Date. 9999))

;; 5. 单元素委托（自动过滤冲突）
(core/use-class 'java.util.Calendar :protocol-name 'ICalAll
                :delegate [['getTime]])
;; 检查协议方法，应包含 getMonth 等，且没有 compareTo 重复
(keys (:sigs @(resolve 'ICalAll)))