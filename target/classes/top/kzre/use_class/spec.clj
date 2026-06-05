(ns top.kzre.use-class.spec
  "use-class 库的 clojure.spec 定义"
  (:require [clojure.spec.alpha :as s]))

;; 基础类型
(s/def ::java-symbol (s/and symbol? #(try (resolve %) (catch Exception _ false))))
(s/def ::interface ::java-symbol)
(s/def ::protocol-name symbol?)
(s/def ::method-name symbol?)
(s/def ::method-names (s/coll-of ::method-name :kind vector? :min-count 1))

;; 选项
(s/def ::rename (s/map-of ::method-name ::method-name))
(s/def ::custom (s/map-of symbol? fn?))
(s/def ::name-mapper (s/and symbol? #(try (let [v (resolve %)] (if-let [f (deref v)] (fn? f) false)) (catch Exception _ false))))
(s/def ::dangerous (s/coll-of ::method-name :kind set?))
(s/def ::via-entry (s/or :two (s/tuple ::method-name ::method-name) :three (s/tuple ::method-name ::method-name ::method-name)))
(s/def ::via (s/coll-of ::via-entry :kind vector?))

;; 组合选项（直接使用完全限定 key，无 :opt-un 约束）
(s/def ::adapter-options (s/keys :opt [::except ::only ::rename ::custom ::dangerous ::name-mapper ::via]))
(s/def ::except ::method-names)
(s/def ::only ::method-names)