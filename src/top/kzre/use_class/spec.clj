(ns top.kzre.use-class.spec
  "领域规格：描述 Clojure 协议、Java 实现及其映射关系。"
  (:require [clojure.spec.alpha :as s]))

;; ── 基础符号 ──
;; 在 spec.clj 中添加
(s/def ::protocol-name symbol?)
(s/def ::protocol-method-sig (s/tuple ::method-name ::params))   ;; [method-name [this ...]]
(s/def ::protocol-method-sigs (s/coll-of ::protocol-method-sig :kind vector? :min-count 1))
(s/def ::protocol-def (s/keys :req [::protocol-name ::protocol-method-sigs]))

(s/def ::type-name     symbol?)       ; Java 类型（类/接口）符号
(s/def ::method-name   symbol?)
(s/def ::java-method   symbol?)
(s/def ::getter        symbol?)
(s/def ::custom-fn     symbol?)       ; 自定义实现函数（Var 符号）

;; ── 方法参数与返回 ──
(s/def ::params    (s/coll-of ::type-name :kind vector?))
(s/def ::return    ::type-name)

;; ── 实现映射 ──
(s/def ::delegate-direct   symbol?)
(s/def ::delegate-map      (s/keys :req [::getter ::method]))
(s/def ::delegate-spec     (s/or :direct ::delegate-direct
                                 :indirect ::delegate-map))
(s/def ::impl-delegate     (s/keys :req [::delegate]))   ; {:delegate <delegate-spec>}
(s/def ::impl-custom       (s/keys :req [::custom-fn]))  ; {:custom <symbol>}
(s/def ::impl-map          (s/or :delegate ::impl-delegate
                                 :custom   ::impl-custom))

;; ── 包装器 ──
(s/def ::wrappers (s/coll-of symbol? :kind vector?))

;; ── 方法签名（五元组） ──
(s/def ::method-sig
  (s/cat :name     ::method-name
         :params   ::params
         :return   ::return
         :impl     ::impl-map
         :wrappers ::wrappers))

;; ── 方法签名集合 ──
(s/def ::method-sigs (s/coll-of ::method-sig :kind vector? :min-count 1))

;; ── 类型定义 ──
(s/def ::type-def
  (s/keys :req [::type-name
                ::method-sigs]))

;; ── 可选配置 ──
(s/def ::include          (s/coll-of symbol?))
(s/def ::exclude          (s/coll-of symbol?))
(s/def ::default-include  boolean?)
(s/def ::rename-map       (s/map-of symbol? symbol?))
(s/def ::rename-fn        fn?)
(s/def ::danger-set       (s/coll-of symbol? :kind set?))
(s/def ::setter-danger?   boolean?)
(s/def ::delegate-entry   (s/or :two (s/tuple symbol? symbol?)
                                :three (s/tuple symbol? symbol? symbol?)))
(s/def ::delegate-config  (s/coll-of ::delegate-entry :kind vector?))
(s/def ::custom-entry     (s/tuple symbol? int? symbol?))
(s/def ::custom-config    (s/coll-of ::custom-entry :kind vector?))
(s/def ::filter fn?)   ;; 自定义过滤谓词，接收方法名符号，返回布尔值
;; ── 包装器配置 ──
(s/def ::global-wrappers (s/coll-of symbol? :kind vector?))
(s/def ::method-wrappers (s/map-of symbol? (s/coll-of symbol? :kind vector?)))
(s/def ::wrappers-config (s/keys :opt-un [::global-wrappers ::method-wrappers]))