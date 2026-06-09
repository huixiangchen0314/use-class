(ns top.kzre.use-class.util
  (:require [clojure.string :as str]
            [clojure.walk :as walk])
  (:import (java.lang.reflect Method)))



(defn bean-name->kebab-name
  "将 Java 方法名符号转为 Clojure kebab-case 符号，保留 get/set/is 前缀。
   例如：getTime → get-time，setTime → set-time，isReady → is-ready。"
  [method-sym]
  (let [name   (name method-sym)
        [prefix body]
        (cond
          (re-find #"^get[A-Z]" name) ["get" (subs name 3)]
          (re-find #"^set[A-Z]" name) ["set" (subs name 3)]
          (re-find #"^is[A-Z]" name)  ["is" (subs name 2)]
          :else                        [nil name])
        hyphenated (-> body
                       (str/replace #"([A-Z])" #(str "-" (.toLowerCase (second %))))
                       (str/replace #"^-" ""))
        full (str (when prefix (str prefix "-")) hyphenated)]
    (symbol full)))

(defn bean-name->kebab-name-strip
  "将 Java 方法名符号转为 Clojure kebab-case 符号，并去除 get/set/is 前缀。
   例如：getTime → time，setTime → set-time，isReady → ready?。"
  [method-sym]
  (let [name   (name method-sym)
        [prefix body bang?]
        (cond
          (re-find #"^get[A-Z]" name) [nil (subs name 3) false]
          (re-find #"^set[A-Z]" name) ["set" (subs name 3) true]
          (re-find #"^is[A-Z]" name)  [nil (str (subs name 2) "?") false]
          :else                        [nil name false])
        hyphenated (-> body
                       (str/replace #"([A-Z])" #(str "-" (.toLowerCase (second %))))
                       (str/replace #"^-" ""))
        full (str (when prefix (str prefix "-")) hyphenated (when bang? "!"))]
    (symbol full)))


(defn ->sym
  [x]
  (cond
    (symbol? x) x
    (keyword? x) (symbol (name x))
    (string? x) (symbol x)
    (and (seq? x) (= (first x) 'quote) (= (count x) 2))
    (->sym (second x))
    :else (throw (IllegalArgumentException. (str "无法转换为符号: " x)))))

(defn ->fn
  "Resolves a wrapper descriptor (symbol, var, or function) into a callable function.
   Symbols are resolved in the current namespace and deref'd if they are Vars."
  [w]
  (cond
    (symbol? w) (some-> (ns-resolve *ns* w) deref)
    (var? w)    @w
    (fn? w)     w
    :else       nil))

(defn index-of
  "Returns the index of the first occurrence of x in coll, or -1 if not found."
  [coll x]
  (or (first (keep-indexed #(when (= x %2) %1) coll)) -1))

(defn- kebab->camel-safe
  [method-sym]
  (let [s (name method-sym)
        parts (str/split s #"-")]
    (if (= 1 (count parts))
      method-sym
      (let [prefix (first parts)
            rest-words (rest parts)]
        (if (#{"get" "set" "is"} prefix)
          (symbol (str prefix (apply str (map str/capitalize rest-words))))
          nil)))))

(defn unquote-form
  [form]
  (if (and (seq? form) (= (first form) 'quote) (= (count form) 2))
    (second form)
    form))

(defn deep-unquote
  [coll]
  (walk/postwalk unquote-form coll))


(defn setter-name? [sym]
  (let [s (name sym)]
    (re-find #"^set[A-Z]" s)))
