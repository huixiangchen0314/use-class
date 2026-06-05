(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'top.kzre/use-class)             ; 库名
(def version "0.1.0")
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def jar-file (format "target/%s-%s.jar" (name lib) version))
(def uber-file (format "target/%s-%s-standalone.jar" (name lib) version))

(defn clean [_]
      (b/delete {:path "target"}))

(defn compile-java [_]
      (b/javac {:src-dirs ["src"]
                :class-dir class-dir
                :basis basis
                :javac-opts ["-source" "8" "-target" "8"]}))

(defn jar [_]
      (clean nil)
      (b/write-pom {:class-dir class-dir
                    :lib lib
                    :version version
                    :basis basis
                    :src-dirs ["src"]
                    :scm {:url "https://github.com/huixinagchen0314/use-class"
                          :connection "scm:git:git://github.com/huixinagchen0314/use-class.git"
                          :developerConnection "scm:git:ssh://git@github.com:huixinagchen0314/use-class.git"}})
      (b/copy-dir {:src-dirs ["src" "resources"]
                   :target-dir class-dir})
      (b/jar {:class-dir class-dir
              :jar-file jar-file}))

(defn uberjar [_]
      (clean nil)
      (b/compile-clj {:basis basis
                      :src-dirs ["src"]
                      :class-dir class-dir
                      :ns-compile ['top.kzre.use-class.corex
                                   'top.kzre.use-class.spec]})
      (b/uber {:class-dir class-dir
               :uber-file uber-file
               :basis basis
               :main 'top.kzre.use-class.corex}))