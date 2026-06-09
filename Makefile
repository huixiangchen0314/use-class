# ── 项目配置 ──
VERSION   := 0.1.0
JAR_FILE  := target/use-class-$(VERSION).jar

.PHONY: clean test jar install repl run all

# 一键全流程：清理 → 测试 → 打包
all: clean test jar

# 清理构建产物（交给 tools.build）
clean:
	clj -T:build clean

# 编译测试用 Java 源文件
compile-test-java:
	javac -source 8 -target 8 -d target/test-classes test/top/kzre/use_class/VarargsDemo.java

# 运行所有测试
test:
	clj -M:test -e "(require 'top.kzre.use-class.core-test) (clojure.test/run-tests 'top.kzre.use-class.core-test)"
	clj -M:test -e "(require 'top.kzre.use-class.wrapper-clean-test) (clojure.test/run-tests 'top.kzre.use-class.wrapper-clean-test)"
	clj -M:test -e "(require 'top.kzre.use-class.optional-wrapper-test) (clojure.test/run-tests 'top.kzre.use-class.optional-wrapper-test)"
	clj -M:test -e "(require 'top.kzre.use-class.prefix-test) (clojure.test/run-tests 'top.kzre.use-class.prefix-test)"
	clj -M:test -e "(require 'top.kzre.use-class.overload-test) (clojure.test/run-tests 'top.kzre.use-class.overload-test)"
	clj -M:test -e "(require 'top.kzre.use-class.varargs-test) (clojure.test/run-tests 'top.kzre.use-class.varargs-test)"

# 构建 JAR（直接调用 tools.build，由它判断文件是否变化）
jar:
	clj -T:build jar

# 本地安装 JAR 到本地 Maven 仓库
install: jar
	mvn install:install-file -Dfile=$(JAR_FILE) -DpomFile=pom.xml

# 启动开发 REPL
repl:
	clj -M:dev

# 运行主程序（示例）
run:
	clj -M:dev -m use-class.core