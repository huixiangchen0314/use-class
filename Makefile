.PHONY: clean test jar install repl run all

# 一键全流程：清理 → 测试 → 打包
all: clean test jar


# 清理构建产物
clean:
	clj -T:build clean

# 运行所有测试
test:
	clj -M:test -e "(require 'top.kzre.use-class.core-test) (clojure.test/run-tests 'top.kzre.use-class.core-test)"
	clj -M:test -e "(require 'top.kzre.use-class.wrapper-clean-test) (clojure.test/run-tests 'top.kzre.use-class.wrapper-clean-test)"
	clj -M:test -e "(require 'top.kzre.use-class.optional-wrapper-test) (clojure.test/run-tests 'top.kzre.use-class.optional-wrapper-test)"
clj -M:test -e "(require 'top.kzre.use-class.prefix-test) (clojure.test/run-tests 'top.kzre.use-class.prefix-test)
# 构建库 JAR
jar:
	clj -T:build jar

# 本地安装 JAR 到 Maven 仓库
install: jar
	mvn install:install-file -Dfile=target/use-class-0.1.0.jar -DpomFile=pom.xml

# 启动开发 REPL
repl:
	clj -M:dev

# 运行主程序（库模式通常为 REPL 或示例）
run:
	clj -M:dev -m use-class.core

