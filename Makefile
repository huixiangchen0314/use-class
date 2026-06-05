.PHONY: clean test jar uberjar

clean:
	clj -T:build clean

test:
	clj -M:test -e "(require 'top.kzre.use-class.core-test) (clojure.test/run-tests 'top.kzre.use-class.core-test "

jar:
	clj -T:build jar

uberjar:
	clj -T:build uberjar

repl:
	clj -M:dev

run:
	clj -M:dev -m use-class.core

install: jar
	mvn install:install-file -Dfile=target/use-class-0.1.0.jar -DpomFile=pom.xml

all: clean test uberjar