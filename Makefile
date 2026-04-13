JAR := target/xmas.jar
NATIVE := xmas-native

.PHONY: dev run jar native repl test lint check clean setup

setup:
	./install.sh

dev:
	clojure -M:run

repl:
	clojure -M -e '(require (quote xmas.ed)) (clojure.main/repl)'

run: jar
	java -jar $(JAR)

jar:
	clojure -X:uberjar

native: jar
	native-image -jar $(JAR) --no-fallback --initialize-at-build-time -o $(NATIVE)

test:
	clojure -M:test

lint:
	clj-kondo --lint src

check: lint
	clojure -M -e '(require (quote xmas.ed)) (println "ok")'

clean:
	rm -rf target .cpcache .nrepl-port $(NATIVE)
