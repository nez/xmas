JAR := target/xmas.jar
NATIVE := xmas-native

.PHONY: run dev jar native clean

dev:
	clojure -M:run

run: jar
	java -jar $(JAR)

jar:
	clojure -X:uberjar

native: jar
	native-image -jar $(JAR) --no-fallback --initialize-at-build-time -o $(NATIVE)

clean:
	rm -rf target .cpcache $(NATIVE)
