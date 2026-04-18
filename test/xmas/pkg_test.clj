(ns xmas.pkg-test
  (:require [clojure.test :refer [deftest is]]
            [xmas.buf :as buf]
            [xmas.el :as el]
            [xmas.pkg :as pkg])
  (:import [java.io File]))

(defn- tmp-home
  "Return [home-dir packages-dir] — fresh temp home for pkg resolution."
  []
  (let [h (doto (File/createTempFile "xmas-home-" "")
            (.delete)
            (.mkdir))
        p (File. h ".xmas/packages")]
    (.mkdirs p)
    [h p]))

(defn- with-home [home-dir f]
  (let [saved (System/getProperty "user.home")]
    (try
      (System/setProperty "user.home" (.getAbsolutePath ^File home-dir))
      (f)
      (finally (System/setProperty "user.home" saved)))))

(defn- rm-rf [^File f]
  (when (.exists f)
    (when (.isDirectory f)
      (doseq [c (.listFiles f)] (rm-rf c)))
    (.delete f)))

(deftest installed-empty-when-nothing
  (let [[h _] (tmp-home)]
    (try (with-home h #(is (empty? (pkg/installed))))
         (finally (rm-rf h)))))

(deftest installed-lists-dirs
  (let [[h p] (tmp-home)]
    (try
      (.mkdir (File. p "alpha"))
      (.mkdir (File. p "beta"))
      (with-home h #(is (= ["alpha" "beta"] (pkg/installed))))
      (finally (rm-rf h)))))

(deftest load-package-evaluates-el-file
  (let [[h p] (tmp-home)]
    (try
      (let [pkg-dir (File. p "greet")]
        (.mkdir pkg-dir)
        (spit (File. pkg-dir "greet.el") "(setq greeted 'hi)"))
      (let [ed (atom {:buf "t" :bufs {"t" (buf/make "t" "" nil)} :msg nil})]
        (with-home h
          (fn []
            (is (true? (pkg/load-package "greet" ed)))
            (is (= 'hi (get @(:el-vars @ed) 'greeted))))))
      (finally (rm-rf h)))))

(deftest load-package-returns-false-if-missing
  (let [[h _] (tmp-home)]
    (try
      (let [ed (atom {:buf "t" :bufs {"t" (buf/make "t" "" nil)}})]
        (with-home h #(is (false? (pkg/load-package "nope" ed)))))
      (finally (rm-rf h)))))

(deftest load-all-loads-every-installed-package
  (let [[h p] (tmp-home)]
    (try
      (let [a (File. p "a") b (File. p "b")]
        (.mkdir a) (spit (File. a "a.el") "(setq marker-a 1)")
        (.mkdir b) (spit (File. b "b.el") "(setq marker-b 2)"))
      (let [ed (atom {:buf "t" :bufs {"t" (buf/make "t" "" nil)} :msg nil})]
        (with-home h #(pkg/load-all! ed))
        (is (= 1 (get @(:el-vars @ed) 'marker-a)))
        (is (= 2 (get @(:el-vars @ed) 'marker-b))))
      (finally (rm-rf h)))))

(deftest load-package-rejects-traversal-names
  ;; A package name containing '..' or path separators must not escape the
  ;; packages directory. `load-package` throws on invalid names; load-all!
  ;; swallows the exception per-entry and keeps going.
  (let [[h _] (tmp-home)]
    (try
      (let [ed (atom {:buf "t" :bufs {"t" (buf/make "t" "" nil)}})]
        (with-home h
          (fn []
            (is (thrown? Exception (pkg/load-package ".." ed)))
            (is (thrown? Exception (pkg/load-package "../foo" ed)))
            (is (thrown? Exception (pkg/load-package "a/b" ed)))
            (is (thrown? Exception (pkg/load-package "" ed)))
            (is (thrown? Exception (pkg/install "../x" "http://example/x"))))))
      (finally (rm-rf h)))))
