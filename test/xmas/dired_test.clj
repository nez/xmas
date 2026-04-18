(ns xmas.dired-test
  (:require [clojure.test :refer [deftest is testing]]
            [xmas.cmd :as cmd]
            [xmas.dired :as dired]
            [xmas.gap :as gap])
  (:import [java.io File]))

(defn- mktemp-dir []
  (doto (File/createTempFile "xmas-dired-" "")
    (.delete)
    (.mkdir)))

(defn- cleanup-dir [^File d]
  (when (.isDirectory d)
    (doseq [^File f (.listFiles d)] (.delete f))
    (.delete d)))

(defn- state-with [buf-name buf]
  {:buf buf-name :bufs {buf-name buf} :msg nil})

(deftest open-creates-dired-buffer
  (let [d (mktemp-dir)]
    (try
      (spit (File. d "a.txt") "a")
      (spit (File. d "b.txt") "b")
      (let [s (dired/open {:buf nil :bufs {}} (.getCanonicalPath d))
            b (cmd/cur s)]
        (is (= :dired-mode (:mode b)))
        (is (true? (:read-only b)))
        (is (.startsWith ^String (:buf s) "*dired "))
        (is (.contains ^String (str (:text b)) "a.txt"))
        (is (.contains ^String (str (:text b)) "b.txt")))
      (finally (cleanup-dir d)))))

(deftest read-only-blocks-edits
  (let [d (mktemp-dir)]
    (try
      (spit (File. d "f.txt") "x")
      (let [s  (dired/open {:buf nil :bufs {}} (.getCanonicalPath d))
            s' (cmd/insert-at-point s "X")]
        (is (= "Buffer is read-only" (:msg s')))
        (is (= (str (:text (cmd/cur s))) (str (:text (cmd/cur s'))))))
      (finally (cleanup-dir d)))))

(deftest mark-delete-sets-D-char
  (let [d (mktemp-dir)]
    (try
      (spit (File. d "f.txt") "x")
      (let [s0 (dired/open {:buf nil :bufs {}} (.getCanonicalPath d))
            ;; move to line 1 (first file entry — parent ".." because dir has a parent)
            s1 (cmd/update-cur s0 #(assoc % :point
                                          (gap/nth-line-start (:text %) 1)))
            s2 (dired/mark-delete s1)
            txt (str (:text (cmd/cur s2)))
            line1-start (gap/nth-line-start (:text (cmd/cur s2)) 1)]
        (is (= \D (.charAt ^CharSequence txt (int line1-start)))))
      (finally (cleanup-dir d)))))

(deftest unmark-resets-char
  (let [d (mktemp-dir)]
    (try
      (spit (File. d "f.txt") "x")
      (let [s0 (dired/open {:buf nil :bufs {}} (.getCanonicalPath d))
            s1 (cmd/update-cur s0 #(assoc % :point
                                          (gap/nth-line-start (:text %) 1)))
            s2 (dired/mark-delete s1)
            ;; reposition to same line before unmark
            s3 (cmd/update-cur s2 #(assoc % :point
                                          (gap/nth-line-start (:text %) 1)))
            s4 (dired/unmark s3)
            txt (str (:text (cmd/cur s4)))
            line1-start (gap/nth-line-start (:text (cmd/cur s4)) 1)]
        (is (= \space (.charAt ^CharSequence txt (int line1-start)))))
      (finally (cleanup-dir d)))))

(deftest do-delete-removes-marked-files
  (let [d (mktemp-dir)]
    (try
      (let [target (File. d "doomed.txt")]
        (spit target "bye")
        (spit (File. d "keeper.txt") "hi")
        ;; Find the line index for "doomed.txt" — entries sorted alphabetically,
        ;; with parent prepended, so: [parent, doomed.txt, keeper.txt]
        (let [s0 (dired/open {:buf nil :bufs {}} (.getCanonicalPath d))
              files (:dired-files (cmd/cur s0))
              doomed-idx (first (keep-indexed
                                  #(when (= "doomed.txt" (.getName ^File %2)) %1)
                                  files))
              doomed-line (inc doomed-idx)
              s1 (cmd/update-cur s0 #(assoc % :point
                                            (gap/nth-line-start (:text %) doomed-line)))
              s2 (dired/mark-delete s1)
              s3 (dired/do-delete s2)]
          (is (not (.exists target)))
          (is (.contains ^String (str (:msg s3)) "Deleted"))
          (is (.exists (File. d "keeper.txt")))))
      (finally (cleanup-dir d)))))

(deftest do-delete-no-marks
  (let [d (mktemp-dir)]
    (try
      (let [s (dired/open {:buf nil :bufs {}} (.getCanonicalPath d))
            s' (dired/do-delete s)]
        (is (= "No marks" (:msg s'))))
      (finally (cleanup-dir d)))))

(deftest revert-rereads-directory
  (let [d (mktemp-dir)]
    (try
      (spit (File. d "old.txt") "x")
      (let [s0 (dired/open {:buf nil :bufs {}} (.getCanonicalPath d))]
        (spit (File. d "new.txt") "y")
        (let [s1 (dired/revert s0)]
          (is (.contains ^String (str (:text (cmd/cur s1))) "new.txt"))))
      (finally (cleanup-dir d)))))
