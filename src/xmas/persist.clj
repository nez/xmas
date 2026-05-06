(ns xmas.persist
  "Read/write small EDN documents under ~/.xmas/. Used by bookmarks,
   recentf, and mini-history. Failures are silenced — persistence is
   convenience, not correctness, and a corrupt or missing file should
   never prevent the editor from starting."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [java.io PushbackReader]))

(defn- xmas-dir ^java.io.File []
  (io/file (System/getProperty "user.home") ".xmas"))

(defn- file ^java.io.File [^String name]
  (io/file (xmas-dir) (str name ".edn")))

(defn load!
  "Read EDN from name; return default on any failure."
  ([name]         (load! name nil))
  ([name default]
   (let [f (file name)]
     (if (.exists f)
       (try
         (with-open [r (PushbackReader. (io/reader f))]
           (edn/read {:default (fn [_ v] v)} r))
         (catch Exception _ default))
       default))))

(defn save!
  "Write `value` to name as EDN. Creates the directory if missing.
   Errors propagate as they would for any spit, which is fine — callers
   that want silence wrap in try."
  [name value]
  (let [d (xmas-dir)]
    (when-not (.exists d) (.mkdirs d)))
  (spit (file name) (pr-str value) :encoding "UTF-8"))

(defn save-quiet!
  "Like save! but swallows IO errors (the common case for autosaving
   convenience data on every change)."
  [name value]
  (try (save! name value) (catch Exception _ nil)))
