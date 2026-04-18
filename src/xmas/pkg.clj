(ns xmas.pkg
  "Minimal package manager: packages live under ~/.xmas/packages/<name>/
   and expose a <name>.el file that we eval on load."
  (:require [clojure.java.io :as io]
            [xmas.el :as el]
            [xmas.log :as log])
  (:import [java.io File]))

(defn packages-dir []
  (str (System/getProperty "user.home") "/.xmas/packages"))

(def ^:private name-re #"[A-Za-z0-9_.][A-Za-z0-9_.-]*")

(defn- valid-name? [pkg]
  (and (string? pkg) (not= pkg "..") (boolean (re-matches name-re pkg))))

(defn- check-name! [pkg]
  (when-not (valid-name? pkg)
    (throw (ex-info (str "invalid package name: " (pr-str pkg)) {:pkg pkg}))))

(defn- pkg-file ^File [pkg]
  (check-name! pkg)
  (File. (str (packages-dir) "/" pkg "/" pkg ".el")))

(defn installed
  "Return a sorted vector of installed package names."
  []
  (let [d (File. (packages-dir))]
    (if (.isDirectory d)
      (vec (sort (.list d)))
      [])))

(defn load-package
  "Eval `~/.xmas/packages/<pkg>/<pkg>.el` against the given editor-state atom.
   Returns true on success, false if the file is missing."
  [pkg editor-atom]
  (let [f (pkg-file pkg)]
    (if (.exists f)
      (do (el/eval-string (slurp f :encoding "UTF-8") editor-atom) true)
      false)))

(defn load-all!
  "Load every installed package. Logs failures and continues."
  [editor-atom]
  (doseq [name (installed)]
    (try (when (load-package name editor-atom)
           (log/log "pkg:" name "loaded"))
         (catch Exception e (log/log "pkg:" name "failed:" (.getMessage e))))))

(def ^:private url-re #"^(https?|git|ssh)://[^\s]+$")

(defn install
  "Clone `url` into ~/.xmas/packages/<pkg>. Returns {:status :ok|:err :msg s}."
  [pkg url]
  (check-name! pkg)
  ;; Reject URLs that don't look like a normal scheme. Otherwise a URL
  ;; starting with `-` (e.g. `--upload-pack=...`) would be parsed by git
  ;; as an option and could execute arbitrary commands (CVE-2017-1000117
  ;; class). `--` before the positional args is a second belt.
  (when-not (and (string? url) (re-matches url-re url))
    (throw (ex-info (str "invalid package url: " (pr-str url)) {:url url})))
  (let [parent (File. (packages-dir))
        target (File. parent pkg)]
    (.mkdirs parent)
    (if (.exists target)
      {:status :ok :msg (str pkg " already installed")}
      (try
        (let [pb   (doto (ProcessBuilder. ["git" "clone" "--" url (.getAbsolutePath target)])
                     (.redirectErrorStream true))
              proc (.start pb)
              out  (slurp (.getInputStream proc))
              exit (.waitFor proc)]
          (if (zero? exit)
            {:status :ok :msg (str "Installed " pkg)}
            {:status :err :msg (str "git clone failed: " out)}))
        (catch Exception e
          {:status :err :msg (str "Install error: " (.getMessage e))})))))
