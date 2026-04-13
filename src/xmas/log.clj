(ns xmas.log
  (:import [java.io FileWriter]
           [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]))

(def ^:private log-file (atom nil))
(def ^:private fmt (DateTimeFormatter/ofPattern "HH:mm:ss.SSS"))

(defn init!
  "Open log file. Call once at startup."
  ([] (init! (str (System/getProperty "user.home") "/.xmas/debug.log")))
  ([path]
   (let [f (java.io.File. path)]
     (.mkdirs (.getParentFile f))
     (reset! log-file (FileWriter. f true)))))

(defn log [& args]
  (when-let [^FileWriter w @log-file]
    (let [ts (.format (LocalDateTime/now) fmt)
          msg (apply str (interpose " " args))]
      (locking w
        (.write w (str ts " " msg "\n"))
        (.flush w)))))

(defn close! []
  (when-let [^FileWriter w @log-file]
    (locking w
      (.close w))
    (reset! log-file nil)))
