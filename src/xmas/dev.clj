(ns xmas.dev
  (:require [xmas.log :as log])
  (:import [java.nio.file FileSystems StandardWatchEventKinds Path]))

(defn ns-for-file
  "Convert src/xmas/foo.clj -> xmas.foo"
  [^String path]
  (when (.endsWith path ".clj")
    (-> path
        (.replace "src/" "")
        (.replace "/" ".")
        (.replace "_" "-")
        (.replace ".clj" "")
        symbol)))

(defn reload! [ns-sym]
  (try
    (require ns-sym :reload)
    (log/log "dev: reloaded" ns-sym)
    (catch Exception e
      (log/log "dev: reload failed" ns-sym (.getMessage e)))))

(defn start-watcher!
  "Watch src/ for .clj changes, auto-reload namespaces. Returns {:future f :running a}."
  []
  (let [watcher (.newWatchService (FileSystems/getDefault))
        src-path (.toPath (java.io.File. "src"))
        running (atom true)]
    ;; register all dirs recursively
    (doseq [dir (file-seq (java.io.File. "src"))
            :when (.isDirectory dir)]
      (.register (.toPath dir) watcher
                 (into-array [StandardWatchEventKinds/ENTRY_MODIFY])))
    (log/log "dev: watching src/ for changes")
    {:running running
     :future (future
               (while @running
                 (when-let [key (.poll watcher 500 java.util.concurrent.TimeUnit/MILLISECONDS)]
                   (doseq [event (.pollEvents key)]
                     (let [changed (str (.context event))
                           dir (.watchable key)
                           full (str (.resolve ^Path dir changed))
                           rel (.replace full (str (.toAbsolutePath src-path) "/") "")]
                       (when-let [ns-sym (ns-for-file (str "src/" rel))]
                         (reload! ns-sym))))
                   (.reset key)))
               (.close watcher)
               (log/log "dev: watcher stopped"))}))

(defn stop-watcher! [{:keys [running]}]
  (reset! running false))
