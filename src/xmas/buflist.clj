(ns xmas.buflist
  "*Buffer List* buffer built on listbuf: tabular view of all open buffers
   with keys to switch, kill, or refresh."
  (:require [xmas.cmd :as cmd]
            [xmas.listbuf :as listbuf]
            [xmas.window :as win]))

(def ^:private buflist-name "*Buffer List*")

(defn- format-entry [b]
  (format "  %s %-20s %s"
          (if (:modified b) "*" " ")
          (or (:name b) "")
          (or (:file b) "")))

(defn- visible-bufs [bufs]
  (->> (vals bufs)
       (remove #(= buflist-name (:name %)))
       (remove #(and (:name %) (.startsWith ^String (:name %) " ")))
       (sort-by :name)
       vec))

(defn- spec [bufs]
  {:name         buflist-name
   :mode         :buflist-mode
   :header       "  M Name                 File"
   :entries      (visible-bufs bufs)
   :format-entry format-entry})

(defn make-buffer
  ([bufs] (make-buffer bufs 0))
  ([bufs version] (listbuf/make (assoc (spec bufs) :version version))))

(defn open [s]
  (-> s (listbuf/refresh buflist-name (spec (:bufs s)))
        (cmd/set-cur-buffer buflist-name)))

;; --- Commands ---

(defn switch
  "Switch to the buffer on the current line."
  [s]
  (if-let [b (listbuf/entry-at-point s)]
    (cmd/set-cur-buffer s (:name b))
    s))

(defn kill
  "Kill the buffer on the current line, then refresh the list."
  [s]
  (if-let [b (listbuf/entry-at-point s)]
    (let [target (:name b)
          s' (-> s
                 (update :bufs dissoc target)
                 ;; windows showing the killed buffer fall back to the buflist
                 (update :windows win/replace-buffer target buflist-name))
          s' (cond-> s'
               (= target (:buf s')) (assoc :buf buflist-name))]
      (-> s' (listbuf/refresh buflist-name (spec (:bufs s')))
             (cmd/msg (str "Killed " target))))
    s))

(defn revert
  "Re-render the buffer list from current :bufs state."
  [s]
  (listbuf/refresh s buflist-name (spec (:bufs s))))
