(ns xmas.buflist
  "*Buffer List* buffer: tabular view of all open buffers with keys to
   switch, kill, or refresh."
  (:require [clojure.string :as str]
            [xmas.buf :as buf]
            [xmas.cmd :as cmd]
            [xmas.window :as win]))

(def ^:private buflist-name "*Buffer List*")

(defn- format-line [b]
  (let [marker (if (:modified b) "*" " ")
        n  (or (:name b) "")
        f  (or (:file b) "")]
    (format "  %s %-20s %s" marker n f)))

(defn- visible-bufs [bufs]
  (->> (vals bufs)
       (remove #(= buflist-name (:name %)))
       (remove #(and (:name %) (.startsWith ^String (:name %) " ")))
       (sort-by :name)
       vec))

(defn- render [bufs]
  (let [vs (visible-bufs bufs)]
    [(str/join "\n" (cons "  M Name                 File"
                          (map format-line vs)))
     vs]))

(defn make-buffer
  ([bufs] (make-buffer bufs 0))
  ([bufs version]
   (let [[text vs] (render bufs)]
     (-> (buf/make buflist-name text nil)
         (assoc :mode :buflist-mode :read-only true :buflist-entries vs
                :version version)))))

(defn- next-version [s]
  (inc (or (get-in s [:bufs buflist-name :version]) 0)))

(defn open [s]
  (-> s (assoc-in [:bufs buflist-name] (make-buffer (:bufs s) (next-version s)))
        (cmd/set-cur-buffer buflist-name)))

;; --- line → buffer helpers ---

(defn- entry-at-point
  "Return the buffer record at point (accounting for 1-line header), or nil."
  [s]
  (let [idx (dec (cmd/line-idx s))]
    (when (>= idx 0)
      (get (:buflist-entries (cmd/cur s)) idx))))

;; --- Commands ---

(defn switch
  "Switch to the buffer on the current line."
  [s]
  (if-let [b (entry-at-point s)]
    (cmd/set-cur-buffer s (:name b))
    s))

(defn kill
  "Kill the buffer on the current line, then refresh the list."
  [s]
  (if-let [b (entry-at-point s)]
    (let [target (:name b)
          p (:point (cmd/cur s))
          s' (-> s
                 (update :bufs dissoc target)
                 ;; windows showing the killed buffer fall back to the buflist
                 (update :windows win/replace-buffer target buflist-name))
          s' (cond-> s'
               (= target (:buf s')) (assoc :buf buflist-name))
          fresh (make-buffer (:bufs s') (next-version s'))]
      (-> s'
          (assoc-in [:bufs buflist-name]
                    (assoc fresh :point (min p (count (:text fresh)))))
          (cmd/msg (str "Killed " target))))
    s))

(defn revert
  "Re-render the buffer list from current :bufs state."
  [s]
  (let [p (:point (cmd/cur s))
        fresh (make-buffer (:bufs s) (next-version s))]
    (-> s (assoc-in [:bufs buflist-name]
                    (assoc fresh :point (min p (count (:text fresh))))))))
