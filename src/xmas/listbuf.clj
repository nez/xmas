(ns xmas.listbuf
  "A read-only buffer rendered as a header plus one line per entry, with the
   entry vector cached on the buffer for O(1) line→entry lookup. Used by
   dired and buflist; any read-only listing should reach for it instead of
   reimplementing the line-to-entry math."
  (:require [clojure.string :as str]
            [xmas.buf :as buf]
            [xmas.cmd :as cmd]))

(defn make
  "Build a list-buffer from a spec.
   :name           buffer name
   :mode           mode keyword (e.g. :dired-mode)
   :header         line 0
   :entries        rich records, one per subsequent line
   :format-entry   (fn [entry] line-string)
   :version        optional refresh counter
   :extras         optional map merged onto the buffer"
  [{:keys [name mode header entries format-entry version extras]
    :or   {version 0 extras {}}}]
  (let [text (str/join "\n" (cons header (map format-entry entries)))]
    (-> (buf/make name text nil)
        (assoc :mode mode :read-only true
               :listbuf-entries (vec entries) :version version)
        (merge extras))))

(defn entry-at-point
  "The entry at point, accounting for the 1-line header. Nil on the header."
  [s]
  (let [idx (dec (cmd/line-idx s))]
    (when (>= idx 0) (get (:listbuf-entries (cmd/cur s)) idx))))

(defn refresh
  "Replace `name` with a fresh make from `spec`, preserving point clamped
   to the new text length and bumping the version."
  [s name spec]
  (let [old   (get-in s [:bufs name])
        v     (inc (or (:version old) 0))
        p     (or (:point old) 0)
        fresh (make (assoc spec :version v))]
    (assoc-in s [:bufs name]
              (assoc fresh :point (min p (count (:text fresh)))))))
