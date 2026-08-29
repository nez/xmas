(ns xmas.dired
  "Directory listing buffers built on listbuf. Marks live here because they
   mutate buffer text directly; navigation/refresh delegate to listbuf."
  (:require [xmas.cmd :as cmd]
            [xmas.gap :as gap]
            [xmas.listbuf :as listbuf])
  (:import [java.io File]))

(defn- file-entries [^String dir]
  (let [d (File. dir)]
    (when (.isDirectory d)
      (let [parent (.getParentFile d)
            children (sort-by #(.getName ^File %) (or (.listFiles d) []))]
        (vec (cond->> children parent (cons parent)))))))

(defn- format-entry [^File f self-path parent-path]
  (let [path (.getCanonicalPath f)
        name (cond
               (= path self-path)   "."
               (= path parent-path) ".."
               :else (.getName f))
        suffix (if (.isDirectory f) "/" "")]
    (str "  " name suffix)))

(defn buf-name-for [^String dir] (str "*dired " dir "*"))

(defn- spec [^String dir]
  (let [d (File. dir)
        self (.getCanonicalPath d)
        parent (.getParentFile d)
        parent-path (when parent (.getCanonicalPath parent))]
    {:name         (buf-name-for dir)
     :mode         :dired-mode
     :header       (str "  " dir ":")
     :entries      (file-entries dir)
     :format-entry #(format-entry % self parent-path)
     :extras       {:dired-dir dir}}))

(defn make-buffer
  "Build a fresh dired buffer for `dir`."
  [^String dir]
  (listbuf/make (spec dir)))

(defn open
  "Open `dir` in a dired buffer, reusing any existing buffer of the same name."
  [s dir]
  (let [canonical (.getCanonicalPath (File. ^String dir))
        name      (buf-name-for canonical)]
    (-> s
        (assoc-in [:bufs name] (make-buffer canonical))
        (cmd/set-cur-buffer name))))

;; --- Navigation / marks ---

(defn- goto-next-line [s]
  (let [b (cmd/cur s)
        t (:text b)
        ln (cmd/line-idx s)
        next (min (inc ln) (dec (gap/line-count t)))]
    (cmd/update-cur s #(assoc % :point (gap/nth-line-start t next)))))

(defn- set-mark-char
  "Replace the mark character (column 0) on the current line."
  [s ^Character ch]
  (let [b (cmd/cur s)
        t (:text b)
        ln (cmd/line-idx s)
        start (gap/nth-line-start t ln)
        eol   (gap/nth-line-end t ln)]
    ;; Skip header (line 0) and empty lines — editing `(inc start)` when
    ;; the line is empty would eat the newline and weld two file rows.
    (if (or (zero? ln) (>= start eol))
      s
      (cmd/update-cur s
        #(if (= ch (.charAt ^CharSequence (:text %) (int start)))
           %
           (-> % (assoc :text (gap/edit (:text %) start (inc start) (str ch)))
                 (update :version (fnil inc 0))))))))

(defn mark-delete [s] (-> s (set-mark-char \D) goto-next-line))
(defn unmark      [s] (-> s (set-mark-char \space) goto-next-line))

(defn- marked-files
  "Scan the buffer for lines beginning with 'D ' and return their Files."
  [s]
  (let [b (cmd/cur s)
        t (:text b)
        files (:listbuf-entries b)]
    (keep-indexed
      (fn [idx f]
        (let [start (gap/nth-line-start t (inc idx))]
          (when (and (< start (count t))
                     (= \D (.charAt ^CharSequence t (int start))))
            f)))
      files)))

(defn revert
  "Re-read the directory, preserving point."
  [s]
  (let [b (cmd/cur s)]
    (listbuf/refresh s (:name b) (spec (:dired-dir b)))))

(defn do-delete
  "Delete all D-marked files and refresh."
  [s]
  (let [marked (marked-files s)]
    (if (empty? marked)
      (cmd/msg s "No marks")
      (let [deleted (reduce
                      (fn [n ^File f]
                        (if (try
                              (if (.isDirectory f)
                                (let [ls (.listFiles f)]
                                  (and (some? ls) (zero? (alength ls)) (.delete f)))
                                (.delete f))
                              (catch Exception _ false))
                          (inc n) n))
                      0 marked)
            failed (- (count marked) deleted)]
        (-> s revert
            (cmd/msg (if (zero? failed)
                       (str "Deleted " deleted)
                       (str "Deleted " deleted ", failed " failed))))))))

(defn find-file-at-point
  "Return [:dir path] or [:file path] for the entry at point, or nil for header."
  [s]
  (when-let [^File f (listbuf/entry-at-point s)]
    [(if (.isDirectory f) :dir :file) (.getCanonicalPath f)]))
