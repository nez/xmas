(ns xmas.cmd
  "Pure state-transition commands shared by ed (terminal) and el (elisp bridge)."
  (:require [xmas.buf :as buf]
            [xmas.gap :as gap]
            [xmas.text :as text]))

(defn buf [s name] (get (:bufs s) name))
(defn cur [s] (buf s (:buf s)))
(defn update-cur [s f] (update-in s [:bufs (:buf s)] f))

(defn line-idx
  "Line number of point in the current buffer."
  [s]
  (let [b (cur s)] (gap/line-of (:text b) (:point b))))

(defn cur-window-path
  "Path into state for the current window leaf."
  [s] (into [:windows] (:cur-window s)))

(defn set-cur-buffer
  "Point the current window at `name` and cache it as (:buf s). Clears any
   saved point/mark so stale coords from the previous buffer can't leak in."
  [s name]
  (let [wp (cur-window-path s)]
    (-> s (assoc :buf name)
          (update-in wp dissoc :point :mark)
          (assoc-in (conj wp :buffer) name)
          (assoc-in (conj wp :scroll) 0)
          (assoc-in (conj wp :hscroll) 0))))

(defn save-cur-point
  "Write the current buffer's point/mark onto the current window.
   No-op if the current buffer doesn't match the window's buffer — e.g.
   while a minibuffer is active (:buf = \" *mini*\"), writing its point
   onto the underlying window would corrupt the real buffer's saved
   coords the next time the window was switched back in."
  [s]
  (let [wp (cur-window-path s)
        b  (cur s)
        win-buf (get-in s (conj wp :buffer))]
    (if (or (nil? b) (not= (:name b) win-buf))
      s
      (-> s (assoc-in (conj wp :point) (:point b))
            (assoc-in (conj wp :mark)  (:mark b))))))

(defn load-window-point
  "If the current window has a saved :point/:mark, copy them onto its buffer."
  [s]
  (let [w (get-in s (cur-window-path s))
        b-name (:buf s)]
    (cond-> s
      (contains? w :point) (assoc-in [:bufs b-name :point] (:point w))
      (contains? w :mark)  (assoc-in [:bufs b-name :mark]  (:mark w)))))

(defn switch-window
  "Save current point, swap `:cur-window` to `path`, load that window's point,
   and update `:buf` to match its buffer."
  [s path]
  (let [s (-> s save-cur-point (assoc :cur-window path))
        target-buf (get-in s (conj (cur-window-path s) :buffer))]
    (-> s (assoc :buf target-buf) load-window-point)))
(defn set-point [s f] (update-cur s #(buf/set-point % f)))
(defn edit [s from to repl]
  (if (:read-only (cur s))
    (assoc s :msg "Buffer is read-only")
    (update-cur s #(buf/edit % from to repl))))
(defn msg [s m] (assoc s :msg m))
(defn msg-error [s prefix ^Throwable e] (msg s (str prefix ": " (.getMessage e))))

(defn- step-n [t p n step]
  (loop [p p i (Math/abs (long n))]
    (if (zero? i) p (recur (step t p) (dec i)))))

(defn forward-char
  ([s] (forward-char s 1))
  ([s n]
   (let [step (if (neg? (long n)) text/prev-pos text/next-pos)]
     (set-point s (fn [t p] (step-n t p n step))))))

(defn backward-char
  ([s]   (forward-char s -1))
  ([s n] (forward-char s (- (long n)))))

(defn goto-char [s n] (set-point s (fn [_ _] (long n))))

(defn beginning-of-line   [s] (set-point s gap/line-start))
(defn end-of-line         [s] (set-point s gap/line-end))
(defn beginning-of-buffer [s] (goto-char s 0))
(defn end-of-buffer       [s] (goto-char s Long/MAX_VALUE))
(defn forward-word  [s] (set-point s text/word-forward))
(defn backward-word [s] (set-point s text/word-backward))

(defn insert-at-point [s ^String text]
  (let [p (:point (cur s))] (edit s p p text)))

(defn insert-newline [s] (insert-at-point s "\n"))

(defn delete-char [s]
  (let [b (cur s) p (:point b) t (:text b)]
    (if (< p (count t)) (edit s p (text/next-pos t p) "") s)))

(defn delete-backward-char [s]
  (let [b (cur s) p (:point b) t (:text b)]
    (if (> p 0) (edit s (text/prev-pos t p) p "") s)))

(defn delete-region [s from to]
  (let [lo (min (long from) (long to))
        hi (max (long from) (long to))]
    (edit s lo hi "")))
