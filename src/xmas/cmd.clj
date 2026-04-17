(ns xmas.cmd
  "Pure state-transition commands shared by ed (terminal) and el (elisp bridge)."
  (:require [xmas.buf :as buf]
            [xmas.gap :as gap]
            [xmas.text :as text]))

(defn buf [s name] (get (:bufs s) name))
(defn cur [s] (buf s (:buf s)))
(defn update-cur [s f] (update-in s [:bufs (:buf s)] f))
(defn set-point [s f] (update-cur s #(buf/set-point % f)))
(defn edit [s from to repl] (update-cur s #(buf/edit % from to repl)))
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
