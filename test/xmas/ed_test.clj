(ns xmas.ed-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [xmas.buf :as buf]
            [xmas.ed :as ed]
            [xmas.spec :as spec]))

;; --- Test helpers ---

(defn make-state
  ([text] (make-state text 0))
  ([text point]
   {:buf "*test*"
    :bufs {"*test*" (assoc (buf/make "*test*" text nil) :point point)}
    :kill [] :msg nil :mini nil
    :scroll 0 :rows 24 :cols 80 :last-key nil}))

(defn point [s] (:point (ed/cur s)))
(defn text [s] (:text (ed/cur s)))

;; --- Properties ---

(def movement-commands
  [ed/forward-char ed/backward-char
   ed/beginning-of-line ed/end-of-line
   ed/beginning-of-buffer ed/end-of-buffer
   ed/next-line ed/previous-line
   ed/forward-word ed/backward-word])

(defspec movement-preserves-valid-state 200
  (prop/for-all [state spec/gen-editor
                 cmd-idx (gen/choose 0 (dec (count movement-commands)))]
    (let [cmd (nth movement-commands cmd-idx)
          s' (cmd state)]
      (and (contains? (:bufs s') (:buf s'))
           (<= 0 (point s') (count (text s')))))))

(defspec movement-doesnt-alter-text 200
  (prop/for-all [state spec/gen-editor
                 cmd-idx (gen/choose 0 (dec (count movement-commands)))]
    (let [cmd (nth movement-commands cmd-idx)]
      (= (text state) (text (cmd state))))))

(defspec self-insert-then-backspace 100
  (prop/for-all [state spec/gen-editor
                 ch (gen/fmap char (gen/choose 32 126))]
    (let [original-text (text state)
          inserted (ed/self-insert (assoc state :last-key ch))
          deleted (ed/delete-backward-char inserted)]
      (= original-text (text deleted)))))

;; --- Movement: forward/backward char ---

(deftest forward-char-basic
  (is (= 1 (point (ed/forward-char (make-state "hello" 0))))))

(deftest forward-char-at-end
  (is (= 2 (point (ed/forward-char (make-state "hi" 2))))))

(deftest forward-char-emoji
  (is (= 3 (point (ed/forward-char (make-state "a\uD83D\uDE00b" 1))))))

(deftest backward-char-basic
  (is (= 2 (point (ed/backward-char (make-state "hello" 3))))))

(deftest backward-char-at-start
  (is (= 0 (point (ed/backward-char (make-state "hello" 0))))))

(deftest backward-char-emoji
  (is (= 1 (point (ed/backward-char (make-state "a\uD83D\uDE00b" 3))))))

;; --- Movement: line ---

(deftest beginning-of-line-test
  (is (= 4 (point (ed/beginning-of-line (make-state "abc\ndef" 6))))))

(deftest end-of-line-test
  (is (= 3 (point (ed/end-of-line (make-state "abc\ndef" 0))))))

(deftest beginning-of-buffer-test
  (is (= 0 (point (ed/beginning-of-buffer (make-state "abc\ndef" 6))))))

(deftest end-of-buffer-test
  (is (= 7 (point (ed/end-of-buffer (make-state "abc\ndef" 0))))))

;; --- Movement: next/previous line ---

(deftest next-line-preserves-column
  (is (= 7 (point (ed/next-line (make-state "abcd\nefgh" 2))))))

(deftest next-line-clamps-to-shorter-line
  (is (= 7 (point (ed/next-line (make-state "abcd\nef" 3))))))

(deftest next-line-at-last-line
  (is (= 3 (point (ed/next-line (make-state "abc" 1))))))

(deftest previous-line-preserves-column
  (is (= 2 (point (ed/previous-line (make-state "abcd\nefgh" 7))))))

(deftest previous-line-at-first-line
  (is (= 0 (point (ed/previous-line (make-state "abc" 2))))))

;; --- Movement: word ---

(deftest forward-word-test
  (is (= 5 (point (ed/forward-word (make-state "hello world" 0))))))

(deftest backward-word-test
  (is (= 6 (point (ed/backward-word (make-state "hello world" 11))))))

;; --- Editing: self-insert ---

(deftest self-insert-char
  (let [s (-> (make-state "ab" 1) (assoc :last-key \X))]
    (is (= "aXb" (text (ed/self-insert s))))))

(deftest self-insert-string
  (let [s (-> (make-state "ab" 1) (assoc :last-key "\uD83D\uDE00"))]
    (is (= "a\uD83D\uDE00b" (text (ed/self-insert s))))))

(deftest self-insert-non-key
  (let [s (-> (make-state "ab" 1) (assoc :last-key :ctrl))]
    (is (= "ab" (text (ed/self-insert s))))))

(deftest insert-newline-test
  (is (= "a\nb" (text (ed/insert-newline (make-state "ab" 1))))))

;; --- Editing: delete ---

(deftest delete-char-basic
  (is (= "ac" (text (ed/delete-char (make-state "abc" 1))))))

(deftest delete-char-at-end
  (is (= "abc" (text (ed/delete-char (make-state "abc" 3))))))

(deftest delete-char-emoji
  (is (= "ab" (text (ed/delete-char (make-state "a\uD83D\uDE00b" 1))))))

(deftest delete-backward-char-basic
  (is (= "ac" (text (ed/delete-backward-char (make-state "abc" 2))))))

(deftest delete-backward-char-at-start
  (is (= "abc" (text (ed/delete-backward-char (make-state "abc" 0))))))

(deftest delete-backward-char-emoji
  (is (= "ab" (text (ed/delete-backward-char (make-state "a\uD83D\uDE00b" 3))))))

;; --- Kill / Yank ---

(deftest kill-line-to-eol
  (let [s' (ed/kill-line (make-state "abc\ndef" 0))]
    (is (= "\ndef" (text s')))
    (is (= ["abc"] (:kill s')))))

(deftest kill-line-at-eol-eats-newline
  (let [s' (ed/kill-line (make-state "abc\ndef" 3))]
    (is (= "abcdef" (text s')))
    (is (= ["\n"] (:kill s')))))

(deftest kill-line-at-end-of-buffer
  (is (= "abc" (text (ed/kill-line (make-state "abc" 3))))))

(deftest yank-inserts-killed
  (let [s' (-> (make-state "abc\ndef" 0) ed/kill-line ed/yank)]
    (is (= "abc\ndef" (text s')))))

(deftest yank-empty-kill-ring
  (is (= "abc" (text (ed/yank (make-state "abc" 0))))))

(deftest kill-region-test
  (let [s (-> (make-state "abcdef" 2)
              ed/set-mark
              (ed/set-point (fn [_ _] 5)))
        s' (ed/kill-region s)]
    (is (= "abf" (text s')))
    (is (= ["cde"] (:kill s')))
    (is (nil? (:mark (ed/cur s'))))))

(deftest kill-region-without-mark
  (is (= "abc" (text (ed/kill-region (make-state "abc" 1))))))

;; --- Undo ---

(deftest undo-restores-text
  (let [s' (-> (make-state "abc" 1) (ed/edit 1 1 "XY") ed/undo)]
    (is (= "abc" (text s')))))

(deftest undo-with-nothing
  (let [s' (ed/undo (make-state "abc" 0))]
    (is (= "No undo" (:msg s')))))

;; --- Mark ---

(deftest set-mark-test
  (is (= 2 (:mark (ed/cur (ed/set-mark (make-state "abc" 2)))))))

(deftest keyboard-quit-clears-mark
  (let [s' (ed/keyboard-quit (ed/set-mark (make-state "abc" 2)))]
    (is (nil? (:mark (ed/cur s'))))
    (is (= "Quit" (:msg s')))))

;; --- Minibuffer ---

(deftest mini-start-switches-buffer
  (let [s' (ed/mini-start (make-state "abc" 0) "Test: " (fn [s _] s))]
    (is (= " *mini*" (:buf s')))
    (is (some? (:mini s')))))

(deftest mini-accept-returns-to-prev
  (let [result (atom nil)
        s' (-> (make-state "abc" 0)
               (ed/mini-start "Test: " (fn [s input] (reset! result input) s))
               ed/mini-accept)]
    (is (= "*test*" (:buf s')))
    (is (nil? (:mini s')))
    (is (= "" @result))))

(deftest keyboard-quit-aborts-mini
  (let [s' (-> (make-state "abc" 0)
               (ed/mini-start "Test: " (fn [s _] s))
               ed/keyboard-quit)]
    (is (= "*test*" (:buf s')))
    (is (nil? (:mini s')))
    (is (= "Quit" (:msg s')))))

;; --- Buffer management ---

(deftest switch-buffer-existing
  (is (= "*test*" (:buf (ed/switch-buffer (make-state "abc" 0) "*test*")))))

(deftest switch-buffer-new
  (let [s' (ed/switch-buffer (make-state "abc" 0) "*new*")]
    (is (= "*new*" (:buf s')))
    (is (contains? (:bufs s') "*new*"))
    (is (= "" (text s')))))

(deftest find-file-test
  (let [f (java.io.File/createTempFile "xmas-test" ".txt")]
    (try
      (spit f "test content")
      (let [s' (ed/find-file (make-state "" 0) (.getAbsolutePath f))]
        (is (= "test content" (text s')))
        (is (not= "*test*" (:buf s'))))
      (finally (.delete f)))))

(deftest save-buffer-test
  (let [f (java.io.File/createTempFile "xmas-test" ".txt")]
    (try
      (let [s (-> (make-state "new content" 0)
                  (assoc-in [:bufs "*test*" :file] (.getAbsolutePath f)))
            s' (ed/save-buffer s)]
        (is (= "new content" (slurp f)))
        (is (false? (:modified (ed/cur s'))))
        (is (clojure.string/starts-with? (:msg s') "Wrote")))
      (finally (.delete f)))))

(deftest save-buffer-no-file
  (is (= "No file" (:msg (ed/save-buffer (make-state "abc" 0))))))

;; --- handle-key ---

(deftest handle-key-bound-command
  (is (= 1 (point (ed/handle-key (make-state "abc" 0) [:ctrl \f])))))

(deftest handle-key-self-insert
  (is (= "aXb" (text (ed/handle-key (make-state "ab" 1) \X)))))

(deftest handle-key-undefined
  (is (some? (:msg (ed/handle-key (make-state "abc" 0) :f12)))))

(deftest handle-key-mini-return
  (let [accepted (atom false)
        s (ed/mini-start (make-state "abc" 0) "?" (fn [s _] (reset! accepted true) s))
        s' (ed/handle-key s :return)]
    (is @accepted)
    (is (nil? (:mini s')))))
