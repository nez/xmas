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
(defn text [s] (str (:text (ed/cur s))))

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

;; --- Redo ---

(deftest redo-after-undo
  (let [s (make-state "abc" 3)
        edited (-> s (ed/handle-key \d))       ;; "abcd", point=4
        undone (ed/undo edited)                ;; "abc", point=3
        redone (ed/redo undone)]               ;; "abcd", point=4
    (is (= "abcd" (text redone)))
    (is (= 4 (point redone)))))

(deftest redo-with-nothing
  (let [s' (ed/redo (make-state "abc" 0))]
    (is (= "No redo" (:msg s')))))

;; --- Goto line ---

(deftest goto-line-basic
  (let [s' (ed/goto-line (make-state "aaa\nbbb\nccc" 0) "2")]
    (is (= 4 (point s')))))

(deftest goto-line-first
  (let [s' (ed/goto-line (make-state "aaa\nbbb" 5) "1")]
    (is (= 0 (point s')))))

(deftest goto-line-past-end
  (let [s' (ed/goto-line (make-state "aaa\nbbb" 0) "99")]
    (is (= 4 (point s')))))  ;; clamps to start of last line

(deftest goto-line-not-a-number
  (let [s' (ed/goto-line (make-state "abc" 0) "xyz")]
    (is (= "Not a number" (:msg s')))))

;; --- Scroll ---

(deftest scroll-down-moves-by-page
  (let [lines (apply str (repeat 30 "line\n"))
        s (make-state lines 0)
        s' (ed/scroll-down s)]
    ;; rows=24, body=22, should move 22 lines down
    (is (> (point s') 0))
    (is (> (point s') (point s)))))

(deftest scroll-up-at-top-stays
  (let [s' (ed/scroll-up (make-state "abc\ndef\nghi" 0))]
    (is (= 0 (point s')))))

(deftest scroll-down-then-up-roundtrip
  (let [lines (apply str (repeat 60 "line\n"))
        s (make-state lines 0)
        down (ed/scroll-down s)
        up (ed/scroll-up down)]
    ;; Should return to start
    (is (= 0 (point up)))))

;; --- Incremental search ---

(deftest isearch-forward-find
  (let [s (make-state "hello world" 0)
        s1 (ed/handle-key s [:ctrl \s])     ;; start isearch
        s2 (ed/handle-key s1 \w)            ;; type 'w'
        s3 (ed/handle-key s2 \o)            ;; type 'o'
        s4 (ed/handle-key s3 :return)]      ;; accept
    (is (some? (:isearch s1)))
    (is (= 6 (point s4)))                   ;; "wo" found at 6
    (is (nil? (:isearch s4)))))

(deftest isearch-backward-find
  (let [s (make-state "abc def abc" 11)
        s1 (ed/handle-key s [:ctrl \r])     ;; start backward isearch
        s2 (ed/handle-key s1 \a)            ;; type 'a', finds at 8
        s3 (ed/handle-key s2 :return)]      ;; accept
    (is (= 8 (point s3)))))

(deftest isearch-cancel-restores-point
  (let [s (make-state "hello world" 0)
        s1 (ed/handle-key s [:ctrl \s])
        s2 (ed/handle-key s1 \w)
        s3 (ed/handle-key s2 [:ctrl \g])]   ;; cancel
    (is (= 0 (point s3)))                   ;; restored to original
    (is (= "Quit" (:msg s3)))
    (is (nil? (:isearch s3)))))

(deftest isearch-not-found-shows-failing
  (let [s (make-state "hello" 0)
        s1 (ed/handle-key s [:ctrl \s])
        s2 (ed/handle-key s1 \z)]
    (is (clojure.string/starts-with? (:msg s2) "Failing I-search"))))

(deftest isearch-backspace-shortens-pattern
  (let [s (make-state "hello world" 0)
        s1 (ed/handle-key s [:ctrl \s])
        s2 (ed/handle-key s1 \w)
        s3 (ed/handle-key s2 \x)            ;; "wx" — not found
        s4 (ed/handle-key s3 :backspace)]   ;; back to "w"
    ;; After backspace, pattern is "w" which IS found
    (is (= "w" (get-in s4 [:isearch :pattern])))))

(deftest isearch-backspace-empty-cancels
  (let [s (make-state "hello" 3)
        s1 (ed/handle-key s [:ctrl \s])
        s2 (ed/handle-key s1 :backspace)]   ;; empty pattern → cancel
    (is (= 3 (point s2)))
    (is (nil? (:isearch s2)))))

(deftest isearch-next-forward
  (let [s (make-state "abc abc abc" 0)
        s1 (ed/handle-key s [:ctrl \s])
        s2 (ed/handle-key s1 \a)            ;; find 'a' at 4 (searches from inc(0)=1)
        s3 (ed/handle-key s2 [:ctrl \s])]   ;; next → finds 'a' at 8
    (is (= 8 (point s3)))))

(deftest isearch-non-search-key-accepts-and-dispatches
  (let [s (make-state "hello world" 0)
        s1 (ed/handle-key s [:ctrl \s])
        s2 (ed/handle-key s1 \w)            ;; find "w" at 6
        s3 (ed/handle-key s2 [:ctrl \f])]   ;; accept + forward-char
    (is (nil? (:isearch s3)))
    (is (= 7 (point s3)))))                 ;; 6 + forward = 7

;; --- Prefix keys ---

(deftest handle-key-prefix-then-command
  ;; C-x C-s requires a pending prefix
  (let [f (java.io.File/createTempFile "xmas-test" ".txt")]
    (try
      (let [s (-> (make-state "data" 0)
                  (assoc-in [:bufs "*test*" :file] (.getAbsolutePath f)))
            s' (-> s (ed/handle-key [:ctrl \x]) (ed/handle-key [:ctrl \s]))]
        (is (= "data" (slurp f)))
        (is (clojure.string/starts-with? (:msg s') "Wrote")))
      (finally (.delete f)))))

(deftest handle-key-prefix-undefined
  (let [s' (-> (make-state "abc" 0)
               (ed/handle-key [:ctrl \x])
               (ed/handle-key \z))]
    (is (clojure.string/starts-with? (:msg s') "prefix"))))

;; --- Exit ---

(deftest exit-no-modified-buffers
  (let [s' (-> (make-state "abc" 0)
               (ed/handle-key [:ctrl \x])
               (ed/handle-key [:ctrl \c]))]
    (is (:exit s'))))

(deftest exit-modified-requires-confirmation
  (let [s (-> (make-state "abc" 0) (ed/edit 0 0 "x"))
        s' (-> s (ed/handle-key [:ctrl \x]) (ed/handle-key [:ctrl \c]))]
    (is (not (:exit s')))
    (is (clojure.string/starts-with? (:msg s') "Modified"))))

;; --- Minibuffer history ---

(deftest mini-history-prev-next
  (let [s (-> (make-state "abc" 0) (assoc :mini-history ["first" "second"]))
        ;; Open mini, navigate history
        s1 (ed/mini-start s "Test: " (fn [s _] s))
        s2 (ed/handle-key s1 [:meta \p])        ;; → "second" (most recent)
        s3 (ed/handle-key s2 [:meta \p])        ;; → "first"
        s4 (ed/handle-key s3 [:meta \n])        ;; → back to "second"
        s5 (ed/handle-key s4 [:meta \n])]       ;; → back to empty
    (is (= "second" (str (get-in s2 [:bufs " *mini*" :text]))))
    (is (= "first" (str (get-in s3 [:bufs " *mini*" :text]))))
    (is (= "second" (str (get-in s4 [:bufs " *mini*" :text]))))
    (is (= "" (str (get-in s5 [:bufs " *mini*" :text]))))))

(deftest mini-accept-records-history
  (let [result (atom nil)
        s (make-state "abc" 0)
        s1 (ed/mini-start s "Test: " (fn [s input] (reset! result input) s))
        ;; Type "foo" into minibuffer
        s2 (-> s1 (ed/handle-key \f) (ed/handle-key \o) (ed/handle-key \o))
        s3 (ed/handle-key s2 :return)]
    (is (= "foo" @result))
    (is (= ["foo"] (:mini-history s3)))))

;; --- CRLF normalization ---

(deftest find-file-crlf-normalized
  (let [f (java.io.File/createTempFile "xmas-crlf" ".txt")]
    (try
      ;; Write CRLF content
      (let [os (java.io.FileOutputStream. f)]
        (.write os (.getBytes "line1\r\nline2\r\n"))
        (.close os))
      (let [s' (ed/find-file (make-state "" 0) (.getAbsolutePath f))]
        (is (= "line1\nline2\n" (text s')))
        (is (= :crlf (:line-ending (ed/cur s')))))
      (finally (.delete f)))))

;; --- Non-printable key ignored ---

(deftest handle-key-control-char-below-space
  (let [s' (ed/handle-key (make-state "abc" 0) (char 1))]
    ;; Ctrl-A is bound to beginning-of-line, dispatches normally
    (is (= 0 (point s')))))

(deftest handle-key-low-char-unbound
  ;; char 2 = Ctrl-B = backward-char, but char 0 is not printable and < 32
  (let [s' (ed/handle-key (make-state "abc" 1) (char 0))]
    ;; char 0 is < 32 and not printable → no self-insert
    (is (= "abc" (text s')))))
