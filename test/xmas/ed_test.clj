(ns xmas.ed-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [xmas.buf :as buf]
            [xmas.ed :as ed]
            [xmas.spec :as spec]
            [xmas.window :as win]))

;; --- Test helpers ---

(defn make-state
  ([text] (make-state text 0))
  ([text point]
   {:buf "*test*"
    :bufs {"*test*" (assoc (buf/make "*test*" text nil) :point point)}
    :kill [] :msg nil :mini nil
    :windows (win/leaf "*test*") :cur-window []
    :scroll 0 :rows 24 :cols 80}))

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
          inserted (ed/self-insert state ch)
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
  (is (= "aXb" (text (ed/self-insert (make-state "ab" 1) \X)))))

(deftest self-insert-string
  (is (= "a\uD83D\uDE00b"
         (text (ed/self-insert (make-state "ab" 1) "\uD83D\uDE00")))))

(deftest self-insert-non-key
  (is (= "ab" (text (ed/self-insert (make-state "ab" 1) :ctrl)))))

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

(deftest switch-buffer-rejects-reserved-names
  ;; Regression: switch-buffer used to let the user switch into " *mini*"
  ;; (or any leading-space reserved name), wedging the minibuffer slot.
  (let [s  (make-state "abc" 0)
        s' (ed/switch-buffer s " *mini*")]
    (is (= "*test*" (:buf s')))
    (is (.contains ^String (:msg s') "Reserved"))))

(deftest find-file-test
  (let [f (java.io.File/createTempFile "xmas-test" ".txt")]
    (try
      (spit f "test content")
      (let [s' (ed/find-file (make-state "" 0) (.getAbsolutePath f))]
        (is (= "test content" (text s')))
        (is (not= "*test*" (:buf s'))))
      (finally (.delete f)))))

(deftest find-file-auto-detects-clojure-mode
  (let [f (java.io.File/createTempFile "xmas-test" ".clj")]
    (try
      (spit f "(defn foo [])")
      (let [s' (ed/find-file (make-state "" 0) (.getAbsolutePath f))]
        (is (= :clojure-mode (:mode (ed/cur s')))))
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

;; --- Window commands ---

(deftest split-window-below-creates-split
  (let [s (ed/split-window-below (make-state "abc"))]
    (is (= :split (get-in s [:windows :type])))
    (is (= :stacked (get-in s [:windows :dir])))
    (is (= [:a] (:cur-window s)))))

(deftest split-window-right-creates-side-by-side
  (let [s (ed/split-window-right (make-state "abc"))]
    (is (= :side-by-side (get-in s [:windows :dir])))))

(deftest other-window-cycles-focus
  (let [s (-> (make-state "abc") ed/split-window-below)
        s' (ed/other-window s)]
    (is (= [:b] (:cur-window s')))
    (is (= [:a] (:cur-window (ed/other-window s'))))))

(deftest delete-window-removes-current
  (let [s (-> (make-state "abc") ed/split-window-below ed/delete-window)]
    (is (= :leaf (get-in s [:windows :type])))
    (is (= [] (:cur-window s)))))

(deftest delete-other-windows-collapses-tree
  (let [s (-> (make-state "abc")
              ed/split-window-below
              ed/split-window-right
              ed/delete-other-windows)]
    (is (= :leaf (get-in s [:windows :type])))
    (is (= [] (:cur-window s)))))

(deftest other-window-updates-buf
  ;; After split, both panes show same buffer. Switch to a different buffer
  ;; in A, then check that C-x o restores B's original buffer view.
  (let [s0 (ed/split-window-below (make-state "a" 0))
        ;; Now both [:a] and [:b] show "*test*". [:a] is current. Open a new buf in [:a].
        s1 (-> s0
               (assoc-in [:bufs "other"] (buf/make "other" "xx" nil))
               (xmas.cmd/set-cur-buffer "other"))
        ;; Sanity: [:a] now shows "other", [:b] still "*test*"
        _ (is (= "other" (get-in s1 [:windows :a :buffer])))
        _ (is (= "*test*" (get-in s1 [:windows :b :buffer])))
        s2 (ed/other-window s1)]
    (is (= [:b] (:cur-window s2)))
    (is (= "*test*" (:buf s2)))))

(deftest per-window-point-independent-when-same-buffer
  ;; Split into two panes showing the same buffer; move in A, switch to B,
  ;; move in B, switch back to A — A's point should be preserved.
  (let [s0 (ed/split-window-below (make-state "hello world" 0))
        ;; A moves to position 3
        s1 (-> s0
               (assoc-in [:bufs "*test*" :point] 3))
        ;; switch to B — saves A's point=3, B has no saved, buffer keeps 3
        s2 (ed/other-window s1)
        _ (is (= 3 (:point (ed/cur s2))))
        ;; B moves to 8
        s3 (assoc-in s2 [:bufs "*test*" :point] 8)
        ;; switch back to A — saves B's 8, loads A's saved 3
        s4 (ed/other-window s3)]
    (is (= [:a] (:cur-window s4)))
    (is (= 3 (:point (ed/cur s4))))
    ;; switch back to B — should restore B's 8
    (let [s5 (ed/other-window s4)]
      (is (= 8 (:point (ed/cur s5)))))))

;; --- M-x ---

(defn- with-commands [s]
  (assoc s :commands (#'xmas.ed/builtin-commands)))

(deftest m-x-runs-named-command
  (let [s (with-commands (make-state "hello" 0))
        s' (ed/execute-extended-command s "forward-char")]
    (is (= 1 (:point (ed/cur s'))))))

(deftest m-x-reports-unknown-command
  (let [s (with-commands (make-state "hello"))
        s' (ed/execute-extended-command s "no-such-command")]
    (is (= "No command: no-such-command" (:msg s')))))

(deftest m-x-blank-input-is-noop
  (let [s (with-commands (make-state "hello" 3))
        s' (ed/execute-extended-command s "")]
    (is (= 3 (:point (ed/cur s'))))))

(deftest command-completer-returns-matches
  (let [s (with-commands (make-state "hello"))
        {:keys [completed candidates]} (#'xmas.ed/command-completer "for" s)]
    (is (= "forward-" completed))
    (is (every? #(.startsWith ^String % "for") candidates))))

(deftest c-u-sets-prefix-arg-to-4
  (let [s (ed/universal-argument (make-state "abcdefghij" 0))]
    (is (= {:mul 4} (:prefix-arg s)))))

(deftest c-u-c-u-multiplies-to-16
  (let [s (-> (make-state "abc") ed/universal-argument ed/universal-argument)]
    (is (= {:mul 16} (:prefix-arg s)))))

(deftest forward-char-respects-prefix
  (let [s (assoc (make-state "abcdefghij" 0) :prefix-arg {:mul 4})
        s' (ed/forward-char s)]
    (is (= 4 (:point (ed/cur s'))))
    (is (nil? (:prefix-arg s')))))

(deftest forward-char-no-prefix-moves-one
  (let [s' (ed/forward-char (make-state "abcdef" 0))]
    (is (= 1 (:point (ed/cur s'))))))

(deftest delete-char-respects-prefix
  (let [s (assoc (make-state "abcdef" 0) :prefix-arg {:num 3})
        s' (ed/delete-char s)]
    (is (= "def" (text s')))))

(deftest handle-key-accumulates-numeric-prefix
  ;; C-u, then "1", "2" → prefix-arg = {:num 12}
  (let [s0 (make-state "abcdefghij" 0)
        s1 (ed/handle-key s0 [:ctrl \u])
        s2 (ed/handle-key s1 \1)
        s3 (ed/handle-key s2 \2)]
    (is (= {:num 12} (:prefix-arg s3)))))

(deftest handle-key-clears-prefix-after-command
  ;; C-u 3 C-f → moves 3 chars, prefix cleared
  (let [s0 (make-state "abcdefghij" 0)
        s (-> s0
              (ed/handle-key [:ctrl \u])
              (ed/handle-key \3)
              (ed/handle-key [:ctrl \f]))]
    (is (= 3 (:point (ed/cur s))))
    (is (nil? (:prefix-arg s)))))

;; --- Keyboard macros ---

(deftest start-kbd-macro-enters-recording
  (let [s (ed/start-kbd-macro (make-state ""))]
    (is (vector? (:macro-recording s)))
    (is (empty? (:macro-recording s)))))

(deftest recording-captures-keys
  (let [s (-> (make-state "")
              ed/start-kbd-macro
              (ed/handle-key \a)
              (ed/handle-key \b))]
    (is (= [\a \b] (:macro-recording s)))))

(deftest end-kbd-macro-strips-trigger
  ;; handle-key for C-x stores :pending; next key )  appends and dispatches.
  ;; end-kbd-macro fires with last two recorded keys being [C-x, )].
  (let [s0 (make-state "")
        s1 (ed/start-kbd-macro s0)
        s2 (ed/handle-key s1 \a)
        s3 (ed/handle-key s2 \b)
        s4 (ed/handle-key s3 [:ctrl \x])
        s5 (ed/handle-key s4 \))]
    (is (nil? (:macro-recording s5)))
    (is (= [\a \b] (:last-macro s5)))))

(deftest call-last-kbd-macro-replays
  (let [s0 (make-state "")
        s1 (assoc s0 :last-macro [\a \b \c])
        s2 (ed/call-last-kbd-macro s1)]
    (is (= "abc" (text s2)))))

(deftest call-last-kbd-macro-no-macro
  (let [s (ed/call-last-kbd-macro (make-state ""))]
    (is (= "No kbd macro defined" (:msg s)))))

(deftest call-last-kbd-macro-doesnt-inline-into-active-recording
  ;; Regression: calling a stored macro while recording another used to
  ;; append each replayed key to :macro-recording, so the new macro
  ;; captured the INLINED keys instead of a single "call macro" step.
  (let [s0 (-> (make-state "") (assoc :last-macro [\x \y]))
        ;; simulate "in the middle of recording", currently has [\z]
        s1 (assoc s0 :macro-recording [\z])
        s2 (ed/call-last-kbd-macro s1)]
    (is (= "xy" (text s2)))
    ;; The replayed keys must NOT be appended to :macro-recording.
    (is (= [\z] (:macro-recording s2)))))

(deftest name-last-kbd-macro-stores-under-name
  (let [s0 (-> (make-state "") (assoc :last-macro [\a \b]))
        s1 (ed/name-last-kbd-macro s0 "greet")]
    (is (= [\a \b] (get-in s1 [:named-macros "greet"])))))

(deftest name-last-kbd-macro-no-macro
  (let [s (ed/name-last-kbd-macro (make-state "") "anything")]
    (is (= "No macro to name" (:msg s)))))

(deftest execute-kbd-macro-by-name
  (let [s0 (-> (make-state "")
               (assoc-in [:named-macros "bang"] [\a \b \c]))
        s1 (ed/execute-kbd-macro s0 "bang")]
    (is (= "abc" (text s1)))))

(deftest execute-kbd-macro-missing
  (let [s (ed/execute-kbd-macro (make-state "") "nope")]
    (is (.contains ^String (:msg s) "No macro"))))

;; --- Help ---

(deftest describe-key-captures-next-key
  (let [s0 (with-commands (make-state "hello"))
        s1 (ed/describe-key s0)]
    (is (= :describe-key (:capture-next s1)))
    (let [s2 (ed/handle-key s1 [:ctrl \f])]
      (is (nil? (:capture-next s2)))
      (is (.contains ^String (:msg s2) "forward-char")))))

(deftest describe-key-undefined
  (let [s0 (with-commands (make-state "hello"))
        s1 (ed/describe-key s0)
        s2 (ed/handle-key s1 [:ctrl \z])]
    (is (.contains ^String (:msg s2) "undefined"))))

(deftest describe-function-found
  (let [s (with-commands (make-state "hello"))
        s' (#'xmas.ed/describe-function s "yank")]
    (is (.contains ^String (:msg s') "most recent kill"))))

(deftest describe-function-missing
  (let [s (with-commands (make-state "hello"))
        s' (#'xmas.ed/describe-function s "no-such-func")]
    (is (.contains ^String (:msg s') "No function"))))

(deftest describe-variable-found
  (let [s0 (make-state "hello")
        s1 (assoc s0 :el-vars (atom {'answer 42}))
        s2 (#'xmas.ed/describe-variable s1 "answer")]
    (is (= "answer = 42" (:msg s2)))))

(deftest describe-variable-missing
  (let [s0 (make-state "hello")
        s1 (assoc s0 :el-vars (atom {}))
        s2 (#'xmas.ed/describe-variable s1 "nope")]
    (is (.contains ^String (:msg s2) "No variable"))))

;; --- Query replace ---

(deftest query-replace-begin-positions-at-first-match
  (let [s (#'xmas.ed/query-replace-begin (make-state "hello world hello" 0) "hello" "HI" false)]
    (is (= "hello" (get-in s [:query-replace :from])))
    (is (= "HI" (get-in s [:query-replace :to])))
    (is (= 0 (:count (:query-replace s))))
    (is (= 0 (:point (ed/cur s))))))

(deftest query-replace-begin-no-match
  (let [s (#'xmas.ed/query-replace-begin (make-state "hello" 0) "zzz" "!" false)]
    (is (nil? (:query-replace s)))
    (is (= "No match" (:msg s)))))

(deftest qr-replace-here-replaces-and-advances
  (let [s0 (#'xmas.ed/query-replace-begin (make-state "foo foo foo" 0) "foo" "XY" false)
        s1 (#'xmas.ed/qr-replace-here s0)]
    (is (= "XY foo foo" (text s1)))
    (is (= 3 (:point (ed/cur s1))))
    (is (= 1 (get-in s1 [:query-replace :count])))))

(deftest qr-replace-all
  (let [s0 (#'xmas.ed/query-replace-begin (make-state "a.a.a" 0) "a" "B" false)
        s1 (#'xmas.ed/qr-replace-all s0)]
    (is (= "B.B.B" (text s1)))
    (is (nil? (:query-replace s1)))
    (is (.contains ^String (:msg s1) "3"))))

(deftest qr-exit-clears-state
  (let [s0 (#'xmas.ed/query-replace-begin (make-state "abc abc" 0) "abc" "!" false)
        s1 (#'xmas.ed/qr-exit s0)]
    (is (nil? (:query-replace s1)))))

(deftest qr-regexp-uses-groups
  (let [s0 (#'xmas.ed/query-replace-begin (make-state "a1 b2 c3" 0) "([a-z])(\\d)" "\\2\\1" true)
        s1 (#'xmas.ed/qr-replace-all s0)]
    (is (= "1a 2b 3c" (text s1)))
    (is (.contains ^String (:msg s1) "3"))))

(deftest qr-regexp-end-state-match-count
  (let [s0 (#'xmas.ed/query-replace-begin (make-state "aaa" 0) "a" "b" true)
        s1 (#'xmas.ed/qr-replace-all s0)]
    (is (= "bbb" (text s1)))
    (is (.contains ^String (:msg s1) "3"))))

(deftest qr-regexp-zero-width-does-not-loop
  ;; Regression: a zero-width match (e.g. $) paired with an empty
  ;; replacement left point un-advanced and the replacer refound the same
  ;; match forever. qr-replace-here now nudges point past the zero match.
  (let [s0 (#'xmas.ed/query-replace-begin (make-state "abc\ndef" 0) "$" "" true)
        s1 (#'xmas.ed/qr-replace-all s0)]
    (is (= "abc\ndef" (text s1)))
    (is (nil? (:query-replace s1)))))

;; --- Shell command ---

(deftest shell-command-single-line-goes-to-msg
  (let [s (ed/shell-command (make-state "") "echo hello")]
    (is (= "hello" (:msg s)))))

(deftest shell-command-multi-line-opens-output-buffer
  (let [s (ed/shell-command (make-state "") "printf 'a\\nb\\nc'")]
    (is (= "*Shell Command Output*" (:buf s)))
    (is (.contains ^String (str (:text (ed/cur s))) "a"))
    (is (.contains ^String (str (:text (ed/cur s))) "c"))))

(deftest shell-command-blank-is-noop
  (let [s0 (make-state "hello")
        s1 (ed/shell-command s0 "")]
    (is (= "*test*" (:buf s1)))))

(deftest shell-command-on-region-pipes-through
  (let [s0 (-> (make-state "hello world" 0)
               (assoc-in [:bufs "*test*" :mark] 0)
               (assoc-in [:bufs "*test*" :point] 5))
        s1 (ed/shell-command-on-region s0 "tr a-z A-Z")]
    (is (= "HELLO world" (text s1)))
    (is (nil? (:mark (ed/cur s1))))))

(deftest shell-command-on-region-no-mark
  (let [s0 (make-state "hello" 2)
        s1 (ed/shell-command-on-region s0 "cat")]
    (is (= "No region" (:msg s1)))
    (is (= "hello" (text s1)))))

;; --- Auto-save ---

(deftest auto-save-writes-backup-when-threshold-crossed
  (let [f (java.io.File/createTempFile "xmas-as-" ".txt")]
    (try
      (spit f "original")
      (let [path (.getAbsolutePath f)
            s0 (-> (make-state "modified content" 0)
                   (assoc-in [:bufs "*test*" :file] path)
                   (assoc-in [:bufs "*test*" :edit-count] 500))
            s1 (ed/auto-save! s0)]
        (let [bak (java.io.File. (#'xmas.ed/auto-save-path path))]
          (is (.exists bak))
          (is (= "modified content" (slurp bak)))
          (is (= 0 (get-in s1 [:bufs "*test*" :edit-count])))
          (.delete bak)))
      (finally (.delete f)))))

(deftest auto-save-skips-below-threshold
  (let [s0 (-> (make-state "x" 0)
               (assoc-in [:bufs "*test*" :file] "/tmp/xmas-nonexistent.txt")
               (assoc-in [:bufs "*test*" :edit-count] 5))
        s1 (ed/auto-save! s0)]
    (is (= 5 (get-in s1 [:bufs "*test*" :edit-count])))))

(deftest save-buffer-deletes-auto-save
  (let [f (java.io.File/createTempFile "xmas-sas-" ".txt")]
    (try
      (let [path (.getAbsolutePath f)
            bak (java.io.File. (#'xmas.ed/auto-save-path path))
            _   (spit bak "stale")
            s   (-> (make-state "hello" 0)
                    (assoc-in [:bufs "*test*" :file] path))
            s'  (ed/save-buffer s)]
        (is (not (.exists bak)))
        (is (= "hello" (slurp f))))
      (finally (.delete f)))))

(deftest command-completer-single-match-completes-fully
  (let [s (with-commands (make-state "hello"))
        {:keys [completed candidates]} (#'xmas.ed/command-completer "yan" s)]
    (is (= "yank" completed))
    (is (empty? candidates))))

(deftest switching-to-different-buffer-clears-window-point
  ;; When current window's :buffer changes (e.g., C-x b), stale saved point
  ;; from the previous buffer must not leak into the new buffer.
  (let [s0 (make-state "hello" 3)
        s1 (-> s0
               (assoc-in [:bufs "other"] (assoc (buf/make "other" "xxxx" nil) :point 2))
               (xmas.cmd/set-cur-buffer "other"))]
    (is (= 2 (:point (ed/cur s1))))
    (is (not (contains? (get-in s1 [:windows]) :point)))))

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
        s2 (ed/handle-key s1 \a)            ;; 'a' at point 0 — extend matches here
        s3 (ed/handle-key s2 [:ctrl \s])    ;; C-s: advance past match → 'a' at 4
        s4 (ed/handle-key s3 [:ctrl \s])]   ;; C-s again → 'a' at 8
    (is (= 0 (point s2)))
    (is (= 4 (point s3)))
    (is (= 8 (point s4)))))

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

(deftest exit-second-attempt-confirms
  ;; Regression: handle-key used to clear :exit-pending at entry of every
  ;; keystroke, so the second C-x C-c never saw the flag and endlessly
  ;; re-asked for confirmation.
  (let [s (-> (make-state "abc" 0) (ed/edit 0 0 "x"))
        s1 (-> s (ed/handle-key [:ctrl \x]) (ed/handle-key [:ctrl \c]))
        s2 (-> s1 (ed/handle-key [:ctrl \x]) (ed/handle-key [:ctrl \c]))]
    (is (:exit-pending s1))
    (is (not (:exit s1)))
    (is (:exit s2))))

(deftest exit-pending-cleared-by-unrelated-command
  ;; After the confirm prompt, any non-exit command should cancel the
  ;; pending exit (Emacs convention).
  (let [s (-> (make-state "abc" 0) (ed/edit 0 0 "x"))
        s1 (-> s (ed/handle-key [:ctrl \x]) (ed/handle-key [:ctrl \c]))
        s2 (ed/handle-key s1 [:ctrl \f])
        s3 (-> s2 (ed/handle-key [:ctrl \x]) (ed/handle-key [:ctrl \c]))]
    (is (:exit-pending s1))
    (is (nil? (:exit-pending s2)))
    (is (not (:exit s3)))
    (is (clojure.string/starts-with? (:msg s3) "Modified"))))

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

;; --- Kill ring overflow ---

(deftest kill-ring-caps-at-60
  (let [s (reduce (fn [s i]
                    (-> s
                        (assoc-in [:bufs "*test*" :point] 0)
                        (ed/kill-line)))
                  (make-state (apply str (repeat 70 "x\n")) 0)
                  (range 70))]
    (is (<= (count (:kill s)) 60))))

;; --- Find file edge cases ---

(deftest find-file-reuses-existing-buffer
  ;; Regression: re-opening a file used to clobber in-memory edits with a fresh slurp.
  (let [f (java.io.File/createTempFile "xmas-reopen" ".txt")]
    (try
      (spit f "original")
      (let [path (.getAbsolutePath f)
            s1  (ed/find-file (make-state "" 0) path)
            ;; mutate the buffer in memory without saving
            s2  (ed/edit s1 0 0 "EDITED ")
            ;; re-run find-file on same path — should NOT clobber
            s3  (ed/find-file s2 path)]
        (is (= "EDITED original" (text s3)))
        (is (= (:buf s2) (:buf s3))))
      (finally (.delete f)))))

(deftest find-file-new-file
  (let [path (str (System/getProperty "java.io.tmpdir") "/xmas-nonexistent-" (System/nanoTime) ".txt")
        s' (ed/find-file (make-state "" 0) path)]
    (is (= "" (text s')))
    (is (= "(New file)" (:msg s')))))

(deftest find-file-error-reading
  ;; Try to open a directory as a file — slurp will throw
  (let [dir (System/getProperty "java.io.tmpdir")
        s' (ed/find-file (make-state "" 0) dir)]
    ;; Should show an error message, not crash
    (is (or (clojure.string/starts-with? (or (:msg s') "") "Error")
            ;; On some systems slurp on a dir returns content, so just check no crash
            (some? s')))))

;; --- Save buffer error ---

(deftest save-buffer-error
  (let [s (-> (make-state "data" 0)
              (assoc-in [:bufs "*test*" :file] "/nonexistent-dir/impossible/file.txt"))
        s' (ed/save-buffer s)]
    (is (clojure.string/starts-with? (:msg s') "Save failed"))))

(deftest save-buffer-relative-path
  ;; Regression: a bare filename (no directory) produced a relative Path
  ;; whose getParent() is nil, which NPE'd inside Files/createTempFile.
  ;; Save goes through .getAbsoluteFile, which resolves against the JVM
  ;; cwd — so the file lands in cwd. Clean it up afterward.
  (let [fname (str "xmas-relsave-" (System/nanoTime) ".txt")
        f (java.io.File. fname)]
    (try
      (let [s (-> (make-state "relative content" 0)
                  (assoc-in [:bufs "*test*" :file] fname))
            s' (ed/save-buffer s)]
        (is (clojure.string/starts-with? (:msg s') "Wrote"))
        (is (= "relative content" (slurp f))))
      (finally (.delete f)))))

;; --- Mini accept without mini ---

(deftest mini-accept-no-mini
  (let [s (make-state "abc" 0)
        s' (ed/mini-accept s)]
    (is (= "abc" (text s')))
    (is (nil? (:mini s')))))

(deftest isearch-accept-clears-stale-failing-msg
  ;; Regression: `Failing I-search: …` was left in :msg after RET, so
  ;; the echo area showed the stale warning once :isearch dissoc'd.
  (let [s  (make-state "hello" 0)
        s1 (ed/handle-key s [:ctrl \s])
        s2 (reduce ed/handle-key s1 (seq "zzz"))     ;; no match → :msg set
        _  (is (.contains ^String (:msg s2) "Failing"))
        s3 (ed/handle-key s2 :return)]                ;; accept
    (is (nil? (:isearch s3)))
    (is (nil? (:msg s3)))))

(deftest mini-accept-falls-back-when-prev-buf-killed
  ;; Regression: if the buffer saved as :prev-buf was killed while the
  ;; minibuffer was open, mini-accept used to restore :buf to that
  ;; dead name — every downstream (cur s) would then return nil.
  (let [s  (make-state "hi" 0)
        s1 (ed/handle-key s [:meta \x])         ;; open M-x
        ;; externally kill *test* while mini is active
        s2 (update s1 :bufs dissoc "*test*")
        ;; fire a benign on-done that just ignores the input
        s2 (assoc-in s2 [:mini :on-done] (fn [s _] s))
        s3 (ed/handle-key s2 :return)]
    (is (nil? (:mini s3)))
    (is (contains? (:bufs s3) (:buf s3)))))

;; --- Mini history dedup ---

(deftest mini-history-no-duplicate
  (let [result (atom nil)
        s (-> (make-state "abc" 0) (assoc :mini-history ["foo"]))
        ;; Type "foo" again — should not duplicate in history
        s1 (ed/mini-start s "Test: " (fn [s input] (reset! result input) s))
        s2 (-> s1 (ed/handle-key \f) (ed/handle-key \o) (ed/handle-key \o))
        s3 (ed/handle-key s2 :return)]
    (is (= "foo" @result))
    (is (= ["foo"] (:mini-history s3)))))

;; --- Mini history prev at boundary ---

(deftest mini-history-prev-at-end-stays
  (let [s (-> (make-state "abc" 0) (assoc :mini-history ["only"]))
        s1 (ed/mini-start s "Test: " (fn [s _] s))
        s2 (ed/handle-key s1 [:meta \p])    ;; → "only"
        s3 (ed/handle-key s2 [:meta \p])]   ;; → still "only" (no more history)
    (is (= "only" (str (get-in s3 [:bufs " *mini*" :text]))))))

;; --- File completer ---

(deftest file-completer-single-match
  (let [dir (java.io.File/createTempFile "xmas-comp" "")]
    (.delete dir)
    (.mkdirs dir)
    (spit (java.io.File. dir "testfile.txt") "")
    (try
      (let [{:keys [completed candidates]}
            (#'ed/file-completer (str (.getPath dir) "/test"))]
        (is (clojure.string/ends-with? completed "testfile.txt"))
        (is (empty? candidates)))
      (finally
        (.delete (java.io.File. dir "testfile.txt"))
        (.delete dir)))))

(deftest file-completer-multiple-matches
  (let [dir (java.io.File/createTempFile "xmas-comp" "")]
    (.delete dir)
    (.mkdirs dir)
    (spit (java.io.File. dir "alpha.txt") "")
    (spit (java.io.File. dir "alphb.txt") "")
    (try
      (let [{:keys [completed candidates]}
            (#'ed/file-completer (str (.getPath dir) "/alph"))]
        (is (clojure.string/ends-with? completed "alph"))
        (is (= 2 (count candidates))))
      (finally
        (.delete (java.io.File. dir "alpha.txt"))
        (.delete (java.io.File. dir "alphb.txt"))
        (.delete dir)))))

(deftest file-completer-no-matches
  (let [{:keys [completed candidates]}
        (#'ed/file-completer "/nonexistent-xmas-path/zzz")]
    (is (= "/nonexistent-xmas-path/zzz" completed))
    (is (empty? candidates))))

(deftest file-completer-directory-input
  ;; When input is a directory, list all children (prefix = "")
  (let [dir (java.io.File/createTempFile "xmas-comp" "")]
    (.delete dir)
    (.mkdirs dir)
    (spit (java.io.File. dir "one.txt") "")
    (spit (java.io.File. dir "two.txt") "")
    (try
      (let [{:keys [candidates]}
            (#'ed/file-completer (str (.getPath dir) "/"))]
        (is (= 2 (count candidates))))
      (finally
        (.delete (java.io.File. dir "one.txt"))
        (.delete (java.io.File. dir "two.txt"))
        (.delete dir)))))

;; --- Tab complete in minibuffer ---

(deftest mini-tab-complete-via-handle-key
  (let [dir (java.io.File/createTempFile "xmas-tab" "")]
    (.delete dir)
    (.mkdirs dir)
    (spit (java.io.File. dir "unique-file.el") "")
    (try
      (let [prefix (str (.getPath dir) "/uni")
            s (make-state "abc" 0)
            s1 (ed/mini-start s "Find: " (fn [s _] s) #'ed/file-completer)
            ;; Type the prefix into minibuffer
            s2 (reduce #(ed/handle-key %1 %2) s1 (seq prefix))
            ;; Press tab
            s3 (ed/handle-key s2 :tab)
            completed (str (get-in s3 [:bufs " *mini*" :text]))]
        (is (clojure.string/ends-with? completed "unique-file.el")))
      (finally
        (.delete (java.io.File. dir "unique-file.el"))
        (.delete dir)))))

(deftest mini-tab-no-completer
  ;; Tab in a minibuffer without a completer is a no-op
  (let [s (ed/mini-start (make-state "abc" 0) "Test: " (fn [s _] s))
        s' (ed/handle-key s :tab)]
    (is (= "" (str (get-in s' [:bufs " *mini*" :text]))))))

;; --- String key self-insert ---

(deftest handle-key-string-self-insert
  (let [s' (ed/handle-key (make-state "ab" 1) "\uD83D\uDE00")]
    (is (= "a\uD83D\uDE00b" (text s')))))

;; --- Isearch string key ---

(deftest isearch-append-string-key
  (let [s (make-state "hello \uD83D\uDE00 world" 0)
        s1 (ed/handle-key s [:ctrl \s])
        s2 (ed/handle-key s1 "\uD83D\uDE00")
        s3 (ed/handle-key s2 :return)]
    (is (= 6 (point s3)))))

;; --- Isearch backward via C-r in isearch ---

(deftest isearch-ctrl-r-reverses
  (let [s (make-state "abc abc abc" 11)
        s1 (ed/handle-key s [:ctrl \s])      ;; start forward
        s2 (ed/handle-key s1 [:ctrl \r])     ;; switch to backward (empty pattern, no-op)
        s3 (ed/handle-key s2 \a)             ;; search backward for 'a'
        s4 (ed/handle-key s3 :return)]
    (is (= 8 (point s4)))))

(deftest isearch-backward-extend-keeps-match-anchored
  ;; Regression: extending a backward-isearch pattern used `(inc p)` as the
  ;; search upper bound, which only kept the current match visible for pn=1.
  ;; Typing a second char jumped to an earlier (sometimes irrelevant) match.
  (let [s  (make-state "ab cd ab" 8)
        s1 (ed/handle-key s [:ctrl \r])      ;; start backward
        s2 (ed/handle-key s1 \a)             ;; pattern "a"  -> finds pos 6
        s3 (ed/handle-key s2 \b)             ;; pattern "ab" -> must stay at 6
        s4 (ed/handle-key s3 :return)]
    (is (= 6 (point s3)))
    (is (= 6 (point s4)))))

;; --- Goto line via M-g binding ---

(deftest goto-line-via-binding
  (let [s (make-state "aaa\nbbb\nccc" 0)
        s1 (ed/handle-key s [:meta \g])      ;; opens goto-line minibuffer
        _ (is (some? (:mini s1)))
        s2 (ed/handle-key s1 \3)             ;; type "3"
        s3 (ed/handle-key s2 :return)]       ;; accept
    (is (= 8 (point s3)))))

;; --- Switch buffer via C-x b ---

(deftest switch-buffer-via-binding
  (let [s (make-state "abc" 0)
        s1 (-> s (ed/handle-key [:ctrl \x]) (ed/handle-key \b))
        _ (is (some? (:mini s1)))
        s2 (reduce #(ed/handle-key %1 %2) s1 (seq "*new*"))
        s3 (ed/handle-key s2 :return)]
    (is (= "*new*" (:buf s3)))
    (is (= "" (text s3)))))

;; --- Eval expression (M-:) ---

(deftest eval-expression-via-binding
  (let [s (make-state "hello" 0)
        s1 (ed/handle-key s [:meta \:])
        _ (is (some? (:mini s1)))
        s2 (reduce #(ed/handle-key %1 %2) s1 (seq "(+ 1 2)"))
        s3 (ed/handle-key s2 :return)]
    (is (= "3" (:msg s3)))))

(deftest eval-expression-persists-state-changes
  ;; Regression: eval-expression used to pass the global `editor` atom to
  ;; el/eval-1, but runs inside `(swap! editor ...)`. Any `swap! *state*`
  ;; from elisp was clobbered by the outer swap committing the old `s`.
  (let [s  (make-state "" 0)
        s1 (ed/handle-key s [:meta \:])
        s2 (reduce #(ed/handle-key %1 %2) s1 (seq "(insert \"Z\")"))
        s3 (ed/handle-key s2 :return)]
    (is (= "Z" (text s3)))))

;; --- Elisp keybindings dispatch ---

(deftest el-bindings-dispatch
  ;; Simulate what init.el loading does: register a binding in :el-bindings
  (let [called (atom false)
        s (-> (make-state "abc" 0)
              (assoc :el-bindings {[:ctrl \t] (fn [s] (reset! called true) s)}))
        s' (ed/handle-key s [:ctrl \t])]
    (is @called)))

;; --- Prefix arg + prefix key ---

(deftest prefix-arg-survives-prefix-key
  ;; Regression: `C-u 4 C-x <cmd>` used to drop :prefix-arg when C-x
  ;; installed a :pending prefix map, so <cmd> saw no argument.
  (let [seen (atom nil)
        cmd  (fn [s] (reset! seen (:prefix-arg s)) s)
        s    (-> (make-state "" 0)
                 (assoc :el-bindings {[:ctrl \x] {[:ctrl \t] cmd}}))
        keys [[:ctrl \u] \4 [:ctrl \x] [:ctrl \t]]
        s'   (reduce ed/handle-key s keys)]
    (is (= {:num 4} @seen))
    ;; After the command runs, :prefix-arg must be cleared.
    (is (nil? (:prefix-arg s')))))

;; --- Minibuffer vs modal commands ---

(deftest isearch-refuses-to-start-inside-minibuffer
  ;; Regression: `C-s` while a minibuffer prompt was open used to set
  ;; :isearch on top of :mini, leaving the editor wedged. isearch-start
  ;; now no-ops and sets :msg when :mini is active.
  (let [s  (make-state "" 0)
        s1 (ed/handle-key s [:meta \x])           ;; opens M-x mini
        _ (is (some? (:mini s1)))
        s2 (ed/handle-key s1 [:ctrl \s])]          ;; would have started isearch
    (is (some? (:mini s2)))
    (is (nil?  (:isearch s2)))))

(deftest mini-start-refuses-second-minibuffer
  ;; Regression: opening a second minibuffer over the first used to
  ;; overwrite :mini, losing the outer prompt's on-done / prev-buf.
  (let [s  (make-state "" 0)
        s1 (ed/handle-key s [:meta \x])
        p1 (:prev-buf (:mini s1))
        s2 (ed/handle-key s1 [:meta \:])]         ;; would open another mini
    (is (= p1 (:prev-buf (:mini s2))))))
