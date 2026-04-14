# xmas — Road to Minimal Emacs

## Current State (v0.1)

Working text editor with Emacs keybindings. ~1300 LOC, 283 tests, gap buffer with line index, elisp reader.

## Phase: Elisp Evaluator (v0.2) — IN PROGRESS

**Goal**: Run user config from `~/.xmas/init.el`. Define commands, bind keys, manipulate buffers.

### Evaluator (~120 LOC)

- eval/apply loop with dynamic scope, Lisp-2 (separate var/fn namespaces)
- Special forms: quote, if, cond, progn, setq, let, defun, lambda, while, and, or
- Built-ins: arithmetic (+, -, *, /, mod, <, >, =, <=, >=), list (cons, car, cdr, list, length, nth, null, append), string (concat, substring, format), predicates (numberp, stringp, listp, symbolp, equal)

### Buffer Bridge (~40 LOC)

- point, point-min, point-max, goto-char, forward-char, backward-char
- beginning-of-line, end-of-line, insert, delete-region
- buffer-string, buffer-name, search-forward, search-backward, message

### Keybinding Bridge (~30 LOC)

- global-set-key: parse Emacs key strings, wrap elisp fn, insert into bindings
- Key string format: \C-x = ctrl, \M-x = meta

### Integration (~20 LOC)

- Init file: load ~/.xmas/init.el at startup, eval each form
- M-: (eval-expression): minibuffer prompt, eval, show result in echo area

### Success Criteria

```elisp
;; ~/.xmas/init.el
(defun kill-whole-line ()
  (beginning-of-line)
  (let ((start (point)))
    (end-of-line)
    (forward-char)
    (delete-region start (point))))

(global-set-key "\C-ck" 'kill-whole-line)
(message "Config loaded")
```

---

## Phase: Tier 1 Features (v0.3)

**Goal**: An editor people would choose to use for small tasks.

### Syntax Highlighting (~200 LOC)

- Face/property system on buffer text
- Major mode hooks for language detection
- At minimum: comments, strings, keywords for Clojure/elisp

### Multiple Windows (~300 LOC)

- C-x 2 (split horizontal), C-x 3 (split vertical), C-x o (other window)
- View becomes a window tree, each window has its own buffer/scroll/point
- C-x 0 (delete window), C-x 1 (delete other windows)

### Modes (~100 LOC)

- Major modes: mode-specific keymaps + hooks
- Minor modes: toggleable overlays (e.g., line-numbers, auto-save)
- Mode-line displays current major + active minor modes

### Dired (~150 LOC)

- C-x d: directory listing buffer
- Enter to open, d to mark delete, x to execute
- Navigation, refresh

### Region Highlighting (~50 LOC)

- Visual feedback for mark-to-point region
- Reverse video or color overlay for selected text

---

## Phase: Emacs Completeness (v0.4)

### Commands & Input

- M-x interactive command dispatch
- Universal argument (C-u prefix)
- Keyboard macros (C-x (, C-x ), C-x e)
- Query replace (M-%)
- Buffer list (C-x C-b)

### Help System

- C-h k (describe-key)
- C-h f (describe-function)
- C-h v (describe-variable)

### Extensibility

- Hooks (add-hook, run-hooks)
- Advice (defadvice / add-function)
- Customization (defcustom, defgroup)
- Minibuffer completion with candidates

### Infrastructure

- Auto-save / crash recovery (#file#)
- Process/shell integration (M-!, shell-command)
- Text properties / overlays (foundation for real highlighting)
- Package manager
