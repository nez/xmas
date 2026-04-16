# Elisp Plugin Support — Remaining Phases

## Status

| Phase | Scope | Status |
|-------|-------|--------|
| 1 | Lexical closures, condition-case/unwind-protect/signal | Done |
| 2 | Buffer-local variables, hooks, define-minor-mode | Done |
| 3 | Keymaps with inheritance, interactive commands | Pending |
| 4 | Regular expressions, text properties | Pending |
| 5 | Hash tables, autoload, cl-lib | Pending |
| 6 | Timers, async processes | Pending |

## What's Implemented (Phases 1-2)

**Special forms:** quote, backquote, if, cond, progn, setq, let, let\*, defun, defmacro, defvar, defcustom, lambda, while, and, or, save-excursion, condition-case, unwind-protect

**Builtins (48):** +, -, \*, /, mod, <, >, =, <=, >=, 1+, 1-, cons, car, cdr, list, length, nth, null, append, reverse, concat, substring, format, string-to-number, number-to-string, char-to-string, numberp, stringp, listp, consp, symbolp, equal, not, boundp, fboundp, functionp, intern, symbol-name, apply, funcall, mapcar, error, signal, point, point-min, point-max, goto-char, forward-char, backward-char, beginning-of-line, end-of-line, insert, delete-region, delete-char, buffer-string, buffer-name, search-forward, search-backward, message, global-set-key, provide, require, load, load-file, featurep, put, get, plist-get, plist-put, macroexpand, make-local-variable, make-variable-buffer-local, buffer-local-value, local-variable-p, add-hook, remove-hook, run-hooks, run-hook-with-args

**subr.el macros:** when, unless, dolist, dotimes, prog1, prog2, push, pop, ignore-errors, setq-local, define-minor-mode

**subr.el functions:** cadr, caddr, caar, cddr, cdar, member, assoc, assq, nthcdr

---

## Phase 3 — Keymaps + Interactive Commands

**Goal:** Per-mode keybindings with inheritance. Commands that read arguments from the user.

### 3a. Keymap Objects

A keymap is a map with optional parent for inheritance lookup.

```
{:bindings {key -> command-or-keymap}
 :parent   keymap-or-nil}
```

**New builtins:**

| Function | Signature | Behavior |
|----------|-----------|----------|
| `make-sparse-keymap` | `()` | Returns `{:bindings {} :parent nil}` |
| `define-key` | `(keymap key def)` | `assoc-in keymap [:bindings key] def` |
| `set-keymap-parent` | `(keymap parent)` | `assoc keymap :parent parent` |
| `lookup-key` | `(keymap key)` | Walk chain: bindings then parent |
| `current-local-map` | `()` | Return current buffer's mode keymap |
| `current-global-map` | `()` | Return global keymap |
| `use-local-map` | `(keymap)` | Set current buffer's keymap |

**ed.clj changes:**
- Migrate flat `bindings` map to a keymap object stored as global-map
- `handle-key` lookup chain: active minor-mode maps -> buffer major-mode map -> global map -> el-bindings
- `define-minor-mode` in subr.el updated to accept `:keymap` argument

**Estimated:** ~50 lines Clojure, ~10 lines subr.el

### 3b. Interactive Declarations

Store `interactive` spec on functions. `call-interactively` reads args from minibuffer.

**New special form handling:**
- `(interactive "spec")` at start of defun body -> stored as `:interactive` on fn def

**New builtins:**

| Function | Signature | Behavior |
|----------|-----------|----------|
| `interactive-p` | `()` | True if called interactively |
| `call-interactively` | `(fn)` | Read args per interactive spec, then call |
| `completing-read` | `(prompt collection)` | Minibuffer with tab-complete over collection |
| `read-string` | `(prompt)` | Read string from minibuffer |
| `read-from-minibuffer` | `(prompt)` | Alias for read-string |

**Interactive spec codes (subset):**

| Code | Meaning |
|------|---------|
| `s` | Read string |
| `b` | Read buffer name (completing) |
| `f` | Read file name |
| `r` | Region: (point, mark) |
| `p` | Prefix arg as number |
| `P` | Raw prefix arg |

**Estimated:** ~40 lines Clojure

---

## Phase 4 — Regular Expressions + Text Properties

**Goal:** Regex matching (the backbone of syntax highlighting and text processing). Text properties for metadata on character ranges.

### 4a. Regular Expressions

Translate Emacs regex -> Java regex at match time. Store match data for group extraction.

**Translation rules:**

| Emacs | Java |
|-------|------|
| `\(` `\)` | `(` `)` (group delimiters swapped) |
| `\|` | `\|` |
| `\w` `\W` | `\w` `\W` |
| `\b` | `\b` |
| `\<` `\>` | `\b` (approximate) |
| `\s-` | `\s` (whitespace) |
| `\1`..`\9` | `\1`..`\9` (backrefs) |

**New builtins:**

| Function | Signature | Behavior |
|----------|-----------|----------|
| `string-match` | `(regexp string &optional start)` | Match, set match-data, return index or nil |
| `match-string` | `(n &optional string)` | Extract nth group from last match |
| `match-beginning` | `(n)` | Start of nth group |
| `match-end` | `(n)` | End of nth group |
| `replace-regexp-in-string` | `(regexp rep string)` | Global replace |
| `looking-at` | `(regexp)` | Match at point in buffer |
| `re-search-forward` | `(regexp &optional bound)` | Search from point |
| `re-search-backward` | `(regexp &optional bound)` | Search backward from point |
| `match-data` | `()` | Return match data list |
| `set-match-data` | `(data)` | Set match data |

**State:** Match data stored in `*vars*` as `:xmas/match-data` — vector of `[start end]` pairs.

**Estimated:** ~60 lines (translator ~20, builtins ~40)

### 4b. Text Properties

Metadata on character ranges: faces, invisible, read-only, etc.

**Buffer addition:** `:props` — sorted-map of `{position -> property-map}` (interval starts).

**New builtins:**

| Function | Signature | Behavior |
|----------|-----------|----------|
| `put-text-property` | `(start end prop val)` | Set property on range |
| `get-text-property` | `(pos prop)` | Get property at position |
| `remove-text-properties` | `(start end props)` | Remove props from range |
| `propertize` | `(string &rest props)` | Return string with properties |
| `text-properties-at` | `(pos)` | All properties at position |
| `next-property-change` | `(pos)` | Next position where props change |
| `add-face-text-property` | `(start end face)` | Add face to range |

**Internal:** `buf/edit` calls a property-shift helper to keep intervals aligned after insertions/deletions.

**view.clj integration:** Read `:props` per-line to apply faces during rendering.

**Estimated:** ~80 lines (intervals ~40, builtins ~25, view integration ~15)

---

## Phase 5 — Hash Tables + Autoload + cl-lib

**Goal:** Data structures and compatibility layer for the package ecosystem.

### 5a. Hash Tables

Emacs hash tables are mutable. Wrap a Clojure atom.

```clojure
(defrecord ElHashTable [data test])
;; data = atom of map, test = 'equal or 'eq
```

**New builtins:**

| Function | Signature | Behavior |
|----------|-----------|----------|
| `make-hash-table` | `(&rest args)` | Create (parse :test keyword) |
| `gethash` | `(key table &optional default)` | Lookup |
| `puthash` | `(key value table)` | Insert/update |
| `remhash` | `(key table)` | Remove |
| `maphash` | `(fn table)` | Iterate |
| `hash-table-count` | `(table)` | Size |
| `hash-table-p` | `(obj)` | Type check |
| `hash-table-keys` | `(table)` | All keys as list |
| `hash-table-values` | `(table)` | All values as list |

**Estimated:** ~30 lines

### 5b. Autoload

Lazy-load marker in `*fns*`. On first call, load the file, then retry.

```clojure
;; autoload stores a marker:
(swap! *fns* assoc fn-sym {:autoload file-name})

;; resolve-fn checks for autoload:
(when (:autoload f) (el-load (:autoload f)) (get @*fns* sym))
```

**New builtins:** `autoload`

**Estimated:** ~15 lines

### 5c. cl-lib

Elisp file loaded on `(require 'cl-lib)`. Implements Common Lisp extensions as macros/functions.

**Priority subset (covers most plugin usage):**

| Name | Type | Notes |
|------|------|-------|
| `cl-defun` | macro | Alias for defun (skip extended arglist for now) |
| `cl-case` | macro | Like cond but matching on value |
| `cl-loop` | macro | Start with `for`/`collect`/`while` subset |
| `cl-remove-if` | function | Filter with negated predicate |
| `cl-remove-if-not` | function | Filter with predicate |
| `cl-find-if` | function | First matching element |
| `cl-position` | function | Index of element |
| `cl-reduce` | function | Fold |
| `cl-mapcar` | function | Multi-list map |
| `cl-destructuring-bind` | macro | Destructure list into variables |
| `cl-incf` / `cl-decf` | macro | In-place increment/decrement |
| `cl-block` / `cl-return-from` | macro | Non-local exit (via condition-case) |
| `cl-labels` | macro | Local function definitions |

**Estimated:** ~120 lines Elisp in `resources/xmas/cl-lib.el`

---

## Phase 6 — Timers + Async Processes

**Goal:** Background execution — unlocks magit, lsp-mode, flycheck, auto-complete.

### 6a. Timers

Scheduled function execution via a timer thread.

```clojure
;; Timer = {:id uuid :fn sym :delay-ms long :repeat-ms nil-or-long :next-fire-ms long}
;; Stored in *timers* atom. Timer thread checks every 100ms.
```

**New builtins:**

| Function | Signature | Behavior |
|----------|-----------|----------|
| `run-with-timer` | `(secs repeat fn &rest args)` | One-shot or repeating timer |
| `run-with-idle-timer` | `(secs repeat fn &rest args)` | Fire after idle period |
| `cancel-timer` | `(timer)` | Remove from timer list |
| `timerp` | `(obj)` | Type check |

**ed.clj integration:** Timer thread fires expired timers by calling `el/run-hooks!`-style into editor atom. Command loop already has 100ms poll — timers piggyback on that.

**Estimated:** ~35 lines

### 6b. Async Processes

Launch external programs, capture output through filters.

```clojure
;; Process = {:proc ProcessBuilder :name str :buffer str :filter fn :sentinel fn}
;; stdout reader thread delivers lines through filter function
;; on exit, sentinel is called
```

**New builtins:**

| Function | Signature | Behavior |
|----------|-----------|----------|
| `start-process` | `(name buffer program &rest args)` | Async process |
| `call-process` | `(program &optional infile dest display &rest args)` | Sync process |
| `process-send-string` | `(process string)` | Write to stdin |
| `set-process-filter` | `(process fn)` | Set output filter |
| `set-process-sentinel` | `(process fn)` | Set exit handler |
| `process-buffer` | `(process)` | Associated buffer |
| `process-status` | `(process)` | :run, :exit, :signal |
| `delete-process` | `(process)` | Kill process |
| `shell-command-to-string` | `(command)` | Capture output |

**Estimated:** ~70 lines

---

## Dependency Graph

```
Phase 3 (keymaps, interactive)
  |
  +---> Phase 4 (regex, text-props)
  |
  +---> Phase 5 (hash, autoload, cl-lib)
  |
  +---> Phase 6 (timers, processes)
```

Phases 4-6 are independent of each other. All depend on Phase 3 for the mode/keymap infrastructure.

## Milestones

**A — Simple plugins work** (after Phase 3):
A `define-minor-mode` form with its own keymap evaluates and works. `M-x` command execution with argument reading. Per-mode keybindings override globals.

**B — Text processing plugins** (after Phase 4):
Font-lock (syntax highlighting) possible. Regex-based search/replace plugins work. Overlays can annotate text.

**C — Package ecosystem** (after Phase 5):
`use-package` style configuration works. Hash-table-heavy plugins (helm, ivy, magit data) function. cl-lib dependent packages load.

**D — External tool integration** (after Phase 6):
magit (git), flycheck (linters), lsp-mode (language servers), vterm/shell-mode all have the primitives they need.

## Line Count Estimates

| Phase | Clojure | Elisp | Total |
|-------|---------|-------|-------|
| 3 | ~90 | ~10 | ~100 |
| 4 | ~140 | 0 | ~140 |
| 5 | ~45 | ~120 | ~165 |
| 6 | ~105 | 0 | ~105 |
| **Total** | **~380** | **~130** | **~510** |
