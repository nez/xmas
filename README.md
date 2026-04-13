# xmas

A minimal Emacs in Clojure.

## Architecture

Everything is a buffer, every action is a state transition.

### [`xmas.buf`](src/xmas/buf.clj) — Buffer data type

A buffer is an immutable map: `{:name :text :point :mark :file :modified :mode :undo}` — a string, a cursor position (integer offset), and metadata.

Three operations define the type:

- [**`make`**](src/xmas/buf.clj#L3) — Constructor. Name, optional text, optional file path.
- [**`edit`**](src/xmas/buf.clj#L13-L24) — The single edit primitive. Splices `repl` into `text[from..to]`, adjusts point, records undo, marks modified. Every mutation (insert, delete, kill) bottoms out here.
- [**`undo`**](src/xmas/buf.clj#L26-L32) — Pops the last edit. Returns `nil` when empty.

[**`set-point`**](src/xmas/buf.clj#L9-L11) clamps a movement function's result to valid range — all cursor movement goes through it.

### [`xmas.text`](src/xmas/text.clj) — Text primitives

Pure functions on strings. No editor state, no buffers — just text and positions.

- [**`next-pos`**](src/xmas/text.clj#L3) / [**`prev-pos`**](src/xmas/text.clj#L9) — Step by one Unicode codepoint (handles surrogate pairs).
- [**`line-start`**](src/xmas/text.clj#L15) / [**`line-end`**](src/xmas/text.clj#L21) — Find newline boundaries around a position.
- [**`word-forward`**](src/xmas/text.clj#L51) / [**`word-backward`**](src/xmas/text.clj#L61) — Scan to next/previous word boundary.
- [**`char-width`**](src/xmas/text.clj#L27) — Terminal display width of a codepoint (2 for CJK/fullwidth, 0 for combining marks, 1 otherwise).
- [**`display-width`**](src/xmas/text.clj#L43) — Total terminal columns for a text range.
- [**`pos-at-col`**](src/xmas/text.clj#L71) — Find the position at a target display column.

### [`xmas.ed`](src/xmas/ed.clj) — Editor core

A single [atom](src/xmas/ed.clj#L13) holds the entire editor state:

```clojure
{:buf "*scratch*"          ;; name of active buffer
 :bufs {"*scratch*" {...}} ;; all buffers by name
 :kill []                  ;; kill ring (clipboard stack)
 :msg nil                  ;; echo area message
 :mini nil                 ;; minibuffer state (when prompting)
 :scroll 0, :rows R, :cols C, :last-key K}
```

[`cur`](src/xmas/ed.clj#L15) and [`update-cur`](src/xmas/ed.clj#L16) bridge editor state to the current buffer. [`edit`](src/xmas/ed.clj#L19) and [`set-point`](src/xmas/ed.clj#L18) delegate to `buf/` through this bridge.

**Commands are pure `state -> state` functions.** [`forward-char`](src/xmas/ed.clj#L23), [`kill-line`](src/xmas/ed.clj#L64), [`yank`](src/xmas/ed.clj#L80), [`save-buffer`](src/xmas/ed.clj#L97) — they all take editor state and return new state. No side effects except `save-buffer` (which spits to disk).

**Minibuffer** — Prompts (find-file, switch-buffer) reuse the buffer abstraction. [`mini-start`](src/xmas/ed.clj#L116) creates a temporary buffer named `" *mini*"` and swaps it in as active. Normal editing commands just work inside it. [`mini-accept`](src/xmas/ed.clj#L123) reads the input and calls the callback.

**Keybindings** — A [nested map](src/xmas/ed.clj#L131-L146). Simple keys map to command functions. `[:ctrl \x]` maps to a sub-map for prefix sequences (`C-x C-f`, `C-x C-s`, etc.). [`handle-key`](src/xmas/ed.clj#L150) dispatches, with chars falling through to `self-insert`.

### [`xmas.term`](src/xmas/term.clj) — Terminal I/O

Raw terminal control without JLine. Two halves:

- **Input**: [`enter-raw-mode!`](src/xmas/term.clj#L19) via `stty`, then reads bytes from `System.in`. [`read-key`](src/xmas/term.clj#L83-L96) decodes raw bytes into semantic keys — chars, `[:ctrl \f]`, `[:meta \b]`, `:up`, `:backspace`, etc. Multi-byte UTF-8 is [decoded](src/xmas/term.clj#L41-L57) into codepoints. CSI escape sequences are [parsed](src/xmas/term.clj#L60-L69) for arrow keys and special keys.

- **Output**: Thin wrappers over ANSI escape codes — [`move`](src/xmas/term.clj#L106), [`cls`](src/xmas/term.clj#L108), [`clreol`](src/xmas/term.clj#L107), [`sg`](src/xmas/term.clj#L113-L115) (set graphics/colors). Everything writes to `System.out` directly.

### [`xmas.view`](src/xmas/view.clj) — Screen rendering

Converts editor state into terminal output:

- [`cursor-pos`](src/xmas/view.clj#L10-L20) — Maps a text offset to a `[row, col]` screen position relative to scroll.
- [`render`](src/xmas/view.clj#L46-L87) — Draws the full screen: text lines, a mode line (Emacs-style `-- *scratch* (fundamental)`), and the minibuffer/echo area. Returns the new scroll position.

### [`xmas.log`](src/xmas/log.clj) — Debug logging

Append-only [file logger](src/xmas/log.clj#L9-L15) to `~/.xmas/debug.log`. Every keypress byte and decoded key is [logged](src/xmas/log.clj#L17-L21).

## nREPL — live hacking

xmas starts an nREPL server on port 7888. Connect from Emacs/CIDER:

```
M-x cider-connect RET localhost RET 7888
```

Or from the command line:

```
emacsclient -e '(cider-connect-clj (list :host "localhost" :port 7888))'
```

Once connected, the running editor is fully inspectable and modifiable:

```clojure
;; inspect state
(keys @xmas.ed/editor)
(:text (xmas.ed/cur @xmas.ed/editor))

;; inject text into the live editor
(swap! xmas.ed/editor xmas.ed/edit 0 0 "hello from emacs!\n")

;; add a keybinding at runtime
(alter-var-root #'xmas.ed/bindings assoc [:ctrl \t] xmas.ed/next-line)

;; define a new command and bind it
(defn my-cmd [s] (xmas.ed/msg s "it works"))
(alter-var-root #'xmas.ed/bindings assoc [:meta \z] my-cmd)

;; open a file programmatically
(swap! xmas.ed/editor xmas.ed/find-file "/tmp/test.txt")

;; switch buffer
(swap! xmas.ed/editor xmas.ed/switch-buffer "*scratch*")
```

Every command is a pure `state -> state` function. `swap!` applies it atomically. Changes appear on the next redisplay cycle (any keypress in xmas).

## Data flow

```
read-key -> handle-key(state, key) -> new state -> render(state) -> terminal
     ^                                                                  |
     '----------------------- command-loop ----------------------------'
```

The [main loop](src/xmas/ed.clj#L169-L177) is tight: read a key, dispatch to a command, get new state, render, repeat. The atom is only `reset!`'d at the loop boundary — commands themselves are pure.
