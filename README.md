# xmas

A minimal Emacs in Clojure.

## Architecture

The editor follows Emacs's core philosophy: everything is a buffer, every action is a state transition.

### [`xmas.ed`](src/xmas/ed.clj) — Editor core

A single [atom](src/xmas/ed.clj#L11) holds the entire editor state as an immutable map:

```clojure
{:buf "*scratch*"          ;; name of active buffer
 :bufs {"*scratch*" {...}} ;; all buffers by name
 :kill []                  ;; kill ring (clipboard stack)
 :msg nil                  ;; echo area message
 :mini nil                 ;; minibuffer state (when prompting)
 :scroll 0, :rows R, :cols C, :last-key K}
```

Each [buffer](src/xmas/ed.clj#L17-L21) is also a map: `{:text :point :mark :file :modified :mode :undo}` — a string, a cursor position (integer offset), and metadata.

[**`edit`**](src/xmas/ed.clj#L32-L44) — The single edit primitive. Every mutation (insert, delete, kill) bottoms out here. It splices `repl` into `text[from..to]`, adjusts the point, records undo, and marks the buffer modified. One function, one place to get editing right.

[**`set-point`**](src/xmas/ed.clj#L23-L26) — All cursor movement goes through this. Takes a function `(text, point) -> new-point` and clamps to valid range. Keeps movement logic decoupled from bounds checking.

**Commands are pure `state -> state` functions.** [`forward-char`](src/xmas/ed.clj#L60), [`kill-line`](src/xmas/ed.clj#L110-L116), [`yank`](src/xmas/ed.clj#L126-L129), [`save-buffer`](src/xmas/ed.clj#L139-L144) — they all take the editor state and return new state. No side effects except `save-buffer` (which spits to disk).

**Minibuffer** — Prompts (find-file, switch-buffer) reuse the buffer abstraction. [`mini-start`](src/xmas/ed.clj#L160-L165) creates a temporary buffer named `" *mini*"` and swaps it in as active. Normal editing commands just work inside it. [`mini-accept`](src/xmas/ed.clj#L167-L173) reads the input and calls the callback.

**Keybindings** — A [nested map](src/xmas/ed.clj#L179-L194). Simple keys map to command functions. `[:ctrl \x]` maps to a sub-map for prefix sequences (`C-x C-f`, `C-x C-s`, etc.). [`handle-key`](src/xmas/ed.clj#L196-L212) dispatches, with chars falling through to `self-insert`.

### [`xmas.term`](src/xmas/term.clj) — Terminal I/O

Raw terminal control without JLine. Two halves:

- **Input**: [`enter-raw-mode!`](src/xmas/term.clj#L18) via `stty`, then reads bytes from `System.in`. [`read-key`](src/xmas/term.clj#L63-L76) decodes raw bytes into semantic keys — chars, `[:ctrl \f]`, `[:meta \b]`, `:up`, `:backspace`, etc. CSI escape sequences are [parsed](src/xmas/term.clj#L40-L50) for arrow keys and special keys.

- **Output**: Thin wrappers over ANSI escape codes — [`move`](src/xmas/term.clj#L85), [`cls`](src/xmas/term.clj#L87), [`clreol`](src/xmas/term.clj#L86), [`sg`](src/xmas/term.clj#L92-L94) (set graphics/colors). Everything writes to `System.out` directly.

### [`xmas.view`](src/xmas/view.clj) — Screen rendering

Converts editor state into terminal output:

- [`line-start`](src/xmas/view.clj#L12-L14) / [`line-end`](src/xmas/view.clj#L16-L18) — Find newline boundaries in the text string. Used everywhere for line-based navigation and rendering.
- [`cursor-pos`](src/xmas/view.clj#L20-L29) — Maps a text offset to a `[row, col]` screen position relative to scroll.
- [`ensure-visible`](src/xmas/view.clj#L31-L46) — Adjusts scroll so the cursor stays on screen.
- [`render`](src/xmas/view.clj#L60-L101) — Draws the full screen: text lines, a mode line (Emacs-style `-- *scratch* (fundamental)`), and the minibuffer/echo area. Returns the new scroll position.

### [`xmas.log`](src/xmas/log.clj) — Debug logging

Append-only [file logger](src/xmas/log.clj#L9-L15) to `~/.xmas/debug.log`. Every keypress byte and decoded key is [logged](src/xmas/log.clj#L17-L21).

## Data flow

```
read-key -> handle-key(state, key) -> new state -> render(state) -> terminal
     ^                                                                  |
     '----------------------- command-loop ----------------------------'
```

The [main loop](src/xmas/ed.clj#L218-L227) is tight: read a key, dispatch to a command, get new state, render, repeat. The atom is only `reset!`'d at the loop boundary — commands themselves are pure.
