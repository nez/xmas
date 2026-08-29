# xmas — Road to Minimal Emacs

## Goal

Reach Emacs parity by the simplest implementation possible. Every feature must
collapse onto a small set of primitives — buffers, pure `state -> state`
commands, gap text, a single editor atom. Resist parallel implementations of
the same idea: when two features look like the same problem, share the
primitive instead of duplicating it. Cut implementation, never scope.

## Current State (v0.2)

Text editor with Emacs keybindings, gap buffer with line index, elisp reader + evaluator,
buffer/keybinding bridges, `~/.xmas/init.el` loader, `M-:` eval-expression, and a comprehensive test suite.

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
