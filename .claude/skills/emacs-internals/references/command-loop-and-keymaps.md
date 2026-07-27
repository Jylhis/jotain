# The command loop, keymaps, and quitting

Sources: GNU Elisp Reference Manual (Emacs 30.2), Command Loop and Keymaps
chapters; `src/keyboard.c`.

## One iteration of the command loop

1. `read-key-sequence` reads events, echoing after `echo-keystrokes`
   seconds, translating via input-decode-map → local-function-key-map →
   key-translation-map, and looking keys up in the **active keymaps** as it
   goes (it stops as soon as the sequence is a complete binding).
2. The binding is executed via `command-execute`: the interactive spec is
   evaluated to produce arguments (`interactive` string codes or a Lisp
   form), then the command body runs.
3. Around execution: `pre-command-hook` (before), `post-command-hook`
   (after — runs *before* redisplay). Errors in these hooks no longer
   remove the offending function (they used to); they're reported and
   execution continues, but treat both hooks as ultra-hot paths.
4. After the command: point adjustment (moving point out of invisible text
   / composed sequences, unless `disable-point-adjustment`), then redisplay
   if no input is pending.

### Command-loop state variables

- `this-command` (set before the command runs; commands may change it, e.g.
  to group undo), `last-command` / `real-last-command` (previous command),
  `this-original-command` (pre-remapping command).
- `this-command-keys` / `this-command-keys-vector`, `last-command-event`,
  `last-input-event`.
- `current-prefix-arg` / `prefix-arg` (raw prefix for this / next command);
  `prefix-numeric-value` converts. `C-u` = `(4)`, `C-u C-u` = `(16)`,
  `M--` = `-`.
- Special events (`special-event-map`: focus, delete-frame, sigusr…) are
  dispatched inside `read-event` itself, invisibly to the sequence reader —
  the event appears only in `last-input-event`.

### Writing commands

- `(interactive SPEC &rest MODES)` — string codes (`"p"`, `"P"`, `"r"`,
  `"sPrompt"`, `"e"`…) or any Lisp form returning the arg list. The MODES
  tail (Emacs 28+) declares which modes the command applies to and powers
  `M-X` and command completion predicates.
- A command is any `commandp` object: interactive-declared function,
  keyboard macro, or `(declare (interactive-only …))`-free callable.
  Commands intended only for Lisp use should declare `interactive-only`.

## Keymaps

### Format

- A keymap is a list `(keymap . ALIST-OR-CHAR-TABLE...)`; global/dense maps
  use a char-table for character events, sparse keymaps an alist. Bindings
  map an event type to a command, another keymap (prefix key), a string
  (keyboard macro), or an indirection. `keymap-set`/`keymap-lookup`
  (Emacs 29+) are the modern string-syntax API (`"C-c C-f"`), replacing
  `define-key`/`kbd` in new code.

### Active keymap precedence (highest first)

1. `overriding-terminal-local-map` (used by transient maps —
   `set-transient-map`)
2. `overriding-local-map`
3. `keymap` **text/overlay property at point** (at the position clicked for
   mouse events)
4. `emulation-mode-map-alists` (e.g. evil, viper)
5. `minor-mode-overriding-map-alist` (major mode overriding minor modes)
6. `minor-mode-map-alist` (enabled minor modes, in list order)
7. Buffer's **local map** (major mode) — or a `local-map` text/overlay
   property, which *replaces* the buffer's local map (so it sits below
   minor-mode maps, unlike the `keymap` property which sits above)
8. `global-map`

`current-active-maps` and `(key-binding KEY)` resolve exactly this order —
use them when debugging "why does this key do X here".

### Remapping and translation

- Command remapping: `(keymap-set map "<remap> <old-cmd>" 'new-cmd)`;
  resolved at lookup time; `this-original-command` preserves the original.
  `keymap-lookup` follows remaps unless NO-REMAP.
- Translation maps run at different stages: `input-decode-map`
  (terminal escape sequences → events; terminal-local),
  `local-function-key-map` (function-key → ASCII fallback),
  `key-translation-map` (last, above everything; for self-inserting
  swaps). A translation entry can be a function (dynamic translation).
  Pitfall: a complete binding whose sequence is a prefix of a translation
  (`ESC`, `M-[`, `M-O`) prevents the translation from being seen — matters
  for TTY key protocols (kkp/xterm modifyOtherKeys).

### Key-binding conventions (for config code)

- `C-c LETTER` is reserved for users; `C-c C-LETTER` and `C-c DIGIT`/
  `C-c {}<>:;` for major modes; `F5`–`F9` for users. Don't bind `C-h`,
  `C-g`, or `ESC ESC ESC`.

## Reading input programmatically

- `read-event` / `read-char` / `read-key` (decoded), `read-key-sequence`
  (full sequence with all translations). `while-no-input` runs its body
  and aborts (like a quit) the moment input arrives — the standard tool for
  making expensive idle work responsive. `input-pending-p` polls.
- `sit-for SECONDS` — redisplay, then wait for input or timeout;
  `sleep-for` waits unconditionally without redisplay.

## Quitting

- `C-g` sets `quit-flag`; the flag is checked only at safe points
  (`maybe_quit` in C, between byte-codes in Lisp), then signals `quit` to
  the innermost command loop. While *waiting for input*, `C-g` is instead
  an ordinary event bound to `keyboard-quit`.
- `inhibit-quit` non-nil defers quits; when the binding unwinds with
  `quit-flag` still set, the quit fires immediately. `with-local-quit`
  re-enables quitting locally — the right wrapper for code called from
  timers, process filters/sentinels, and pre/post-command hooks (which run
  with `inhibit-quit` bound).
- `minibuffer-quit` signals quit without aborting keyboard-macro
  definition/execution.

## Recursive editing

- `recursive-edit` nests a full command loop; implemented as
  `(catch 'exit …)` — throw nil = return (`exit-recursive-edit`, `C-M-c`),
  throw t = abort with quit (`abort-recursive-edit`, `C-]`); `top-level`
  unwinds all levels. `recursion-depth` reports nesting. Minibuffer input
  is a recursive edit. Prefer a special mode or dedicated buffer over
  `recursive-edit` in application code; legitimate users: the debugger,
  `query-replace`'s `C-r`.
