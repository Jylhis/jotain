# Finding Information in Emacs

> [!NOTE]
> AI generated

Also covers programmatic access for scripts and AI agents via `emacs --batch`.


## The self-documenting editor

The starting point is always `C-h` (the Help prefix). If you remember nothing else, remember `C-h
C-h`, it shows you all the help commands.


## 1. The Help system (`C-h`)

`C-h` is the gateway to everything. Press it and wait, `which-key` will show you all available
subcommands. Or press `C-h C-h` for the interactive help menu.

### Quick orientation

`C-h C-h` (or `C-h ?`) runs `help-for-help`, which shows all available help commands. Basically the
meta-help. `C-h C-q` toggles a cheat sheet of common commands via `help-quick-toggle`.

### The essential eight

These are the commands you'll use 90% of the time.

`C-h f` (`describe-function`) gives you documentation for a function, with completion. `C-h v`
(`describe-variable`) shows documentation and the current value of a variable. `C-h k`
(`describe-key`) tells you what command a key sequence runs, with full docs. `C-h o`
(`describe-symbol`) gives you everything about a symbol, the function docs, variable docs, and face
docs all combined. `C-h m` (`describe-mode`) describes the current major mode and all active minor
modes. `C-h b` (`describe-bindings`) lists every active keybinding in the current buffer. `C-h x`
(`describe-command`) is like `C-h f` but restricted to interactive commands. And `C-h w`
(`where-is`) tells you which key(s) run a given command.

### More help commands

Beyond those eight, there is a whole collection of help commands worth knowing about.

`C-h c` (`describe-key-briefly`) shows the command name for a key in the echo area without opening a
popup. `C-h a` (`apropos-command`) finds commands matching a pattern. `C-h d`
(`apropos-documentation`) searches documentation strings. `C-h i` (`info`) opens the Info manual
browser. `C-h r` (`info-emacs-manual`) jumps directly to the Emacs manual. `C-h R`
(`info-display-manual`) lets you choose a manual by name. `C-h S` (`info-lookup-symbol`) looks up a
symbol in the language-appropriate manual. `C-h F` (`Info-goto-emacs-command-node`) finds a command
in the Emacs manual. `C-h K` (`Info-goto-emacs-key-command-node`) finds a key in the Emacs manual.

For general orientation: `C-h t` (`help-with-tutorial`) runs the interactive Emacs tutorial, `C-h p`
(`finder-by-keyword`) browses packages by category, `C-h P` (`describe-package`) gives documentation
for a specific package, `C-h l` (`view-lossage`) shows the last 300 keystrokes so you can replay
what you just did, and `C-h e` (`view-echo-area-messages`) shows the message history.

There is also `C-h .` (`display-local-help`) for showing help-echo text at point, `C-h n`
(`view-emacs-news`) for release notes, and the internationalization helpers: `C-h C`
(`describe-coding-system`), `C-h I` (`describe-input-method`), and `C-h L`
(`describe-language-environment`).

### Built-in documents

Emacs ships with several standalone documents accessible directly from the help system. `C-h t` is
the interactive tutorial available in 20+ languages. `C-h n` (or `C-h C-n`) shows release notes for
the current version. `C-h C-f` opens the FAQ, `C-h C-p` opens known issues and workarounds, `C-h
C-d` explains how to debug Emacs itself, and `C-h C-a` shows version, license, and credits. There is
also `C-h h` which opens the HELLO file, showing character set display samples in many languages.

### Navigating Help buffers

When a `*Help*` buffer is open, there are some useful keys for navigating within it. `TAB` and
`S-TAB` jump between hyperlinks, `RET` follows the hyperlink at point, `s` jumps to the source code
of the described item, `i` looks it up in the Info manual, `I` looks it up specifically in the Elisp
Reference Manual, and `c` opens customization for a variable or face.

For history navigation: `l` goes back, `r` goes forward, `n` and `p` move between help pages, and
`q` quits the help window.

The `s` key deserves special attention. After `C-h f some-function`, pressing `s` takes you directly
to the source code definition. This is the fastest path from "what does it do?" to "how does it
work?"


## 2. The describe family

Emacs has 39 `describe-*` commands. The essential eight are covered above. Here are the rest,
grouped by what they inspect.

### Code

`describe-function` shows the docstring, arglist, type specifier, keybindings, source file, and
obsolescence info. `describe-variable` shows the value, docstring, buffer-local status,
safe-local-variable status, and custom type. `describe-symbol` combines everything: function docs,
variable docs, and face docs for the same symbol. `describe-command` is like `describe-function` but
limited to interactive commands.

### Keys and bindings

`describe-key` gives full docs for the command bound to a key sequence. `describe-key-briefly` just
shows the command name in the echo area. `describe-bindings` lists all active keybindings, covering
minor mode, major mode, and global bindings. `describe-keymap` shows all bindings in a specific
named keymap. `describe-prefix-bindings` shows what follows after a prefix key, so for example `C-x
C-h` shows everything available after `C-x`. This works with any prefix: `C-c C-h`, `M-g C-h`, and
so on. `describe-personal-keybindings` shows your custom `bind-key` and `use-package :bind`
overrides.

### Modes

`describe-mode` shows the current major mode plus all active minor modes with
docs. `describe-minor-mode` gives documentation for a specific minor
mode. `describe-minor-mode-from-indicator` lets you identify a minor mode from its mode-line
lighter, and `describe-minor-mode-from-symbol` does the same by symbol name.

### Text and display

This group covers character-level inspection. `describe-char` tells you everything about the
character at point: unicode, properties, overlays, face, and font. `describe-text-properties` shows
all text properties at point. `describe-face` gives face attributes like foreground, background,
font family, and weight. `describe-font` shows low-level font metrics. `describe-fontset` reveals
which fonts cover which character ranges. `describe-syntax` shows the syntax table, meaning which
characters are treated as words, punctuation, and so on. `describe-categories` shows the character
category table. `describe-char-fold-equivalences` shows case-fold equivalences for a
character. `describe-buffer-case-table` shows buffer case conversion
rules. `describe-current-display-table` shows how characters render.

### International

`describe-coding-system` gives encoding details (UTF-8, latin-1, etc.),
`describe-current-coding-system` shows the active encoding for the current buffer or process,
`describe-input-method` shows how an input method maps keystrokes to characters,
`describe-language-environment` covers language support including charsets, encodings, input
methods, and samples, and `describe-character-set` shows characters in a specific charset.

### Packages, themes, widgets

`describe-package` shows package metadata, dependencies, status, and description. `describe-theme`
gives color theme documentation and face assignments. `describe-widget` shows widget and button
properties at point. `describe-icon` shows icon properties.


## 3. Apropos, finding what you don't know the name of

When you don't know the exact name, apropos searches by pattern. There are 9 apropos commands, each
searching different things.

`apropos-command` (`C-h a`) searches names of interactive commands. `apropos-documentation` (`C-h
d`) searches the documentation strings of all symbols. `apropos-user-option` (available from the
menu) searches names of `defcustom` variables. Then there are the `M-x` variants: `apropos` searches
names of functions, variables, and faces. `apropos-function` searches function names including
non-interactive ones. `apropos-variable` searches all variable names. `apropos-value` searches
values of variables and function definitions. `apropos-library` finds symbols defined in a specific
library file. And `apropos-local-variable` and `apropos-local-value` search buffer-local variables
and their values.

### Pattern syntax

A single word like `"buffer"` matches anything with "buffer" in the name. Multiple words like
`"buffer read only"` match symbols containing at least two of those words. If the pattern contains
`^$*+?.\[`, it's treated as a regular expression. There is also a synonym system, so "find" also
matches "open" and "edit" (configurable via `apropos-synonyms`).

### Examples

`C-h a font` finds all commands with "font" in their name. `C-h d overlay` finds all symbols whose
docstring mentions "overlay". `M-x apropos-library RET python` shows everything defined in
python.el. `M-x apropos-value RET "utf-8"` finds all variables whose value contains "utf-8".

### Reading apropos output

Each result shows the symbol name (which is clickable), type indicators (`c` for command, `f` for
function, `m` for macro, `v` for variable, `u` for user option, `a` for face), key bindings if any,
and the first line of the docstring. Click any symbol name to see its full documentation.


## 4. The Info reader, structured manuals

Info is the primary documentation format for GNU software. Emacs ships with the complete Emacs
Manual and the Emacs Lisp Reference Manual, plus 30+ subsystem manuals.

### Opening Info

`C-h i` opens the top-level directory with all manuals. `C-h r` jumps directly to the GNU Emacs
Manual. `C-h R` lets you choose a specific manual by name. `C-h F` finds a command's section in the
manual. `C-h K` finds a key's section in the manual. `C-h S` looks up a symbol in the
language-appropriate manual.

### Navigation

In the Info reader, `n` and `p` move to the next and previous node at the same level, `u` goes up to
the parent node. `]` and `[` move in reading order, going into children. `m` selects a menu item by
name with completion, `f` follows a cross-reference by name. `l` and `r` handle history (back and
forward). `SPC` and `DEL` scroll, `.` and `b` go to the beginning of the current node, `d` goes to
the top-level Info directory, and `q` quits.

### Searching

There are several ways to search within Info. `i` searches the manual's index for a topic, which is
the best approach for finding specific items. `,` jumps to the next index match. `I` is a virtual
index, searching all index entries with a regexp. `s` does full-text search within the current
manual. And `M-x info-apropos` searches indices across all installed Info manuals.

Index search (`i`) is better than text search (`s`) because the index is curated, it points to the
most relevant section for each topic. Text search finds every mention, which is mostly noise.

### `info-lookup-symbol` (`C-h S`)

This one is underused but quite powerful. It's language-aware: in an Elisp buffer, it searches the
Elisp Reference Manual index. In a C buffer, it searches the C library manual. In a Python buffer,
it searches the Python manual (if installed).

So in an Elisp buffer with cursor on `mapcar`, you do `C-h S mapcar RET` and it opens the Elisp
manual at the mapcar definition.

### Available manuals

Type `C-h R TAB` to see all available manuals. A typical Emacs 30 installation includes Emacs,
Elisp, Org, Gnus, Tramp, CC Mode, Eglot, Flymake, use-package, ERT, Calc, Ediff, Info, ERC, Eshell,
and many more.


## 5. Automatic and contextual help

These features show information without you explicitly asking for it.

### ElDoc

ElDoc displays function signatures and variable types in the echo area as you type. It's enabled
globally by default via `global-eldoc-mode`. So while typing `(mapcar |)`, the echo area shows
`mapcar: (FUNCTION SEQUENCE)`.

Key settings: `eldoc-idle-delay` controls seconds before showing (default 0.5),
`eldoc-echo-area-use-multiline-p` controls multi-line display, and `M-x eldoc-doc-buffer` shows full
docs in a dedicated buffer.

### which-key

After pressing a prefix key like `C-x`, if you pause, a popup shows all available continuations with
descriptions. This is built into Emacs 30.

`which-key-idle-delay` controls the seconds before the popup appears (default 1.0), and
`which-key-popup-type` controls whether it displays as a minibuffer, side-window, or frame. There
are also useful commands: `M-x which-key-show-major-mode` shows all major mode bindings, and `M-x
which-key-show-minor-mode-keymap` shows bindings for a specific minor mode.

### Completion annotations

When using `M-x`, `C-h f`, or `C-h v` with completion, each candidate shows a type indicator (`c`
for command, `f` for function, `m` for macro, `v` for variable, `u` for user option, `a` for face)
and the first line of its docstring. This turns completion into a discovery tool, you type a partial
name and browse what exists.

### Help at point

`C-h .` (`display-local-help`) shows the `help-echo` text property at point. Many UI elements like
buttons, links, and mode-line indicators have this property.


## 6. Source code navigation

When documentation isn't enough, read the source.

### find-function family

`find-function` opens the source definition of a function, and `find-function-other-window` does the
same in another window. `find-variable` and `find-variable-other-window` do the same for
variables. `find-face-definition` finds face source definitions. `find-library` opens a library file
by name. `find-function-on-key` finds the source of the command bound to a
key. `find-function-at-point` and `find-variable-at-point` find the source of whatever symbol or
variable is at point.

As a shortcut from Help: in any `*Help*` buffer, press `s` to jump to source.

### xref (cross-reference framework)

`M-.` (`xref-find-definitions`) jumps to the definition of the symbol at point. `M-?`
(`xref-find-references`) finds all references to the symbol at point. `M-,` (`xref-go-back`) goes
back after jumping, and `C-M-,` (`xref-go-forward`) goes forward. `M-x xref-find-apropos` finds
definitions matching a regexp.

xref works across languages. It uses etags, Eglot/LSP, or grep depending on what's available.


## 7. Shortdoc, function groups by topic

`M-x shortdoc` shows common functions organized by topic with live examples and expected
output. Available groups in Emacs 30 include: `string` (string manipulation), `list` (list
operations), `alist` (association lists), `sequence` (generic sequences with `seq-map`,
`seq-filter`, `seq-reduce`, etc.), `number` (arithmetic), `vector`, `regexp` (regular expressions),
`buffer` (buffer manipulation), `process` (process management), `file-name` (filename operations),
`file` (file I/O), `hash-table`, `keymaps`, `text-properties`, `overlay`, `symbol`, `comparison`,
and `map` (the map library).

Each entry shows the function signature, a brief description, and a live example.


## 8. Customization browser

`M-x customize` is not just for configuration, it's also a documentation browser. Every `defcustom`
variable shows its type, valid values, default, and a full description.

`customize` shows top-level customization groups. `customize-browse` gives you a hierarchical tree
of all option groups. `customize-group` shows all options in a specific group. `customize-variable`
gives a single variable with full docs and type. `customize-face` shows a face with preview and
attribute editor. `customize-apropos` finds options matching a pattern, and there are also
`customize-apropos-faces` and `customize-apropos-groups` for faces and groups specifically.

`customize-browse` is particularly useful for discovery. It lets you explore what Emacs can do by
browsing option hierarchies. Categories like `programming`, `faces`, `editing`, and `convenience`
reveal features you never knew existed.


## 9. Search-based discovery

When you're looking for a needle in a haystack, search.

### In the current buffer

`C-s` (`isearch-forward`) does incremental search, `C-r` (`isearch-backward`) does reverse
incremental search, `C-M-s` (`isearch-forward-regexp`) does regexp search, and `M-x occur` shows all
lines matching a regexp in a clickable buffer.

### Across files

`M-x grep` runs grep with results in a clickable buffer. `M-x rgrep` does recursive grep with
directory and file filtering. `M-x lgrep` greps in a single directory. `C-x p g`
(`project-find-regexp`) searches across the current project, and `C-x p f` (`project-find-file`)
finds files by name in the project.

### Across buffers

`multi-occur` runs `occur` across multiple buffers, and `multi-occur-in-matching-buffers` runs
`occur` in buffers matching a name regexp.


## 10. External documentation inside Emacs

### Unix manual pages

`M-x man` views a Unix man page (requires external `man` command), and `M-x woman` does the same
without external dependencies, the name stands for "WithOut Man". Both give hyperlinked, formatted
output inside Emacs.

### Web browsing

`M-x eww` opens a URL or searches the web inside Emacs. `M-x eww-open-file` opens a local HTML file.

### Package discovery

`C-h p` (`finder-by-keyword`) browses packages by category keyword. `M-x list-packages` shows a
tabulated list of all available packages. `C-h P` (`describe-package`) gives full documentation for
a specific package.


## 11. Runtime introspection

### Evaluate any expression

`M-:` (`eval-expression`) lets you evaluate any Elisp expression and see the result. This is the
most powerful introspection tool, you can query anything.

```elisp
;; What mode is this buffer in?
M-: major-mode RET

;; What's the value of a variable?
M-: tab-width RET

;; What key runs a command?
M-: (where-is-internal 'save-buffer) RET

;; What command is on a key?
M-: (key-binding (kbd "C-x C-s")) RET
```

### Debugging as discovery

`debug-on-entry` lets you step through a function, seeing every call and return
value. `debug-on-variable-change` breaks when a variable changes, so you can find who's mutating
state. `trace-function` logs every call to a function with arguments and return
values. `edebug-defun` instruments a function for step-by-step execution with breakpoints. And
`profiler-start` / `profiler-report` give you CPU and memory profiling to find what's slow.

### Observing recent activity

`C-h l` (`view-lossage`) shows the last 300 keystrokes and commands. `C-h e`
(`view-echo-area-messages`) shows the message log with recent `message` calls.


## 12. Elisp introspection functions

These are the building blocks. Use them in `M-:`, in `*scratch*`, or in batch mode.

### Documentation

```elisp
;; Get a function's docstring
(documentation 'mapcar t)

;; Get a variable's docstring
(documentation-property 'tab-width 'variable-documentation)

;; Get the arglist of a function
(help-function-arglist 'mapcar)
;; → (FUNCTION SEQUENCE)
```

### Finding where things come from

```elisp
;; Which file defines a function?
(symbol-file 'python-mode 'defun)
;; → "python"

;; Which file defines a variable?
(symbol-file 'tab-width 'defvar)

;; Is this a C primitive or Elisp?
(subrp (symbol-function 'forward-char))  ;; → t (C)
(subrp (symbol-function 'forward-word))  ;; → nil (Elisp)

;; Is it native-compiled?
(subr-native-elisp-p (symbol-function 'forward-word))

;; Where is a library file?
(locate-library "org")
;; → "/path/to/org.elc"
```

### Discovering API surfaces

```elisp
;; Find all functions with a prefix
(apropos-internal "^font-lock-" #'functionp)

;; Find all commands with a prefix
(apropos-internal "^describe-" #'commandp)

;; Find all user options in a prefix
(apropos-internal "^org-" #'custom-variable-p)
```

### Key binding introspection

```elisp
;; What command is on a key?
(key-binding (kbd "C-x C-f"))
;; → find-file

;; What keys run a command?
(where-is-internal 'find-file)
;; → ([24 6] [open] ...)

;; Human-readable:
(mapconcat #'key-description (where-is-internal 'find-file) ", ")
;; → "C-x C-f, <open>, ..."
```

### Symbol properties

```elisp
;; The property list reveals metadata the docs don't mention:
(symbol-plist 'font-lock-mode)
;; Properties like: safe-local-variable, permanent-local, risky-local-variable,
;; side-effect-free, byte-compile, compiler-macro

;; Check if a variable is safe as a file-local variable
(get 'tab-width 'safe-local-variable)

;; Get the customize type (shows all valid values)
(require 'cus-edit)
(custom-variable-type 'font-lock-maximum-decoration)
```

### Feature and capability testing

```elisp
;; Does this Emacs have native compilation?
(featurep 'native-compile)

;; Does it have tree-sitter?
(featurep 'treesit)

;; What features are loaded?
features  ;; → (rmc iso-transl tooltip ...)

;; Build configuration
system-configuration-options
emacs-version
system-type
```

### Simulating a file open

```elisp
;; What mode + minor modes activate for a .py file?
(with-temp-buffer
  (setq buffer-file-name "/tmp/test.py")
  (set-auto-mode)
  (cons major-mode
        (seq-filter (lambda (m) (and (boundp m) (symbol-value m)))
                    minor-mode-list)))
```


## 13. Accessing documentation from outside Emacs

For scripts, CI, and AI agents. Two approaches: batch mode (standalone) and emacsclient
(persistent).

### `emacs --batch --eval`

This starts a fresh Emacs, evaluates code, and exits. No GUI, no user config, no persistent state.

```bash
# Get a function's documentation
emacs --batch --eval '(princ (documentation '\''mapcar t))' 2>/dev/null

# Find what command a key runs
emacs --batch --eval '(princ (key-binding (kbd "C-x C-f")))' 2>/dev/null

# Find what keys run a command
emacs --batch --eval '(princ (mapconcat #'\''key-description (where-is-internal '\''find-file) ", "))' 2>/dev/null

# List all functions matching a prefix
emacs --batch --eval '(princ (mapconcat #'\''symbol-name (apropos-internal "^font-lock-" #'\''functionp) "\n"))' 2>/dev/null

# Get a function's arglist
emacs --batch --eval '(princ (help-function-arglist '\''make-overlay))' 2>/dev/null

# Find where a function is defined
emacs --batch --eval '(princ (symbol-file '\''python-mode '\''defun))' 2>/dev/null

# Check if a feature exists
emacs --batch --eval '(princ (featurep '\''native-compile))' 2>/dev/null

# Get build configuration
emacs --batch --eval '(princ system-configuration-options)' 2>/dev/null

# Simulate opening a file and list active modes
emacs --batch --eval '(progn
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test.py")
    (set-auto-mode)
    (let ((modes (list major-mode)))
      (dolist (m minor-mode-list)
        (when (and (boundp m) (symbol-value m))
          (push m modes)))
      (princ (mapconcat #'\''symbol-name modes "\n")))))' 2>/dev/null

# Load a library first, then query
emacs --batch --eval '(progn
  (require '\''auth-source)
  (princ (documentation '\''auth-source-search t)))' 2>/dev/null
```

### Temp file pattern for large output

When output would be too large for stdout, write to a file:

```bash
emacs --batch --eval '(with-temp-file "/tmp/emacs-result.txt"
  (dolist (fn (apropos-internal "^overlay-" #'\''functionp))
    (insert (format "%s: %s\n\n" fn (documentation fn)))))' 2>/dev/null

# Then read it
cat /tmp/emacs-result.txt
```

### `emacsclient --eval`

This connects to a running Emacs server. It's faster since there is no cold start, and it has access
to user config and loaded state.

```bash
# Requires: M-x server-start or emacs --daemon first
emacsclient --eval '(documentation '\''mapcar t)'
emacsclient --eval '(buffer-list)'
```

### Comparison

`emacs --batch --eval` takes about 0.5-1.0s to start (cold), doesn't require a running server, has
no state persistence between calls, doesn't load user config, and is best for scripts, CI, and
standalone queries. `emacsclient --eval` starts in about 0.1s (warm), requires a running server,
shares state with the running Emacs, has user config loaded, and is best for interactive workflows
where you want to leverage existing state.

### Limitations of batch mode

There is no state between calls, so each invocation is a fresh process. If you need to chain
operations, use `(progn ...)`. There is no GUI, so functions that require interactive input will
silently fail. The cold start adds about 0.5-1.0s overhead per call, so for many queries it's better
to use one `emacs --batch` invocation with multiple operations. Output goes to stdout/stderr, use
`princ` for stdout and `2>/dev/null` to suppress startup messages. Libraries are not loaded by
default, so use `(require 'library)` or `--load file.el` for non-default libraries.


## 14. What ships with Emacs

### On-disk reference materials

`C-h t` is the interactive tutorial available in 20+ languages. `C-h n` shows the release notes
(stored in `etc/NEWS` through `etc/NEWS.30`). `C-h C-f` is the FAQ. `C-h C-p` covers known issues
and workarounds. `C-h C-d` explains how to debug Emacs with GDB. `C-h C-a` shows version, license,
and credits. `C-h h` shows character set samples in many scripts. And there are printable
quick-reference cards in `etc/refcards/`.

### The source code as documentation

Emacs Lisp source files serve as documentation through three mechanisms.

Commentary sections at the top of each file explain the module's purpose and design:

```elisp
;;; Commentary:
;; A grab-bag of basic Emacs commands not specifically related to some
;; major mode or to file-handling.
```

Find them with `M-x find-library` then search for `;;; Commentary:`.

Docstrings on every function and variable are accessible via `C-h f` and `C-h v`, or
programmatically via `(documentation 'symbol t)`.

Defcustom declarations show the type, valid values, and group for every user option:

```elisp
(defcustom make-backup-files t
  "Non-nil means make a backup of a file the first time it is saved.
This can be done by renaming the file or by copying. ..."
  :type 'boolean
  :group 'backup)
```

### Key source directories

The `lisp/` directory contains the Elisp standard library (1,587 `.el` files). Under it,
`lisp/progmodes/` has programming language modes like `python.el` and `js.el`, `lisp/emacs-lisp/`
has the Elisp language infrastructure (byte-compiler, cl-lib, seq, map), and `lisp/net/` has
networking code (Tramp, EWW, URL). `lisp/org/` contains Org mode. The `src/` directory has the C
core, covering the Lisp interpreter, display engine, and buffer management. `doc/emacs/` has the
Emacs Manual texinfo sources, `doc/lispref/` has the Elisp Reference texinfo sources, and
`doc/misc/` has 70+ subsystem manual texinfo sources. `etc/NEWS*` has the version-by-version
changelog.


## Quick reference: "How do I find out about...?"

What a function does: `C-h f function-name`. What a variable controls: `C-h v variable-name`. What a
key does: `C-h k` then press the key. What key runs a command: `C-h w command-name`. What this mode
provides: `C-h m`. All keybindings right now: `C-h b`. Everything about a symbol: `C-h o
symbol-name`.

Functions you can't name: `C-h a partial-name` or `C-h d keyword`. Read the manual on a topic: `C-h
i` then `i topic`. Find a command in the manual: `C-h F command-name`. How a function is
implemented: `C-h f function-name` then `s` to jump to source. What functions exist for X: `M-x
shortdoc` or `M-: (apropos-internal "^prefix-" #'functionp)`. What options exist for X: `M-x
customize-group` or `M-x customize-apropos`. What packages do X: `C-h p` or `M-x
list-packages`. What changed in this version: `C-h n`. A Unix command's manual: `M-x man` or `M-x
woman`. What just happened: `C-h l` for keystrokes or `C-h e` for messages. From a script or AI
agent: `emacs --batch --eval '(princ (documentation '\''fn t))' 2>/dev/null`.
