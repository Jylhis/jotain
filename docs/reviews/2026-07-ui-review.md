# UI review — the user-facing layer (2026-07)

A review of everything that decides what Jotain looks like on screen: theme and
appearance switching, the modeline and headerline, fonts and glyph fallbacks,
fringes and margins, line highlighting, the completion UI, keybinding surface,
and Org styling.

Line numbers are against `b26628e`. Findings marked **verified** were confirmed
by evaluating Elisp against a real Emacs; **static** ones follow from the repo's
own code but were not executed.

## Method, and its limitation

The visual half of this review could not be exercised. `just screenshot` and
`just showcase` need a built Emacs, and none could be produced in the review
environment: the egress proxy returns 403 for the `emacs-overlay` flake input,
that overlay is in no binary cache, and every package archive (MELPA, GNU ELPA,
NonGNU ELPA) is blocked as well.

A substitute Emacs was assembled to get *some* runtime signal — nixpkgs' own
`pkgs.emacs` 30.2 built straight from the pinned nixpkgs (no overlay needed),
the real Jylhis themes `trivialBuild`-ed from the `nix/design-pin.nix`
revision, and the real `lisp/init-*.el` modules — but `compat` is in no binary
cache, which took out the whole nixpkgs `emacsPackages` route. `doom-modeline`,
`nerd-icons`, `dirvish`, `magit`, `org-modern`, `indent-bars`, `breadcrumb` and
`pulsar` were all absent, so the resulting frames could not settle any
appearance question and are not included here.

Consequence: **findings 1, 3 and 4 are visual-verdict calls that remain
unsettled.** They need `doom-modeline` and the full fringe stack. Run
`just showcase` on a real distribution build to confirm or kill them.

---

## Findings

### 1. The macOS titlebar is pinned to dark while the theme follows the system — **static**

`early-init.el:82-83`:

```elisp
(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(ns-appearance . dark) default-frame-alist)
```

`ns-appearance` is hardcoded to `dark` for every frame, but the theme layer is
built around *following* the system: `init-ui.el:84` runs `auto-dark-mode` with
`jotain-theme-light` (`jylhis-sheet`) and `jotain-theme-dark` (`jylhis-field`).

In macOS light mode the buffer renders light while the titlebar is styled dark.
With `ns-transparent-titlebar` also on, the titlebar takes the frame's
background colour but keeps light-on-light title text — the worst of the two
combinations.

`early-init.el` runs before the theme layer exists, so this cannot be decided
there. The honest fix is to set `ns-appearance` from `auto-dark`'s
appearance-change hook, alongside the theme swap.

### 2. `calendar-iso-week-face` is a frozen snapshot and survives theme switches — **verified**

`init-ui.el:281-282`:

```elisp
(copy-face 'font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil :height 0.7)
```

`copy-face` copies attribute *values*; it does not create a link. Verified by
mutating the source face afterwards:

```
copy-face src="#eeeeee" copy="#111111" inherit=unspecified
```

The copy keeps the old colour and its `:inherit` is `unspecified`.

`calendar` is deferred, so the copy is taken from whichever theme is active the
first time the calendar opens. Every later `auto-dark` flip restyles
`font-lock-constant-face` but leaves the ISO week gutter on the previous
theme's colour.

A `defface` with `:inherit font-lock-constant-face` and `:height 0.7` tracks the
theme correctly and needs no `:config` block at all.

### 3. `doom-modeline-icon` is a global set from a per-frame hook — **static**

`init-ui.el:95-96` and `:112`:

```elisp
(setopt doom-modeline-icon (and (display-graphic-p frame) t))
...
(add-hook 'server-after-make-frame-hook #'jotain-ui--apply-modeline-icons)
```

The hook is per-frame; the variable it sets is global. In a daemon session
serving a GUI frame *and* a TTY frame — the documented `services.jotain`
deployment, which ships an `emacsclient` wrapper as `EDITOR` — the last frame
created wins for all of them:

- open a TTY client next to a running GUI frame → icons switch off in the GUI
  frame too;
- open a GUI client next to a running TTY frame → the TTY frame renders Nerd
  Font glyphs as tofu.

The same shape applies to `nerd-icons-font-family` (`init-ui.el:302-314`),
though that one is benign because TTY frames ignore font families.

This is very likely the same root cause as the open `TODO.md` entry *"Fix themes
in terminal"*. A real fix needs the value to be frame-local at render time
rather than a global re-set per frame creation.

### 4. diff-hl and flymake both claim the left fringe — **static**

- `init-vc.el:235` — `(diff-hl-side 'left)`
- `init-prog.el:457` — `(flymake-fringe-indicator-position 'left-fringe)`

Any line that is both VC-modified and carries a diagnostic can show only one of
the two bitmaps. In a `prog-mode` buffer under active editing — where both are
enabled — that overlap is the common case, not the corner case: you are editing
a line, so it is modified, and the edit is mid-flight, so it is diagnosed.

Moving flymake to `'right-fringe` separates the two channels at no cost; the
left fringe then reads as "VC state" and the right as "diagnostics".

The terminal side is already handled well: `flymake-margin-indicators-string`
(`init-prog.el:462`) supplies `!`/`?`/`·` for the margin indicators Flymake
falls back to when the display has no fringes. Only the graphical case collides.

### 5. `hl-line` highlights entire wrapped paragraphs in prose buffers — **verified**

`init-ui.el:252` enables `hl-line-mode` in `text-mode`; `init-writing.el:44`
enables `visual-line-mode` in the same hook.

`hl-line` highlights the *logical* line. Verified: `hl-line-range-function`
defaults to `nil`, and in a `visual-line-mode` buffer holding a single 400-char
paragraph, `line-beginning-position`/`line-end-position` span `1..401` — the
whole paragraph.

So in Markdown, Org and Denote buffers the current-line highlight is a solid
block covering every screen row the paragraph wraps onto. That is a very
different visual weight from the single-row highlight the same setting produces
in code, and it fights `mixed-pitch-mode`, which is trying to make those buffers
read like prose.

Either set a buffer-local `hl-line-range-function` returning the visual line, or
drop `text-mode` from the `hl-line` hook and keep the highlight for code and
config only.

### 6. The visual half of `whitespace-style` never renders — **verified**

`init-editing.el:51`:

```elisp
(whitespace-style '(face trailing tabs tab-mark))
```

Nothing in the config enables `whitespace-mode` or `global-whitespace-mode`. The
only consumer is the `before-save` hook at `init-editing.el:49`, which calls
`whitespace-cleanup`.

`face`, `tabs` and `tab-mark` are display styles. Without the mode on they
render nothing — the block reads as if tabs are visualised, and they are not.

The cleanup half is also narrower than it looks, which was worth confirming
because `whitespace-cleanup` is destructive. Verified with this exact style list:

```
indent-tabs-mode=nil -> "a\tb\nc\td\n"
indent-tabs-mode=t   -> "        x\ny\n"
```

Interior tabs are preserved and leading spaces are not tabified in either
direction: `tabs` governs display, while cleanup's tab rewriting is driven by
`indentation`/`space-before-tab`/`space-after-tab`, none of which are set. So
the hook is effectively `delete-trailing-whitespace` and is safe for Makefiles
and Go.

Either enable `whitespace-mode` somewhere if the visualisation is wanted, or
reduce the list to `'(trailing)` so it states what it actually does.

### 7. Markdown's new `C-c m` prefix shadows the global `consult-man` — **static**

`init-completion.el:226` binds `C-c m` globally to `consult-man`, and
`init-keys.el:108` documents it in the which-key description table as
`"consult-man"`.

`init-writing.el:108-116` now binds a `C-c m ...` prefix inside
`markdown-mode-map` — `C-c m h`, `C-c m i`, `C-c m l`, and so on. A prefix and a
command cannot share a key, so in every Markdown buffer `C-c m` stops being
`consult-man` and becomes a prefix.

Two consequences: the global binding silently disappears in the buffers where
it is arguably most reachable, and `init-keys.el`'s which-key label for `C-c m`
is now wrong there. `C-c m` is also the only `C-c <letter>` slot in the config
that is both a global command and a mode-local prefix — every other
`C-c <letter>` is bound exactly once.

Either move the markdown keymap to a free prefix, or drop the global
`consult-man` binding and describe `C-c m` as the markdown prefix. Leaving both
is the one state that is actively misleading.

### 8. Ownership of `prefix-help-command` is decided by module load order — **static**

`which-key-mode` (`init-ui.el:270`) sets `prefix-help-command`, and embark's
`:init` (`init-completion.el:318`) then overwrites it:

```elisp
(setq prefix-help-command #'embark-prefix-help-command)
```

Today that produces the documented behaviour, because `init.el:54` requires
`init-ui` and `init.el:59` requires `init-completion` — embark runs last and
wins. Nothing encodes that dependency. Reordering the requires, or making
`init-completion` load earlier for an unrelated reason, silently hands `C-h`
after a prefix back to which-key and removes the embark paged view that
`init-completion.el` documents at length.

An `:after which-key` on the embark block, or a one-line comment at the `setq`
naming the ordering constraint, makes the coupling visible.

### 9. `org-startup-indented` may disable org-modern's block styling — **needs confirmation**

`init-org.el:42` sets `(org-startup-indented t)`, which turns on
`org-indent-mode` in every Org buffer; `init-org.el:272` enables
`org-modern-mode` in the same buffers.

org-modern documents `org-modern-block-fringe` as incompatible with
`org-indent-mode` — the fringe-drawn block backgrounds are the part that does
not survive.

This could not be verified: org-modern's source was not fetchable, so the
incompatibility could not be checked against the pinned version, and the exact
remedy (disabling `org-modern-block-fringe`, or `org-modern-block-name` alone)
depends on which release is in the epkgs snapshot. Flagged for a local check
rather than asserted.

### 10. Frame-setup hooks cover only server frames — **static**

`jotain-ui-apply-font`, `jotain-ui-apply-emoji-font`,
`jotain-ui--apply-nerd-icons-font` and `jotain-ui--apply-modeline-icons` are all
registered on `server-after-make-frame-hook` only (`init-ui.el:112, 179, 206,
314`), plus one direct call at load time.

That covers the two deployments that matter — a plain GUI start (initial frame
exists at load time) and the daemon (server hook fires per client frame). It
does not cover a non-daemon Emacs that gains a graphical frame later:
`emacs -nw` followed by `make-frame-on-display`, or a GUI frame opened on a
second display with different font availability. Those frames never re-probe.

Low severity given the daemon-first deployment, and adding
`after-make-frame-functions` alongside the server hook would double-fire on
daemon clients. Listed for completeness rather than as a call to action.

---

## Not findings

Things that looked wrong and are not, recorded so they are not re-reported:

- **`init-tabs.el` reading a tab parameter with `alist-get`.** The tab-bar alist
  has a bare symbol (`current-tab` / `tab`) as its first element, which looks
  like it should break `assq`. It does not — verified:
  `(alist-get 'jotain-tabs-project-dir '(current-tab (name . "x")
  (jotain-tabs-project-dir . "/p")))` returns `"/p"`, and a missing key returns
  `nil` rather than signalling.
- **The theme fallback in `init-ui.el:62-70`.** `load-theme` is called with
  `NO-ENABLE` set, so the `jotain-ui--disable-other-themes` advice
  (`init-ui.el:38-45`) correctly does nothing during the pre-load, and a failed
  pre-load degrades to Modus before `auto-dark` reads the theme variables at
  `init-ui.el:84`. The ordering is right.
- **`whitespace-cleanup` on `before-save` mangling tabs.** It does not, with the
  style list this config sets — see finding 6.
- **`mode-line-format . none` in the embark `display-buffer-alist` entry**
  (`init-completion.el:324`). `none` is a documented window-parameter value for
  suppressing the mode line, not a typo for `nil`.

## Resolved before this review landed

An earlier draft carried an eleventh finding: `just screenshot` captured at
t+3s while `init.el` scheduled a package-archive warm-up on a 2-second idle
timer, so the refresh fired inside the capture window. Commit `3d3e204`
("init: never fetch package archives at startup") removed that startup fetch
entirely, so the race no longer exists and the finding is dropped rather than
carried as fixed.

## Open questions

- Findings 1, 3 and 4 want a screenshot to settle. `just showcase` captures the
  scenes in both themes; run it on a real distribution build.
- Finding 3 and the `TODO.md` item "Fix themes in terminal" should be
  investigated together; they may have one root cause.
- Finding 9 needs org-modern's pinned source to resolve.
