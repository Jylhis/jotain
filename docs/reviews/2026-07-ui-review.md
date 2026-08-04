# UI review — the user-facing layer (2026-07)

A review of everything that decides what Jotain *looks like* on screen: theme
and appearance switching, the modeline and headerline, fonts and glyph
fallbacks, fringes and margins, line highlighting, the completion UI, Org
styling, and the headless screenshot harness that is supposed to make all of
this verifiable.

Line numbers are against the tree at review time (6118f9f, 2026-07-30).

## Method, and an important limitation

**The screenshot path could not be exercised in this session.** `just
screenshot` needs a built Emacs, and no Emacs 30/31 could be produced here:

- `nix build .#default` fails because the `emacs-overlay` flake input cannot be
  fetched — the session's egress proxy returns HTTP 403 for
  `github.com/nix-community/...`, and the repo's GitHub scope covers only
  `jylhis/jotain`. `add_repo` refuses the cross-owner add.
- The overlay source is not substitutable either: re-running the
  `scripts/bootstrap-agent-env.sh` prefetch with a working `nix hash convert`
  resolves `nixpkgs` and friends from `cache.nixos.org`, but `emacs-overlay`,
  `treefmt-nix`, `flake-compat`, `home-manager`, `jylhis-emacs`, `nmt` and
  `scss-reset` are all cache misses.
- `emacs.nix` composes the overlay unconditionally, so even
  `variant = "mainline"` (which only needs nixpkgs' own `pkgs.emacs`) cannot
  evaluate.
- Every package archive is blocked as well: `melpa.org`, `elpa.gnu.org` and
  `elpa.nongnu.org` all return 403 through the proxy, so a source-checkout run
  cannot bootstrap `elpa/` either.

So the findings below are **static**, not visual. Where a claim could be
settled by running Elisp it was: Ubuntu's `emacs-gtk` 29.3 was installed from
apt and used as an oracle for built-in semantics, and frame capture itself was
verified to work under `xvfb-run` (`x-export-frames` produced a valid PNG). The
real config cannot run on that binary — it aborts in `init-core.el:370` on
`minibuffer-regexp-mode`, which is Emacs 30+. That is the declared version
floor behaving correctly, not a defect.

Findings marked **verified** were confirmed by evaluating Elisp. Findings
marked **static** follow from the repo's own code but were not executed.
Finding 8 additionally depends on third-party package internals that were not
available here and is flagged accordingly.

---

## Findings

### 1. The macOS titlebar is pinned to dark while the theme follows the system — **static**

`early-init.el:88-90`:

```elisp
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist)
  (push '(ns-appearance . dark) default-frame-alist))
```

`ns-appearance` is hardcoded to `dark` for every frame. But the whole theme
layer is built around *following* the system appearance: `init-ui.el:77-84`
runs `auto-dark-mode` with `jotain-theme-light` (`jylhis-sheet`) and
`jotain-theme-dark` (`jylhis-field`).

In macOS light mode the buffer renders light while the titlebar is styled dark.
With `ns-transparent-titlebar` also on, the titlebar takes the frame's
background colour but keeps light-on-light title text, which is the worst of
the two combinations.

The fix is to drive `ns-appearance` from the same signal `auto-dark` uses, or
to drop the entry and let it follow the system default. It cannot be decided in
`early-init.el`, which runs before the theme layer exists — so the honest
version is to set it from `auto-dark`'s appearance-change hook alongside the
theme swap.

### 2. `calendar-iso-week-face` is a frozen snapshot and survives theme switches — **verified**

`init-ui.el:281-282`:

```elisp
(copy-face 'font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil :height 0.7)
```

`copy-face` copies attribute *values*; it does not create a link. Verified:

```
copy-face src="#eeeeee" copy="#111111" inherit=unspecified
```

after mutating the source face. The copy keeps the old colour and its
`:inherit` is `unspecified`.

`calendar` is `:defer t`, so the copy is taken from whichever theme happens to
be active the first time the calendar is opened. Every subsequent `auto-dark`
flip restyles `font-lock-constant-face` but leaves the ISO week gutter on the
previous theme's colour — light-theme week numbers on a dark calendar, or the
reverse.

A `defface` with `:inherit font-lock-constant-face` and `:height 0.7` tracks
the theme correctly and needs no `:config` block at all.

### 3. `doom-modeline-icon` is a global set from a per-frame hook — **static**

`init-ui.el:88-97` and `:112`:

```elisp
(setopt doom-modeline-icon (and (display-graphic-p frame) t))
...
(add-hook 'server-after-make-frame-hook #'jotain-ui--apply-modeline-icons)
```

The hook is per-frame; the variable it sets is global. In a daemon session that
serves a GUI frame *and* a TTY frame — the documented `services.jotain`
deployment, which ships an `emacsclient` wrapper as `EDITOR` — the last frame
created wins for all of them:

- open a TTY client next to a running GUI frame → icons switch off in the GUI
  frame too;
- open a GUI client next to a running TTY frame → the TTY frame starts
  rendering Nerd Font glyphs as tofu.

The same shape applies to `nerd-icons-font-family` (`init-ui.el:300-314`),
though that one is benign because TTY frames ignore font families.

This is very likely the same underlying issue as the open `TODO.md` entry
*"Fix themes in terminal"*. A genuine fix needs the value to be frame-local at
render time rather than a global re-set per frame creation.

### 4. diff-hl and flymake both claim the left fringe — **static**

Two independent modules put indicators in the same place:

- `init-vc.el:235` — `(diff-hl-side 'left)`
- `init-prog.el:457` — `(flymake-fringe-indicator-position 'left-fringe)`

Any line that is both VC-modified and carries a diagnostic can only show one of
the two bitmaps. In a `prog-mode` buffer under active editing — exactly where
both modes are enabled (`global-diff-hl-mode` on `after-init`, `flymake-mode`
on `prog-mode`) — that overlap is the common case, not the corner case: you are
editing a line, so it is modified, and the edit is mid-flight, so it is
diagnosed.

Moving flymake to `'right-fringe` separates the two channels and costs nothing;
the left fringe then reads as "VC state" and the right as "diagnostics".

Worth noting that the *terminal* side of this is already handled well:
`flymake-margin-indicators-string` (`init-prog.el:459-462`) supplies `!`/`?`/`·`
for the margin indicators Flymake falls back to when the display has no
fringes. Only the graphical case collides.

### 5. `hl-line` highlights entire wrapped paragraphs in prose buffers — **verified**

`init-ui.el:250-252` enables `hl-line-mode` in `text-mode`; `init-writing.el:44`
enables `visual-line-mode` in the same hook.

`hl-line` highlights the *logical* line. Verified: `hl-line-range-function`
defaults to `nil`, and in a `visual-line-mode` buffer holding a single 400-char
paragraph, `line-beginning-position`/`line-end-position` span `1..401` — the
whole paragraph.

So in Markdown, Org and Denote buffers the current-line highlight is a solid
block covering every screen row the paragraph wraps onto. That is a very
different visual weight from the single-row highlight the same setting produces
in code, and it fights `mixed-pitch-mode` (`init-writing.el:53-59`), which is
trying to make those buffers read like prose.

Either set a buffer-local `hl-line-range-function` that returns the visual line,
or drop `text-mode` from the `hl-line` hook and keep the highlight for code and
config only. The module's own comment already says the intent is to keep
hl-line away from buffers "where it would fight the cursor" — prose has the
same problem for a different reason.

### 6. The visual half of `whitespace-style` never renders — **verified**

`init-editing.el:51`:

```elisp
(whitespace-style '(face trailing tabs tab-mark))
```

Nothing in the config ever enables `whitespace-mode` or
`global-whitespace-mode`. The only consumer is the `before-save` hook at
`init-editing.el:45-49`, which calls `whitespace-cleanup`.

`face`, `tabs` and `tab-mark` are display styles. Without the mode on, they
render nothing — the block reads as if tabs are visualised, and they are not.

The good news is that the cleanup half is also narrower than it looks, which
was worth confirming because `whitespace-cleanup` is destructive. Verified with
this exact style list:

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

### 7. Ownership of `prefix-help-command` is decided by module load order — **static**

`which-key-mode` (`init-ui.el:268-270`) sets `prefix-help-command`, and embark's
`:init` (`init-completion.el:317-319`) then overwrites it:

```elisp
(setq prefix-help-command #'embark-prefix-help-command)
```

Today that produces the documented behaviour, because `init.el:115` requires
`init-ui` and `init.el:120` requires `init-completion` — embark runs last and
wins. But nothing encodes that dependency. Reordering the requires in `init.el`,
or making `init-completion` load earlier for an unrelated reason, silently hands
`C-h` after a prefix back to which-key and quietly removes the embark paged
view that `init-completion.el:301-311` documents at length.

An `:after which-key` on the embark block, or a one-line comment at the `setq`
naming the ordering constraint, makes the coupling visible.

### 8. `org-startup-indented` may disable org-modern's block styling — **needs confirmation**

`init-org.el:32` sets `(org-startup-indented t)`, which turns on
`org-indent-mode` in every Org buffer; `init-org.el:74-76` enables
`org-modern-mode` in the same buffers.

org-modern documents `org-modern-block-fringe` as incompatible with
`org-indent-mode` — the fringe-drawn block backgrounds that `init-org.el:72-73`
advertises as "faux-rendered blocks" are the part that does not survive.

This one could not be verified here: org-modern's source was not fetchable, so
the incompatibility could not be checked against the pinned version, and the
exact remedy (disabling `org-modern-block-fringe`, or `org-modern-block-name`
alone) depends on which release is in the epkgs snapshot. Flagged for a local
check rather than asserted.

### 9. Frame-setup hooks only cover server frames — **static**

`jotain-ui-apply-font`, `jotain-ui-apply-emoji-font`,
`jotain-ui--apply-nerd-icons-font` and `jotain-ui--apply-modeline-icons` are all
registered on `server-after-make-frame-hook` only (`init-ui.el:112, 179, 206,
314`) plus one direct call at load time.

That covers the two deployments that matter — a plain GUI start (initial frame
exists at load time) and the daemon (server hook fires per client frame). It
does not cover a non-daemon Emacs that gains a graphical frame later:
`emacs -nw` followed by `make-frame-on-display`, or a GUI frame opened on a
second display with a different font situation. Those frames never re-probe.

Low severity given the daemon-first deployment, and adding
`after-make-frame-functions` alongside the server hook would double-fire on
daemon clients. Listed for completeness rather than as a call to action.

### 10. `just screenshot` races its own background package refresh — **static**

`Justfile:307-314` captures on a fixed timer:

```
JOTAIN_SCREENSHOT_OUT="$out" timeout 600 xvfb-run -a -s '-screen 0 1920x1080x24' \
    ./result/bin/emacs --init-directory="{{config_dir}}" \
    --eval '(run-at-time 3 nil (lambda () ... (jotain-screenshot ...) ...)))'
```

`init.el:108` schedules the staleness-gated archive warm-up on a **2-second
idle timer**:

```elisp
(run-with-idle-timer 2 nil #'jotain--refresh-package-archives-maybe)
```

A headless screenshot run is idle from the moment init finishes, so on any
machine whose `archive-contents` cache is older than the 7-day
`jotain-package-refresh-max-age` — or missing, which is every fresh checkout —
the refresh fires at t+2s and the capture happens at t+3s, one second into it.
The captured frame therefore carries whatever the refresh puts in the echo area,
and the run does network I/O that a headless capture has no reason to do.

`init.el:57-59` already provides the opt-out and names this class of use case:
`JOTAIN_NO_PACKAGE_REFRESH`. The recipe simply does not set it. Exporting it
alongside `JOTAIN_SCREENSHOT_OUT` makes the capture deterministic and offline.

This is the one finding that directly blocks the review method itself, so it is
worth fixing before the next visual pass.

---

## Not findings

Things that looked wrong and are not, recorded so they are not re-reported:

- **`init-tabs.el:46-50` reading a tab parameter with `alist-get`.** The
  tab-bar alist has a bare symbol (`current-tab` / `tab`) as its first element,
  which looks like it should break `assq`. It does not — verified:
  `(alist-get 'jotain-tabs-project-dir '(current-tab (name . "x")
  (jotain-tabs-project-dir . "/p")))` returns `"/p"`, and a missing key returns
  `nil` rather than signalling.
- **The theme fallback in `init-ui.el:56-71`.** `load-theme` is called with
  `NO-ENABLE` set, so the `jotain-ui--disable-other-themes` advice correctly
  does nothing during the pre-load, and a failed pre-load degrades to Modus
  before `auto-dark`'s `:custom` reads the theme variables at
  `init-ui.el:82`. The ordering is right.
- **`whitespace-cleanup` on `before-save` mangling tabs.** It does not, with
  the style list this config sets — see finding 6.
- **`mode-line-format . none` in the embark `display-buffer-alist` entry**
  (`init-completion.el:325`). `none` is a documented window-parameter value for
  suppressing the mode line, not a typo for `nil`.
- **`just screenshot`'s `timeout 600` being the wrong knob for a cold cache.**
  It is the right knob. `run-at-time 3` is scheduled when `--eval` is processed,
  which is after init has finished — so package installation is already
  complete and does not eat into the 3-second capture delay. The separate
  refresh race in finding 10 is a different mechanism.

## Open questions

- Findings 1, 3 and 4 are all visual-verdict calls that want a screenshot to
  settle. Fixing finding 10 first, then capturing a light-mode and a dark-mode
  frame plus a TTY frame, would confirm or kill all three cheaply.
- Finding 3 and the `TODO.md` item "Fix themes in terminal" should be
  investigated together; they may have one root cause.
- Finding 8 needs org-modern's pinned source to resolve.
