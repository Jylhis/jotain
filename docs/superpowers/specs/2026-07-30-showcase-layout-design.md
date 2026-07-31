# Showcase layout — design (2026-07-30)

A window layout that puts as much of Jotain's visual identity into one frame as
will legibly fit, plus the recipe that captures it in both themes. Intended
output: two PNGs that can be dropped into the README or the site, and that make
theme regressions obvious at a glance.

## Motivation

`just screenshot` captures whatever the editor happens to look like at startup:
one window, one buffer, no tab bar (`tab-bar-show 1` hides it below two tabs),
no completion UI, no dirvish, no magit. That is a screenshot of Emacs, not of
Jotain.

The 2026-07 UI review (`docs/reviews/2026-07-ui-review.md`) also left three
findings — 1 (macOS titlebar pinned to dark), 3 (`doom-modeline-icon` in mixed
GUI/TTY sessions) and 4 (diff-hl and flymake contesting the left fringe) — that
are visual-verdict calls. They need a scene that actually renders the thing in
question, in both themes, before anyone can confirm or kill them.

## Constraints

- **This design ships untested.** No Emacs 30/31 can be built in the authoring
  session: the egress proxy returns 403 for the `emacs-overlay` flake input,
  the overlay is not substitutable from any binary cache, and every package
  archive is blocked. The first real execution will be the maintainer's.
  Everything below is therefore designed to **degrade rather than abort** — see
  Error handling.
- Linux/graphical only, like `just screenshot`. `jotain-screenshot`
  (`lisp/init-ai.el:46`) signals a `user-error` on tty frames and on builds
  without `x-export-frames`.
- No new packages. The scene may only use what the distribution already ships.

## Placement

`etc/showcase.el`, alongside `etc/debug-init.el`.

That directory is the established home for launch harnesses: files outside
`lisp/` and `test/` so the use-package scanner (`nix/use-package.nix`, which
scans `lisp/` recursively) and the `elisp-compile` / `elisp-lint` flake checks
never see them. `debug-init.el` states this rule in its own header comment; the
showcase follows it.

Loaded via `emacs --load` **after** normal init, so the whole configuration is
already in effect when the scene is built.

## Architecture

Three units with clear boundaries.

### 1. The scene spec — data

A list describing the windows, and a symbol naming the transient step. No
behaviour, no Emacs calls. Something a reader can scan and edit without
understanding the driver:

- which file or buffer each window shows,
- roughly where it goes (the split sequence),
- optionally a line to jump to,
- the transient step to run once the windows exist.

Keeping this as data is what makes a second scene a data edit rather than a
second function.

### 2. The driver — behaviour

Walks the spec: creates the splits, visits the buffers, jumps to lines, runs
the transient step, and leaves the frame ready for capture. Knows nothing about
themes or files on disk; it only interprets the spec.

### 3. The capture loop — orchestration

Disables `auto-dark-mode`, then for each theme: load it, ask the driver to
build the scene, `redisplay`, call `jotain-screenshot` with a per-theme output
path, tear the scene down. Calls the existing capture function rather than
reimplementing `x-export-frames`.

## The scene

One 1920×1080 frame:

```
┌─ tab-bar: jotain │ notes ──────────────────────────────┐
├──────────────────────────┬─────────────────────────────┤
│ breadcrumb header-line   │  dirvish                    │
│ lisp/init-ui.el          │  (nerd-icons, vc-state,     │
│   line numbers,          │   file-size / file-time)    │
│   indent-bars, hl-line,  ├─────────────────────────────┤
│   rainbow-delimiters,    │  magit-status               │
│   hl-todo, diff-hl +     │                             │
│   flymake in the fringe  ├─────────────────────────────┤
│   ┌─ corfu popup ─┐      │  org buffer (org-modern)    │
│   │ nerd-icons    │      │                             │
├───┴───────────────┴──────┴─────────────────────────────┤
│ doom-modeline                                          │
└────────────────────────────────────────────────────────┘
```

**Two tabs.** `tab-bar-show 1` (`lisp/init-tabs.el:30`) hides the bar with only
one tab, so a single-tab scene silently loses a whole UI element.

**An Elisp file in the main window**, not Nix: it exercises
`rainbow-delimiters` (hooked to `emacs-lisp-mode` only, `init-ui.el:369-370`)
and is self-referential — the config displaying its own source.

**Repo files throughout**, not synthetic samples. dirvish points at the repo,
magit at the repo, the main window at a file in `lisp/`.

## Transient UI: corfu

`vertico` and `corfu` are mutually exclusive in a single capture — vertico
needs an active minibuffer, corfu needs the code window selected. Corfu is
chosen because it keeps the code window focused, so `hl-line`, the cursor and
the active `breadcrumb` header-line all render live, and it still demonstrates
the completion stack through `nerd-icons-corfu`.

The transient step is named in the spec, so a `vertico` scene is a data change,
not a rewrite. That is the whole reason the spec/driver split exists.

## Theme loop

`auto-dark-mode` is disabled **before** any theme is forced. It is enabled at
startup (`init-ui.el:84`) and polls system appearance; left running, it can
flip the theme back between scene construction and capture, silently producing
two PNGs of the same theme.

Then, per theme in `jylhis-sheet`, `jylhis-field`: `load-theme` → build scene →
`redisplay` → `jotain-screenshot` → tear down. The `load-theme` advice at
`init-ui.el:38-45` already disables the previous theme, so no manual
`disable-theme` bookkeeping is needed.

Output: two PNGs named after the theme, under a directory the recipe passes in.

## The recipe

`just showcase [dir]`, modelled on `screenshot` (`Justfile:296-316`): same
`xvfb-run` invocation, same `--init-directory`, plus
`--load etc/showcase.el`, and asserts both PNGs are non-empty before reporting
success.

It exports **`JOTAIN_NO_PACKAGE_REFRESH=1`**. This is review finding 10:
`init.el:108` schedules the archive warm-up on a 2-second idle timer, and a
headless capture is idle from the moment init finishes, so on any stale or
missing cache the refresh fires *during* the capture window — network I/O and
echo-area output landing in the frame. `init.el:57-59` provides the env var for
exactly this case.

The same export is added to the existing `screenshot` recipe in the same
change. The race is identical there, and fixing it in one recipe while leaving
the other broken would be arbitrary.

## Error handling

The governing rule, given the untested constraint: **a failure in one scene
step must not prevent the capture.**

- Every scene step is wrapped so a missing package, an unreadable file or a
  failed input simulation degrades that one window and lets the rest of the
  scene build.
- Failures are reported to `*Messages*` so a maintainer running it for the
  first time can see which step degraded rather than guessing from a
  half-populated PNG.
- The capture itself is *not* wrapped: if `jotain-screenshot` fails there is no
  artifact, and the recipe must exit non-zero rather than report a success it
  did not achieve. This mirrors the existing `screenshot` recipe, which exits 1
  when the PNG is missing or empty.
- The Emacs process exits non-zero if any capture failed.

## Testing

Automated testing is not meaningful here — the artifact is a picture, and the
`elisp-test` suite runs in batch mode where `x-export-frames` is unavailable.
The check is manual:

1. `just showcase` produces two non-empty PNGs.
2. Both are visually inspected for: tab bar present with two tabs, doom-modeline
   with glyphs, breadcrumb header-line, line numbers, indent bars, corfu popup
   with nerd-icons, dirvish icons, magit, org-modern styling.
3. The light PNG settles review findings 2 and 4; the pair together settles
   finding 1 on macOS.

`etc/` is excluded from `elisp-lint` and `elisp-compile` by virtue of living
outside `lisp/`, so the new file adds no CI surface. The Justfile recipe is not
exercised by CI either — `screenshot` isn't, for the same reason (it needs a
built Emacs and a display).

## Out of scope

- TTY capture. Review finding 3 wants a terminal frame, but `x-export-frames`
  cannot produce one; that needs a different mechanism (a terminal emulator
  screenshot, or `ansi-term` capture) and is its own piece of work.
- Wiring the PNGs into the README or the site build.

## Addendum — what actually shipped (2026-07-31)

The design above called for **one dense frame**. That was changed to a **set of
scenes** on request, and the scenes were then captured for real. What changed:

- `jotain-showcase-scenes` is an alist of seven named scenes — `code`,
  `completion`, `popup`, `files`, `vc`, `org`, `overview` — each captured under
  both themes, so 14 PNGs rather than 2. The dense frame survives as the
  `overview` scene.
- Transient scenes (`completion`, `popup`) take the capture thunk as an
  argument and capture themselves, since the UI they exist to show only lives
  inside a command that has not returned.
- **`jotain-screenshot` cannot capture a child frame.** `x-export-frames`
  exports one frame's own contents, so corfu's popup — a separate child frame —
  is absent from its output. `jotain-showcase-child-frame-scenes` routes those
  scenes through ImageMagick's `import -window root`, which composites every
  mapped window. This is a real limitation of `jotain-screenshot` worth
  recording independently of the showcase.
- **The `popup` scene still does not show the popup.** corfu displays from
  `post-command-hook`, which never runs when `completion-at-point` is called
  from a timer instead of the command loop. Driving it through
  `execute-kbd-macro` so a real command loop iteration occurs is the likely
  fix; untested. The scene currently captures the buffer without the popup.
- The images were produced on a substitute Emacs, not the distribution — see
  the capture notes committed alongside them. Findings 1, 3 and 4 of the UI
  review are therefore still unsettled by these captures.
- Wiring the PNGs into the README or the site build.
