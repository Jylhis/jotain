# Deep-research review — implementation & docs (2026-07)

A close review of the Jotain Elisp layer and its documentation, cross-checked
against **upstream Emacs 30/31 behaviour** and a set of recent Emacs writing.
Findings only reflect upstream Emacs semantics; anything that would only hold on
the `github:jylhis/emacs` Meson fork was deliberately ignored.

## Method

- **Read** every `lisp/init-*.el`, `early-init.el`, `init.el`, and the affected
  `docs/*.mdx` pages.
- **Compared** the requested themes (spell-check, mode line, docs-as-service,
  diary, structured navigation) against the source material listed under
  [Sources](#sources).
- **Verified** each candidate finding adversarially (refute-by-default) before
  it was allowed into this report. One plausible-looking bug (an alleged
  `vundo-unicode-symbols` void-variable at load time) was **refuted** —
  `vundo-unicode-symbols` is an autoloaded `defconst`, so the eager `:custom`
  is correct — and is *not* listed below.

Because no Emacs binary is available in this environment (Nix dev-shell only),
Elisp semantics were checked by reasoning plus the repo's own
`.claude/skills/{elisp-dev,emacs-internals}` reference notes, not by execution.
Line numbers are against the tree at review time.

## Summary of findings

| # | Sev | Area | File | What |
|---|-----|------|------|------|
| 1 | Med | Docs / Info | `lisp/init-docs.el:36` | `add-to-list 'Info-directory-list` before `info-initialize` hides every built-in manual in the fallback path |
| 2 | Med | Project | `lisp/init-project.el:127` | `compile-multi` entry keyed on `python-mode` never fires (buffers are `python-ts-mode`); `%file-name%` is not substituted |
| 3 | Low | Core / internals | `early-init.el:22` | `bidi-display-reordering nil` is an unsupported debug-only knob and redundant |
| 4 | Low | Editing | `lisp/init-editing.el:89` | `(use-package newcomment …)` is defined twice, byte-identical |
| 5 | Low | Spell | `lisp/init-writing.el:52` | `(org-mode . jinx-mode)` is redundant — `org-mode` derives from `text-mode` |
| 6 | Low | Org / state | `lisp/init-org.el:53,83` | `org-clock-persist-file` and `org-roam-db-location` litter outside `var/` |
| 7 | Low | Keys | `lisp/init-keys.el:60` | `jotain-keyboard-quit-dwim` has a dead `cond` branch; docstring says "deactivate" but calls `keyboard-quit` |
| 8 | Low | Devops | `lisp/init-lang-devops.el:28` | `setq` used on the `dockerfile-mode-command` defcustom (repo rule: `setopt`) |
| 9 | Low | Docs drift | `lisp/init-devenv.el:35` | `@doc` still claims `devenv-reload` refreshes "through envrc" after direnv was dropped |
| 10 | Low | Docs drift | `docs/keybindings.mdx` | Missing `C-c j`/`C-c M-j` (majutsu), global `C-c C-e` (eca), `C-x C-g`/`C-x C-j`; `C-c C-d` miscategorised as mode-local |
| 11 | Info | Spell | `lisp/init-writing.el:51` | jinx never runs in `prog-mode`; code comments/docstrings go unchecked |
| 12 | Info | Diary | `lisp/init-org.el` | No `appt`, `diary-file`, or `org-agenda-to-appt` — no pre-event reminders |
| 13 | Info | Core / GC | `lisp/init-core.el:38,48` | 16 MiB GC threshold is a magic number duplicated; minibuffer-exit reset can drift and fires under nested minibuffers |
| 14 | Info | Docs | `docs/finding-information-in-emacs.mdx:6` | "AI generated" banner is warranted; one illustrative `symbol-file` return value is wrong |

No high-severity or security issues were found. The configuration is unusually
clean; most items below are polish, consistency, and doc-drift.

---

## 1. Spell-checking — jinx (requested area)

`lisp/init-writing.el` already uses **jinx** (enchant-backed, JIT font-lock
driven) rather than flyspell, which is exactly the direction of the
[emacsredux "Replacing Flyspell with Jinx"](https://emacsredux.com/blog/2026/07/13/replacing-flyspell-with-jinx/)
post. Two refinements:

**1a — Redundant Org hook (Finding 5, low).** The `:hook` is:

```elisp
:hook ((text-mode . jinx-mode)
       (org-mode  . jinx-mode))
```

`org-mode` is `define-derived-mode org-mode outline-mode`, and `outline-mode`
is derived from `text-mode`. `define-derived-mode` runs the parent modes under
`delay-mode-hooks` and then `run-mode-hooks`, which runs the delayed
`text-mode-hook` (and `outline-mode-hook`) in every Org buffer. So
`(text-mode . jinx-mode)` **already** enables jinx in Org; the `org-mode` entry
just toggles the (idempotent) minor mode a second time. → Drop line 52.

**1b — No prog-mode coverage (Finding 11, enhancement).** The whole reason the
emacsredux post recommends **`global-jinx-mode`** over per-mode hooks is that
jinx decides *what* to check by **face**: in a programming buffer it limits
itself to comments and strings automatically, so one mode replaces both
`flyspell-mode` and `flyspell-prog-mode`. Jotain's per-mode hooks mean typos in
code comments and docstrings are never flagged. This may be intentional; if
comment/docstring checking is wanted, either switch to `global-jinx-mode` or add
`(prog-mode . jinx-mode)`. The current bindings (`M-$` → `jinx-correct`,
`C-M-$` → `jinx-languages`) already match the post exactly, and disabling
`text-mode-ispell-word-completion` (to avoid the NixOS "no word-list" corfu
error) is a correct, well-reasoned touch.

## 2. Mode line & UI (requested area)

`lisp/init-ui.el` uses **doom-modeline** (`doom-modeline-height 28`, LSP
segment on, GitHub/encoding off). Compared to
[davep's minimalist mode-line post](https://blog.davep.org/2026/06/15/more-mode-line-tweaking.html)
(which pares a `mood-line` down to buffer/project/VC), doom-modeline is a
heavier, batteries-included choice — a legitimate philosophical difference, not
a defect. Worth noting only as a design tension: doom-modeline already renders
project/VC/LSP, so if the config ever trends toward minimalism, `mood-line` or a
hand-rolled `mode-line-format` would be the lighter path. **No change
recommended.** The rest of the module (font-preference `cl-loop` with
`find-font` probing, emoji fontset fallback, `load-theme` disable-others advice,
the `calendar-intermonth-text` propertize *form* — correctly an evaluated sexp,
not a pre-computed string) is correct.

## 3. Docs-as-service & Info discoverability (requested area)

Framed against Charles Choi's
["In Emacs, everything looks like a service"](http://yummymelon.com/devnull/in-emacs-everything-looks-like-a-service.html):
the post's real thesis is *model every integration as a client/server with a UI
layer, a client edge, and a local store* — and Jotain's `devenv.el`
transient + `@doc` cookie convention + bundled Info manual is a good expression
of "make the service discoverable." But the discoverability layer has a real
bug and some drift.

**3a — Info fallback hides the standard manuals (Finding 1, medium).**
`lisp/init-docs.el:36`:

```elisp
(add-to-list 'Info-directory-list found)
```

`Info-directory-list` defaults to `nil` and is populated **lazily** by
`info-initialize` (from `INFOPATH` + `Info-default-directory-list`), which only
runs `(unless Info-directory-list …)`. Pre-seeding it here with a single
directory means `info-initialize` later sees a non-nil value and does nothing —
so in the source-checkout fallback path the Info reader can find `jotain.info`
**but no longer finds the Emacs, Elisp, Org, or any other bundled manual.**
That defeats the module's own purpose. Fix: add to
**`Info-additional-directory-list`** instead (designed for user additions,
searched *after* the standard set, never suppresses initialisation), or call
`(info-initialize)` immediately before the `add-to-list`. (The Nix-wrapped build
is unaffected — it uses `INFOPATH`, which `info-initialize` reads correctly —
so this only bites the exact path the module exists to serve.)

**3b — `docs/keybindings.mdx` drift (Finding 10, low).** The reference table has
fallen behind `init-*.el`:

- `C-c j` → `majutsu-log` and `C-c M-j` → `majutsu-dispatch`
  (`init-vc.el:182`, labelled "jujutsu" in `init-keys.el`) are **missing** from
  the `C-c` table.
- Global `C-c C-e` → `eca` (`init-ai.el:51`) is undocumented; the page only
  lists the dired-local `C-c C-e` → `wdired` override, without noting the global
  binding it shadows.
- `C-x C-g` → `jotain-switch-git-status-buffer` and `C-x C-j` →
  `jotain-switch-jj-status-buffer` (`init-vc.el:46,119`) are **missing** from the
  `C-x` table.
- `C-c C-d` → `helpful-at-point` is bound **globally** in helpful's `:bind`
  (`init-help.el:51`), not in `helpful-mode-map`; the page lists it under
  "Mode-local overrides" as a helpful-mode binding. It should move to the global
  list, with a note that `sops-mode` locally overrides `C-c C-d`.

**3c — AI-generated docs banner (Finding 14, info).** The "AI generated" note on
`docs/finding-information-in-emacs.mdx` is appropriate and honest; the page is
broadly accurate and repo-consistent. One illustrative example gives a wrong
`symbol-file` return shape — worth a one-line correction to match the
`locate-library` example later in the same file — but nothing structural.

## 4. Diary / calendar / appointments (requested area)

`lisp/init-ui.el` configures the built-in **calendar** well (ISO week numbers,
Monday `calendar-week-start-day`), and `init-org.el` wires up org-agenda,
capture, roam, and clock. But — motivated by the
[ray-on-emacs "Emacs Carnival: Diary"](https://ray-on-emacs.blogspot.com/2026/06/emacs-carnival-diary-part-2.html)
post — there is a genuine **functional gap** (Finding 12, enhancement):

- No `appt` activation, so **no pre-event reminders** ever fire for timed
  entries; org-agenda is an on-demand view only.
- No `diary-file` and no `org-agenda-include-diary`/`org-agenda-to-appt`
  bridge, so free-form and sexp diary entries (`diary-float`, anniversaries)
  aren't surfaced anywhere.

If reminders are wanted, an `appt` `use-package` block (`:ensure nil`,
`(appt-activate 1)`, `appt-message-warning-time`, and `(org-agenda-to-appt)` on
`org-agenda-finalize-hook` plus a light idle timer) closes it. If the on-demand
agenda is considered sufficient, this should be recorded as an explicit
non-goal — but it is not equivalent to having reminders.

## 5. Structured navigation & editing (requested area)

Measured against emacsredux's
["Essential Structured Navigation and Editing Commands"](https://emacsredux.com/blog/2026/06/20/essential-structured-navigation-and-editing-commands/):
Jotain correctly leans on the **built-in** sexp family (`C-M-f/b`, `C-M-u/d`,
`C-M-a/e`, `C-M-SPC`, `C-M-k`, `C-M-t`) — which are tree-sitter-aware in Emacs
30+ — rather than re-binding them, and layers `expreg` (`C-=`),
`multiple-cursors`, and the prose-level `transpose-sentences`/`-paragraphs` on
top. It also opts into the Emacs 31 `delete-pair-push-mark` and
`kill-region-dwim` niceties (guarded for Emacs 30). Two issues:

**5a — Duplicate `newcomment` block (Finding 4, low).** `init-editing.el`
defines `(use-package newcomment …)` **twice** — lines 54–74 and again 89–109 —
byte-for-byte identical, separated only by the `emacs` transpose block. The
second expansion silently re-runs. Delete lines 89–109.

**5b — Dead `cond` branch in DWIM quit (Finding 7, low).** `init-keys.el:57`:

```elisp
(cond
 ((> (minibuffer-depth) 0)                     (abort-recursive-edit))
 ((get-buffer-window "*Completions*" 'visible) (delete-completion-window))
 ((region-active-p)                            (keyboard-quit))   ; ← dead
 (t                                            (keyboard-quit)))
```

The `region-active-p` branch is identical to the fallback, so it is dead; worse,
the docstring promises "Region active → deactivate it," which implies
`(deactivate-mark)`. Either drop the branch or make it do what the docstring
says. (`keyboard-quit` does deactivate the mark as a side effect, so behaviour is
fine today — but the code doesn't match its contract.) The paired global
`M-s R` bindings — `replace-regexp-as-diff` (global) vs
`dired-do-replace-regexp-as-diff` (`dired-mode-map`) — do **not** conflict; the
buffer-local map shadows the global, and the code comments say so.

## 6. Core, early-init & internals (expansion vs the thesis)

Cross-checked against the attached thesis *"The GNU Emacs Architecture —
Unlocking the Core"* (Karlsson, Uppsala 2026), which documents the
single-threaded command loop, the cooperative **GIL**-serialised Lisp threads,
and stop-the-world mark-and-sweep GC. The config's performance machinery is
**consistent with that model**: the `early-init` GC-defer
(`gc-cons-threshold most-positive-fixnum`), the runtime 16 MiB restore + idle
`garbage-collect`, and the minibuffer GC pause are all correct applications of
"raise the threshold so `consing_until_gc` is large; there are no threads to
race." Nothing in the config wrongly assumes `make-thread` buys parallelism.
Two refinements and one correctness nit:

**6a — `bidi-display-reordering nil` (Finding 3, low).** `early-init.el:22`
sets it via `setq-default`. Upstream documents this variable as intended **only
for debugging**; setting it to `nil` is discouraged and can mis-display real
RTL text. It is also **redundant**: `bidi-paragraph-direction
'left-to-right` + `(setopt bidi-inhibit-bpa t)` are the supported LTR
performance knobs — and are exactly the two the repo's *own*
`.claude/skills/emacs-internals/references/redisplay.md` cites as the mechanism
(it does **not** mention `bidi-display-reordering`). Drop the
`bidi-display-reordering nil` clause; keep the other two. (Impact is cosmetic
and only with RTL text present — hence low, not medium.)

**6b — GC magic number (Finding 13, info).** `16 * 1024 * 1024` appears twice
(`init-core.el:38` and the `minibuffer-exit-hook` reset at line 48). A single
`(defconst jotain-gc-cons-threshold (* 16 1024 1024))` referenced from both
removes the drift risk. Separately, the exit hook lowers the threshold
unconditionally, even when an *outer* recursive minibuffer is still open
(`enable-recursive-minibuffers` is on) — guarding on `(= (minibuffer-depth) 0)`
keeps the high threshold until the outermost minibuffer is dismissed. Both are
minor.

**6c — Thesis caveats.** The briefing flagged two thesis details that are
version-stale and should *not* inform the config: **pure space** (§7.4.6) was
removed in Emacs 30 (pdumper-only), and a `max_specpdl_size` description is
inaccurate. Neither affects Jotain; noted so the internals notes don't inherit
them.

## 7. Broader sweep

- **`compile-multi` python key (Finding 2, medium).** `init-project.el:127`
  keys entries on `python-mode`, but the repo routes tree-sitter modes via
  `major-mode-remap-alist` (commit *"route ts-modes via major-mode-remap-alist"*),
  so Python files open in `python-ts-mode` and this entry **never appears** in
  the picker. Go/Nix/Rust/Zig correctly use their `-ts-mode` keys; Python is the
  odd one out (as are `haskell-mode`/`meson-mode`, worth an audit). Also
  `"pytest %file-name%"` — `compile-multi` does **not** template `%…%`; it would
  run that literal string. Use `python-ts-mode` and a function-valued action
  that builds the argv from `buffer-file-name`.
- **Org state outside `var/` (Finding 6, low).** `org-clock-persist` writes to
  `~/.emacs.d/org-clock-save.el` and `org-roam` to `~/.emacs.d/org-roam.db` —
  both land in the repo root under `just run`, contrary to the config's own
  `var/` discipline (`init-core.el` themes every other state file). Set
  `org-clock-persist-file` and `org-roam-db-location` to `(jotain-var-file …)`.
- **`setq` on a defcustom (Finding 8, low).** `init-lang-devops.el:28` uses
  `(setq dockerfile-mode-command cmd)` inside a `boundp` guard; the repo rule is
  `setopt` for defcustom options (so `:set`/`:type` run). `(setopt
  dockerfile-mode-command cmd)` with a literal symbol and runtime value is valid
  here.
- **`@doc` drift (Finding 9, low).** `init-devenv.el:35` says `devenv-reload`
  refreshes "through envrc (the direnv substrate configured in init-prog)," but
  direnv/envrc was dropped for the native devenv env loader (commit *"Drive
  project env via native devenv"*); the same `@doc` even refers to the "native
  env loader" a few lines later. Reword to the native loader.

## What's already strong

Worth stating plainly, since a findings list skews negative: the module
boundaries and the "package that enhances a built-in shares its file" rule are
followed consistently; `:ensure nil` discipline is correct throughout;
Emacs-31-only features are uniformly `boundp`/`fboundp`-guarded so the config
loads clean on 30; the `custom.el`-supersession advice, the `save-place`
recenter, and the `savehist` "why `register-alist` is excluded" comment show
real care about edge cases; and the `@doc` cookie convention plus bundled Info
manual make the config genuinely self-documenting — the spirit of the
"everything is a service" post.

## Suggested sequencing

1. **Fix now (safe, mechanical):** Findings 1, 4, 5, 6, 8, 9, and the Finding 10
   doc-table updates — all zero-risk.
2. **Fix with a test:** Finding 2 (`compile-multi` python key + action).
3. **Decide (design calls):** Finding 3 (drop the bidi knob), Finding 11
   (`global-jinx-mode` vs status quo), Finding 12 (`appt`/diary — feature or
   documented non-goal), Finding 13 (GC constant + depth guard).

---

## Sources

- **Thesis (attached):** Erik J. Karlsson, *The GNU Emacs Architecture —
  Unlocking the Core*, Bachelor Programme in Computer Science, Uppsala
  University, 2026-03-18.
- Emacs Redux — *Replacing Flyspell with Jinx*, 2026-07-13.
- Emacs Redux — *Essential Structured Navigation and Editing Commands*,
  2026-06-20.
- Charles Choi (yummymelon) — *In Emacs, Everything Looks Like a Service*.
- Dave Pearson — *More mode-line tweaking*, 2026-06-15.
- ray-on-emacs — *Emacs Carnival: Diary, Part 2*, 2026-06.
- Irreal — *Structured Emacs Editing With Builtin Commands*, 2026 (pointer to the
  Emacs Redux structured-editing post).
- Cross-checked against upstream GNU Emacs 30/31 and the repo's own
  `.claude/skills/{elisp-dev,emacs-internals}` reference notes.
