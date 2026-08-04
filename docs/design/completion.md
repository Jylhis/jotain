# Completion and snippets — design specification

**Date:** 2026-07-26
**Status:** Implemented 2026-07-27 in `lisp/init-completion.el` and
`lisp/init-snippets.el`, with coverage in `test/completion-test.el`. Both items
this document recorded as blocking were settled by measurement rather than
deferred — see §2.3 and §2.4. Where the implementation departs from the
specification as
first written, the section says so instead of being quietly edited to match.
**Evidence base:** [`docs/reviews/2026-07-completion-research.md`](../reviews/2026-07-completion-research.md).
Section references below of the form *(research §2.1)* point into that report.
Claims marked **[unverified]** here are ones the research explicitly could not
establish — they are design intent, not evidence.

---

## 1. Requirements

These are the owner's stated requirements. They are inputs, not conclusions.

| # | Requirement |
| --- | --- |
| R1 | Completion pops up **automatically in code**, and **only on request in prose** (text/org/markdown/commit buffers). |
| R2 | **TAB indents. Only.** TAB never triggers completion. |
| R3 | A **separate, explicit key** drives the snippet + completion chain. |
| R4 | **RET never accepts a completion.** Enter inserts a newline, always. |
| R5 | Sources that matter: **eglot (LSP)**, **dabbrev/file**, **tempel snippets**, **history/abbrev/spelling**. AI inline suggestions are **surveyed only** — no adoption decision. |
| R6 | Every feature here has a **documented opt-out**. |

Two prior decisions are settled and not revisited: **tempel** is the snippet
engine, and the AI layer gets **no recommendation** either way.

---

## 2. Design

### 2.1 Trigger model (R1)

**Global default: no auto-popup.** `corfu-auto` stays `nil` — which is also
corfu's own shipped default (research §4.1).

**Code buffers opt in**, per-mode, before the mode activates:

```elisp
(add-hook 'prog-mode-hook (lambda () (setq-local corfu-auto t)))
```

This works with `global-corfu-mode` for a specific, verified reason: `corfu-auto`
is read **exactly once**, in the `corfu-mode` body at enable time, and
`global-corfu-mode` dispatches from `after-change-major-mode-hook`, which
`run-mode-hooks` runs *after* major-mode hooks (research §4.1). The value latches;
`corfu-auto--post-command` never re-reads it.

Two failure modes this design must respect, both verified:

1. **`corfu-mode-hook` is too late.** `define-minor-mode` runs the mode hook after
   the body. Setting `corfu-auto` there does nothing.
2. **Buffers already open when `global-corfu-mode` first enables keep their old
   value** — they do not re-run major-mode hooks. Relevant at startup ordering and
   after a config reload. A buffer opened before corfu initialised will not have
   auto-popup until its mode is re-run.

**Prose buffers get nothing extra.** They inherit the global `nil`, so completion
there is manual-only via the key in §2.3. No `text-mode-hook` entry is needed —
absence *is* the configuration.

**Honest scoping:** upstream's own per-mode knob is `global-corfu-modes`, which
toggles `corfu-mode` itself, not auto-vs-manual. The `setq-local`-in-a-hook
technique is *a* mechanism the corfu README licenses ("setting `corfu-auto` to t
locally … before enabling"), not one upstream presents as *the* prog/prose recipe
(research §4.1).

**Tuning.** `corfu-auto-delay` and `corfu-auto-prefix` keep their current values
(`0.1` / `2`) pending measurement. The research found **no evidence** for any
particular tuning — every number in circulation for this stack is folklore until
measured (research §6.6). Do not present the current values as tuned.

### 2.2 TAB (R2)

**`tab-always-indent` = `t`.**

This is not a customization that fights core — it is the **stock Emacs default** on
both 30 and 31, and `indent-for-tab-command`'s only `completion-at-point` call is
guarded by `(eq tab-always-indent 'complete)`, making that branch provably
unreachable (research §2.1). The current config sets `'complete`
(`lisp/init-completion.el`:276); this design reverts to the default.

The claim that corfu recommends `'complete` — i.e. that this deviates from
upstream — **was refuted 0-3** (research §2.1).

**`tab-first-completion` is out of scope.** It was added in Emacs **28.1** (not 30),
takes exactly five fixed symbols (not a predicate), and is **inert unless
`tab-always-indent` is `complete`** (research §2.3). It should be documented as
irrelevant here, never tuned. This retires the "guard" option considered during
scoping.

**TAB must also be freed inside the popup.** `corfu-map` binds `TAB` →
`corfu-complete` (research §6.5). Setting `tab-always-indent` alone leaves TAB
completing whenever the popup is open, which violates R2 in exactly the moment it
matters most:

```elisp
(keymap-unset corfu-map "TAB" t)
(keymap-unset corfu-map "<tab>" t)
```

**Superseded — there is no exception.** This section originally kept `TAB` for
snippet field navigation as the one deliberate exception. That exception is gone:
fields now move on `M-}`/`M-{` (tempel's own keys) or `C-M-n`/`C-M-p`, and `TAB`
appears nowhere.

The exception turned out to rest on a false premise. **Upstream `tempel-map`
never bound `TAB` at all** — it ships `M-}`/`M-{`, `M-RET` (`tempel-done`),
`M-<up>`/`M-<down>` and a set of command remaps. The `TAB`/`S-TAB` pair was this
configuration's own addition in `init-snippets.el`'s `:bind` block. Removing it
is a deletion, not a substitution, and tempel's own keys take over for free.

`C-M-n`/`C-M-p` are added alongside as a mnemonic alias. No field key may
collide with corfu's `M-n`/`M-p`: `tempel-map` is installed as an overlay
`keymap` property, which is consulted *before* minor-mode maps, so a shared key
would be stolen from the popup mid-snippet. `M-[` would have been actively
hostile on a TTY — `ESC [` is the CSI introducer.

Other TAB consumers that bypass `tab-always-indent` entirely and are **not**
touched by this design (research §2.2): `org-cycle`, `message-tab`, shell-mode's
TAB, `c-tab-always-indent`, and the minibuffer.

### 2.3 The keys (R3, R4)

| Key | Role |
| --- | --- |
| `C-M-i` | **The chain key.** Opens completion; inside the popup, completes. |
| `TAB` | Indent. Only. (Plus tempel field navigation during a live snippet.) |
| `RET` | Newline. Only. Never accepts a candidate. |
| `M-n` / `M-p` | Navigate candidates. |
| `C-g` | Dismiss. |
| `M-+` / `M-*` | tempel by name / interactive insert (unchanged). |

**Why `C-M-i` and not `M-TAB`.** The Emacs manual directs users away from `M-TAB`
because window managers reserve Alt+Tab, and recommends `C-M-i` or `ESC TAB`
(research §2.4). On a TTY all three collapse to the same event, so `C-M-i` is the
only choice that is safe on GUI *and* terminal — and this repo ships a terminal-only
distribution.

Binding a chain command to `C-M-i` **overrides a documented standard binding**
(global `C-M-i` → `complete-symbol` → `completion-at-point`). That is a deliberate,
documented choice, not a defect. Note `C-u C-M-i` currently runs
`info-complete-symbol`; a custom command replaces that behavior.

**RET (R4).** Upstream documents freeing it explicitly (research §6.5):

```elisp
(keymap-unset corfu-map "RET" t)
```

This is a supported configuration, not a workaround. If shell/comint buffers later
need RET to send input, upstream's `menu-item` `:filter` form is the sanctioned
pattern.

**Resolved — and it turned on a mechanism, not a preference.** `corfu-map`
contains a `<remap> <completion-at-point>` entry pointing at `corfu-complete`,
and a remap fires only for the command the key actually *resolves to*. Binding
`C-M-i` to `completion-at-point` — rather than leaving the stock global
`complete-symbol`, which is not remapped — is therefore what makes the same key
both open the popup and accept inside it. No second accept key is needed, and
the choice of `completion-at-point` is load-bearing rather than cosmetic.

Asserted in `completion-test-one-key-opens-and-accepts`.

One thing this does *not* settle: whether `corfu-complete`'s behaviour after
navigating with `M-n` (it extends the common prefix, and inserts when the
selection is unambiguous) feels right in practice. That needs a live Emacs. If
it disappoints, the lever is `corfu-preselect` / `corfu-on-exact-match`, not a
second accept key.

### 2.4 The chain (R3)

The chain is **declarative, via capf ordering** — not a custom dwim command. This
falls out of two verified facts:

- `tempel-expand` returns **only the single exactly-matching template** and
  presents no UI (research §6.1).
- Both tempel capfs return `:exclusive 'no`, so they **fall through** when nothing
  matches (research §1.3, §6.1).

So ordering `tempel-expand` ahead of the semantic capf yields exactly the desired
behavior — *expand a snippet if the text before point names one, otherwise
complete normally* — with `C-M-i` bound to plain `completion-at-point`:

```
prog-mode buffer, buffer-local completion-at-point-functions:
  tempel-expand           (depth -90)   exact template name → expand, else fall through
  eglot-completion-at-point (depth 0)   semantic candidates
  t                                     → global list: cape-dabbrev, cape-file, cape-keyword
```

`tempel-complete` (all templates, shown in the popup) remains available on `M-+`
for browsing by name.

**Prior art:** `make-hippie-expand-function` is the sanctioned way to build a
bespoke chain bound to its own key, and hippie-exp's Commentary explicitly
recommends several such functions "bound to different keys" (research §6.4). This
design reaches the same end through the capf list instead, which keeps a single
`completion-at-point` entry point and avoids a second, parallel expansion
mechanism. `hippie-expand` is **not** adopted.

**Resolved by measurement — and it exposed a live bug.** All four facts below
are asserted in `test/completion-test.el`; none came from documentation.

1. **An exclusive capf shadows everything after it, unconditionally** — even
   when nothing it offers matches the text at point.
2. **`cape-capf-nonexclusive` is the fix.** Wrapping the exclusive capf restores
   the fallthrough, while still keeping control when its own candidates match.
3. **`cape-capf-super` propagates non-exclusivity only when *every* input is
   non-exclusive.** One exclusive input makes the whole merge exclusive.
4. **The `:exclusive no` fallthrough ignores `completion-styles`.** It is decided
   by a bare `try-completion`, so `foo` is discarded against `barfoo` under
   orderless exactly as under `basic` — even though orderless itself matches.
   The `minibuffer.el` FIXME is real, not theoretical.

**Facts 1 and 3 together were a live bug.** `init-snippets.el` merges the
snippet capf with eglot's. Tempel is non-exclusive but eglot is not, so the
merged capf inherited eglot's exclusivity — meaning **`cape-dabbrev`, `cape-file`
and `cape-keyword` never ran in any LSP buffer.** They were configured and dead.
The implementation wraps the merged capf in `cape-capf-nonexclusive`, gated by
`jotain-completion-eglot-nonexclusive`.

**Fact 4 bounds a residual weakness**, which the merge happens to mitigate. A
non-exclusive capf loses candidates a non-prefix pattern would have matched. For
`tempel-complete` alone that would mean snippet names vanishing whenever you type
an orderless-style pattern. Inside the merge the collection also holds the
server's candidates, so the prefix test generally succeeds and the merged capf is
kept. This is a real argument for the merge that the original rationale never
made — but it is mitigation, not a fix, and the underlying approximation is
upstream's.

**Departure from the specification as written.** §2.4 originally proposed
replacing `tempel-complete` with `tempel-expand` (exact-match, no popup entry).
The implementation keeps `tempel-complete` and the merge, on the owner's
decision that snippet names should stay visible in the popup. `tempel-expand`
would have been structurally immune to fact 4 — it never hands the wrapper a
partial prefix — so that immunity was traded for discoverability, knowingly.

**Correction to the current config's stated rationale.** `lisp/init-snippets.el`:45-48
says prepending `tempel-complete` would shadow LSP candidates. It would not —
tempel is non-exclusive and falls through (research §1.3). The `cape-capf-super`
merge is still defensible, but for a *different* reason than the comment gives:
it makes snippet and server candidates appear in **one** popup simultaneously,
rather than the server's list appearing only when no template matches. If that
simultaneity is not wanted, the merge can be dropped in favour of plain ordering —
which also sheds cape's documented whole-lifetime candidate cache, currently used
with **no cache buster** (research §1.5).

### 2.5 Prose completion (R1, R5)

Manual-only, via `C-M-i`. Sources in a prose buffer:

- `tempel-expand` / `tempel-complete` (text-mode hook, already configured).
- `cape-dabbrev`, `cape-file`, `cape-keyword` from the global list.
- **Not** `ispell-completion-at-point`: `init-writing.el`:42 sets
  `text-mode-ispell-word-completion` to `nil`. The research confirms this removes
  exactly two things — the (default-absent) `C-M-i` binding and the buffer-local
  ispell capf — and nothing else (research §5.4). The existing rationale (the capf
  throws "No plain word-list found" where `ispell-alternate-dictionary` is absent,
  as on NixOS) stands, and jinx covers spell-checking.

Worth recording: had that option been left at its default, the ispell capf sits at
**depth 10** and would have been a harmless late fallback, not a shadow
(research §5.1).

**Documented hazard.** If `flyspell-mode` is ever enabled, `flyspell-mode-map`
binds `M-TAB`/`ESC TAB`/`C-M-i` to `flyspell-auto-correct-word`, and as a
minor-mode map it **outranks the global binding** — silently destroying the chain
key in exactly the buffers prose completion is for (research §5.6). This config
uses jinx, not flyspell, so it should not arise; the mitigation if it ever does is
`(setopt flyspell-use-meta-tab nil)`.

### 2.6 AI / LLM inline suggestions (R5)

**No recommendation is made, per the stated constraint.**

Both research rounds produced **zero verified claims** on the AI completion layer —
copilot.el, minuet-ai, codeium/windsurf, aidermacs, gptel, eca-emacs — and on how
ghost-text overlays interact with corfu's child frame or with core
`completion-preview-mode` (research §6.6). Nothing in this document should be read
as evidence for or against adoption.

One structural note that *is* verified and would apply to any future adoption:
Emacs 31's `completion-preview-inhibit-functions` is the sanctioned per-context
suppression hook for core's ghost text (research §3.2), and core's preview binds
TAB **only while a preview is displayed** (research §3.1) — which would conflict
with R2 in that window. Core `completion-preview-mode` is **off by default** and
this design does not enable it.

---

## 3. Opt-out design (R6)

### 3.1 Idiom

**Grounded in this repo's own convention, not external evidence.** The research
gathered nothing on opt-out idioms across two rounds — neither the Elisp manual's
normative guidance nor any real-world config (research §6.6). Rather than invent an
evidence claim, this section follows the pattern already established in `lisp/`:

- a `defgroup jotain-<area>` (`jotain-ui`, `jotain-writing`, `jotain-vc`,
  `jotain-devops`);
- boolean or list `defcustom jotain-<area>-<knob>` toggles with `:type` and
  `:group` (`jotain-prog-warn-non-ts-mode`, `jotain-prog-enable-risky-js-lsp`,
  `jotain-prog-warn-non-ts-exclude`);
- set with `setopt`, per the repo rule.

### 3.2 Proposed knobs

All under a new `(defgroup jotain-completion nil … :group 'convenience)` in
`lisp/init-completion.el`.

| Option | Default | Disables |
| --- | --- | --- |
| `jotain-completion-auto-modes` | `'(prog-mode-hook)` | Auto-popup. `nil` ⇒ manual-only **everywhere**, including code. Add hooks to extend auto-popup to more modes. |
| `jotain-completion-auto-delay` | `0.1` | — (tuning; see §2.1 on the absence of evidence) |
| `jotain-completion-auto-prefix` | `2` | — (tuning) |
| `jotain-completion-free-return` | `t` | `nil` ⇒ restore corfu's default `RET` = `corfu-insert`. |
| `jotain-completion-free-tab` | `t` | `nil` ⇒ restore corfu's default `TAB` = `corfu-complete`. |
| `jotain-completion-eglot-nonexclusive` | `t` | `nil` ⇒ leave the LSP capf exclusive, so the cape fallbacks stay suppressed in LSP buffers (see §2.4). |
| `jotain-completion-key` | `"C-M-i"` | The chain key. `nil` ⇒ bind nothing, leaving the stock global binding intact. |
| `jotain-completion-fallbacks` | `t` | `nil` ⇒ omit `cape-dabbrev` / `cape-file` / `cape-keyword` from the global capf list. |
| `jotain-completion-snippets` | `t` | `nil` ⇒ do not add any tempel capf (`M-+`/`M-*` still work). |

**Granularity rule:** every knob disables exactly one mechanism, and `nil` always
means "behave as stock Emacs/corfu would". No knob silently re-enables another.

**Substitution, recorded:** the ninth knob was originally
`jotain-completion-snippet-tab`, gating the `TAB` field-navigation exception.
That exception no longer exists (§2.2), so the knob would have gated nothing —
and its `nil` branch would have meant "restore a binding upstream never had",
violating the rule above. It is replaced by
`jotain-completion-eglot-nonexclusive`, which gates a mechanism the
implementation genuinely adds and which the original table omitted. The count is
still nine.

### 3.3 Timing semantics — stated, because it is not uniform

This matters and cannot be papered over:

- `jotain-completion-auto-modes`, `-snippets`, `-fallbacks` are read at **load
  time** (they add/remove hooks). Changing them needs a **restart**, or manual
  `remove-hook`.
- `-auto-delay`, `-auto-prefix` are read **per keystroke** — immediate.
- `-free-return`, `-free-tab`, `-snippet-tab`, `-key` modify keymaps at load time;
  a `:set` function could make them immediate, at the cost of the `setopt`-only
  caveat that pattern implies.

**Recommendation:** do **not** add `:set` functions in the first implementation.
Keep the knobs load-time and document the restart, rather than shipping
half-reactive options. (The precedent is core's own
`text-mode-ispell-word-completion`, whose `:set` covers the keymap half while the
capf half is read at mode entry — a split that demonstrably confused this
research, twice.)

### 3.4 Escape hatches that need no knob

Documented for the user, because they cost nothing to support:

- **Per-buffer, right now:** `M-x corfu-mode` toggles the popup in the current
  buffer.
- **Auto-popup in one buffer:** `M-x eval-expression` →
  `(setq-local corfu-auto nil)` then re-run the major mode (the value latches at
  mode-enable time — §2.1).
- **Everything off:** `M-x global-corfu-mode` toggles the whole stack.
- **`var/custom.el` is write-only** in this config, so Customize-set values do not
  persist across sessions by design. Opt-outs belong in the user's own init or in
  a machine-local file, not in Customize.

An environment-variable opt-out (precedent: `JOTAIN_INFO_DIR`) is **deliberately
not proposed**. That precedent exists for a value that must be readable before
Lisp can discover it; completion behavior has no such constraint, and a defcustom
is more discoverable.

---

## 4. Delta from the current configuration

What implementing this would change. Nothing here is done yet.

| File | Change |
| --- | --- |
| `lisp/init-completion.el`:276 | `tab-always-indent` `'complete` → `t`. |
| `lisp/init-completion.el`:281-287 | `corfu-auto` `t` → `nil` globally; add `prog-mode-hook` opt-in. |
| `lisp/init-completion.el` | New: `defgroup jotain-completion` + the §3.2 knobs; `keymap-unset` of `RET`/`TAB` in `corfu-map`; bind the chain key. |
| `lisp/init-snippets.el`:45-48 | Correct the comment's rationale (research §1.3); decide whether to keep the `cape-capf-super` merge and, if kept, whether to add a cache buster. |
| `lisp/init-snippets.el`:51 | Consider `tempel-expand` (exact-match, no UI) for the buffer-local prog-mode capf instead of `tempel-complete`, per §2.4. |
| `docs/` | Document the chain key and the opt-out table; regenerate the package reference if any `@doc` block changes. |

**Both preconditions are discharged.** They were recorded as needing a live
Emacs, and they did — but a *batch* one, not an interactive one, which is
cheaper than this document assumed. The `elisp-test` check already runs
`emacs --batch` against the full package set, so capf dispatch and keymap state
are both directly assertable. The lesson worth keeping: "needs a running Emacs"
is not the same as "needs a human at a GUI".

What still genuinely needs eyes on a screen is narrower than the original list:
how `corfu-complete` *feels* as an accept gesture after `M-n` (§2.3), and
whether the auto-popup delay is comfortable — the latter having no evidence base
at all (§2.1).

---

## 5. Explicitly out of scope

- **yasnippet migration** — tempel is settled.
- **AI adoption** — surveyed only, no recommendation (§2.6).
- **company-mode** — not evaluated; corfu is the incumbent and nothing in the
  research challenged it.
- **Minibuffer completion** (vertico/consult/orderless) — untouched. This document
  covers the in-buffer side only.
- **Performance tuning** — no evidence base exists (research §6.6). Any future
  tuning should start by measuring, not by copying numbers.
