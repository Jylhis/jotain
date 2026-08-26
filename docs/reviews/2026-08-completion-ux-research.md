# In-buffer completion UX — "less annoying, more natural" research

**Date:** 2026-08-26
**Scope:** A second round on the in-buffer side of `lisp/init-completion.el`,
narrowly aimed at the request "make the autocomplete popup and usage less
annoying and more natural." Builds on
[`2026-07-completion-research.md`](./2026-07-completion-research.md) and the
design in [`../design/completion.md`](../design/completion.md); the capf/keymap
facts settled there are not re-litigated here.
**Status:** Research + implementation. The changes it motivates shipped in the
same round in `lisp/init-completion.el` and `test/completion-test.el`.

## How to read this

Each concrete claim is tagged:

- **[V]** — verified against a primary source (package source read on its
  release branch, GNU manual, or NEWS).
- **[CFG]** — corroborated by a widely-cited real configuration, not by a
  normative source.
- **[FOLK]** — community consensus / folklore; sensible but not documented.
- **[I]** — a sound inference from a verified source.

Two hard constraints from the existing design bound every option below and are
treated as non-negotiable: **TAB indents only** (R2) and **RET is a newline,
never an accept** (R4).

---

## 1. The three levers that were actually available

The 2026-07 round left the trigger model (auto in code, manual in prose),
the freed RET/TAB, and the capf chain settled. What it did **not** tune, and
what this round found, are three orthogonal levers:

1. **The popup commits candidates you did not choose** — `corfu-preview-current`.
2. **The popup fires half a beat too eagerly** — `corfu-auto-delay` / `-prefix`.
3. **Two "more natural" surfaces were simply absent** — a documentation panel
   (`corfu-popupinfo`) and inline ghost text (`completion-preview-mode`).

Nothing here needs a new package: `corfu-popupinfo` is a bundled corfu
extension and `completion-preview` is built into Emacs 30+, so all three are
`:ensure nil` and add nothing to the package closure.

---

## 2. `corfu-preview-current` — the accidental-accept lever [V]

**Finding, and a corrected premise.** corfu's `corfu-preview-current` defaults
to the symbol **`insert`**, not `t`. Its docstring: *"If the variable has the
value `insert', the candidate is automatically inserted on further input."* So
with the shipped default, while a popup is open, **continued typing commits the
selected candidate** into the buffer. [V — corfu.el defcustom]

This directly undercuts the spirit of R4 and the freed RET/TAB. The 2026-07
design took great care that RET, TAB, and continued typing never *accept* a
candidate — but `corfu-preview-current insert` was quietly doing exactly that on
continued input. Setting it to **`nil`** shows the menu with no inline text and
inserts nothing until an explicit `corfu-complete` (`C-M-i`). [V — docstring;
CFG — this is what Prot's config sets]

This is the single biggest "less annoying / more natural" win, and it is also
what makes room for lever 3: with `nil`, corfu contributes **no** inline text,
so `completion-preview-mode`'s ghost is the one and only inline surface — no
duelling previews.

**Shipped:** `(corfu-preview-current nil)` in the corfu `:custom` block.

## 3. Auto-popup timing — the eagerness lever [V]

**Corrected premises.** corfu's shipped defaults are `corfu-auto-delay` = **0.2**
and `corfu-auto-prefix` = **3**. [V — corfu.el / GNU ELPA doc] This config was
running **0.1 / 2** — *below both defaults*, in the direction corfu's own
docstrings caution against: a very short delay or small prefix "will create high
load for Emacs, in particular if executing the completion backend is costly,"
and the README example annotates `corfu-auto-delay 0` / `corfu-auto-prefix 0`
with "TOO SMALL — NOT RECOMMENDED!". [V — corfu README + docstrings]

The 2026-07 design was explicit that 0.1 / 2 were **not measured** — "every
number in circulation for this stack is folklore until measured." Given no
measurement exists either way, moving to corfu's *documented defaults* (0.2 / 3)
is the better-justified resting point than sitting below them: it is the one
change most directly aimed at "it pops up while I'm still typing," and it aligns
the popup with `completion-preview-minimum-symbol-length` (3), so the ghost text
and the popup begin suggesting at the same keystroke. [V for the defaults; I for
"better-justified than 0.1/2"]

Real configs cluster at delay 0.1–0.25 and prefix 2–3; anything at or above
0.2 / 3 is squarely inside the documented-safe range, below it is the debated
zone. Prot side-steps the question entirely (no auto-popup). [CFG]

**Shipped:** defaults raised to `jotain-completion-auto-delay 0.2`,
`jotain-completion-auto-prefix 3`, with the docstrings rewritten to cite the
corfu recommendation instead of "not tuned."

## 4. `corfu-popupinfo` — the documentation panel [V]

**What it is.** A bundled corfu extension (successor to `corfu-doc`) that shows
a child-frame panel beside the candidate list with the selected candidate's
docstring or source location — the IDE "detail pane." Enabled with
`(corfu-popupinfo-mode 1)`; inactive by default like all corfu extensions.
[V — extensions/corfu-popupinfo.el + corfu README]

**Delay is a cons `(INITIAL . SUBSEQUENT)`,** defaulting to `(2.0 . 0.5)`: a
long wait before it first appears (so docs do not flash on every pause) and a
short refresh as you arrow between candidates. The split exists precisely to
make first appearance unobtrusive while keeping candidate-to-candidate refresh
instant. A responsive-but-quiet value is `(1.0 . 0.5)` (Prot uses `(1.25 . 0.5)`).
[V — defcustom; CFG — Prot]

**Keymap safety.** `corfu-popupinfo-map` binds only `M-t` (toggle),
`M-h`/`M-g` (documentation/location), and the `C-M-v` scroll family. It touches
**neither** the freed RET/TAB **nor** `M-n`/`M-p`, so it is safe under this
config's constraints — verified against the full `corfu-map` list. [V]

`corfu-echo` (one-line echo-area doc, `corfu-echo-delay '(2.0 . 1.0)`) is the
lighter alternative; running both is redundant. This config chose the panel for
the fuller IDE feel; the echo mode is the documented fallback if the child frame
ever feels heavy. [V — extensions/corfu-echo.el; FOLK — "pick one"]

**Shipped:** `corfu-popupinfo` block, delay `(1.0 . 0.5)`, gated by
`jotain-completion-doc-popup` (default t).

## 5. `completion-preview-mode` — inline ghost text (Finding 12b) [V]

This is the deferred feature decision from
[`2026-07-emacs-nix-deep-review.md` Finding 12](./2026-07-emacs-nix-deep-review.md),
option (b): adopt the built-in inline preview alongside corfu.

**What it is.** A buffer-local minor mode (Emacs **30.1**; user options extended
in **31**) that draws the top completion candidate as greyed "ghost text" after
point, updated as you type, sourced from the **same** `completion-at-point-functions`
corfu uses — so it previews the eglot/cape/tempel candidates already configured,
with no separate source list. `global-completion-preview-mode` is the global
twin. [V — NEWS.30 "New Modes and Packages"; completion-preview.el commentary]

### 5.1 The R2 landmine [V — decisive]

`completion-preview-active-mode-map` (active only while a preview shows) binds,
verbatim on the `emacs-31` branch:

```elisp
"C-i" #'completion-preview-insert
"M-i" #'completion-preview-complete
```

`C-i`, `TAB`, and `?\t` are the **same event** in Emacs — `"C-i"` and `"TAB"`
are identical keymap keys. So the shipped binding **is** "TAB accepts the
preview": on a terminal the Tab key sends `C-i` directly; in a GUI the `<tab>`
event is translated to `C-i` by `local-function-key-map` when `<tab>` is unbound
in the active maps, and the minor-mode map then shadows `indent-for-tab-command`.
The mode's commentary confirms the intended UX is "accept with TAB." Left as-is
this is a **direct R2 violation** in exactly the buffers it runs in. [V — keymap
source + Emacs key-translation semantics]

The remedy is a one-liner: `(keymap-unset completion-preview-active-mode-map
"C-i" t)` (REMOVE = t), after which Tab falls through to indentation while a
preview shows. The whole-candidate accept then needs a non-TAB home; this config
binds `completion-preview-insert` to **`M-RET`** (R4 governs bare RET only; `M-RET`
is a distinct event) and leaves `M-i` (common-prefix complete) as shipped. [V —
`keymap-unset` semantics; I — M-RET is R4-safe]

**Do not** copy the common community recipe that binds TAB to
`completion-preview-complete` (e.g. the "À la Mode" corfu+preview config): it
re-introduces the exact R2 violation. [FOLK, explicitly incompatible with R2]

**R4 is safe out of the box** — the map binds no `RET`/`<return>`, not even
commented out. The only obligation is: do not add one. [V]

### 5.2 The options that matter [V — all defaults from the emacs-31 source]

| Option | Default | Use here |
| --- | --- | --- |
| `completion-preview-idle-delay` | `nil` (immediate) | Set to `jotain-completion-auto-delay` (0.2) so the ghost and the popup share one debounce, and a costly LSP capf is not hit twice per keystroke at different times. |
| `completion-preview-minimum-symbol-length` | `3` | Left as-is; matches the 3-char popup prefix. (`nil` would fire after punctuation/whitespace too — *more* aggressive, not less.) |
| `completion-preview-sort-function` | `#'minibuffer--sort-by-length-alpha` (user option in 31) | Paired with `corfu-sort-function` so the ghost matches the popup's top row — NEWS.31 promotes it as a user option *specifically* "together with … Corfu … for a more integrated experience." Guarded on the option's `custom-type` so Emacs 30 is untouched. |
| `completion-preview-inhibit-functions` | `nil` (new in 31) | Add a comment/string predicate (`(nth 8 (syntax-ppss))`) so the ghost does not appear where symbol completion is meaningless. Guarded on `boundp` (absent on 30). |
| `completion-preview-completion-styles` | `'(basic)` — a **`defvar`**, not a defcustom | **Left as shipped.** The preview computes candidates with `basic` (prefix) matching, *not* the config's orderless. Consequence: the ghost is prefix-only, which is the conservative, predictable choice for an always-on inline hint; orderless stays where it belongs, in the popup and the minibuffer. [V — it is a defvar defaulting to `(basic)`; I — leaving it prefix-only is the calmer choice] |

### 5.3 Coexistence with corfu, and where to run it [V + FOLK]

corfu has **no** inline ghost text of its own; it previews only inside its popup
(`corfu-preview-current`). The corfu README does not mention completion-preview —
they are independent, rendering in different places (popup vs. inline overlay),
and do not fight over one UI element. They *do* both read the same capf, so with
both surfaces live you see the popup list **and** one ghost line. [V]

The folklore worry about a "busy double surface" comes almost entirely from
running completion-preview together with `corfu-preview-current insert` — i.e.
*two* inline previews. Setting `corfu-preview-current nil` (§2) collapses that to
exactly one inline surface (the ghost) plus the popup list — which is the
familiar modern-editor arrangement, not noise. [I from §2 + the coexistence
facts]

**Placement decision.** Rather than `global-completion-preview-mode` (which would
put ghost text in prose and the minibuffer — the minibuffer is explicitly *not*
recommended, and prose is meant to stay quiet per R1), the mode is enabled only
in `jotain-completion-auto-modes` — the *same* mode list that gets the
auto-popup. Code gets both surfaces; prose inherits neither and stays manual-only
via `C-M-i`; the minibuffer is never touched. [FOLK for "run preview where
auto-popup is on"; V for the minibuffer caveat — minad/vertico discussion #512]

**Shipped:** `completion-preview` block enabled across
`jotain-completion-auto-modes`, gated by `jotain-completion-inline-preview`
(default t), with the C-i unbind, the M-RET accept, the idle-delay pairing, the
sort pairing, and the comment/string inhibit.

### 5.4 Residual — needs eyes on a GUI [I]

Two things batch tests cannot settle, recorded honestly, same as the 2026-07
design did for `corfu-complete`'s feel:

1. Whether ghost + popup together feels integrated or busy in day-to-day coding.
   Mitigated structurally (`corfu-preview-current nil`, matched sorts) and by the
   one-toggle opt-out `jotain-completion-inline-preview`.
2. Whether prefix-only ghost (from `completion-preview-completion-styles '(basic)`)
   ever disagrees jarringly with an orderless popup top row. If so, the lever is
   aligning that defvar with `completion-styles`, at the cost of a fuzzy ghost.

---

## 6. Options considered and *not* taken

- **`corfu-preselect 'prompt`** — would keep typed input selected so an
  accidental accept inserts your own text. With `corfu-preview-current nil`
  already removing auto-insert, the marginal safety is small and it complicates
  accepting the first candidate; left at the default `valid`. [V — defcustom;
  decision: out of scope]
- **Buffer-local `orderless-literal-only`/`basic` style for corfu** (the README's
  "separate styles for minibuffer vs. Corfu" recipe) — a real "more predictable
  in-buffer matching" lever, but it changes matching semantics the owner has been
  living with and is orthogonal to "annoying popup." Deferred as a separate
  decision, not bundled into this round. [V — README recipe]
- **`corfu-on-exact-match`** — already `nil` (no surprising auto-insert); no
  change. [V]
- **Width-pinning (`corfu-min-width`/`-max-width`) and hiding the scrollbar** —
  cosmetic calm-feel tweaks; not "annoying-usage" fixes. Left alone. [CFG]

---

## 7. Sources

- corfu.el, `extensions/corfu-popupinfo.el`, `extensions/corfu-echo.el`, and
  README — <https://github.com/minad/corfu>; GNU ELPA doc
  <https://elpa.gnu.org/packages/doc/corfu.html>.
- `completion-preview.el` on the `emacs-31` branch —
  <https://raw.githubusercontent.com/emacs-mirror/emacs/emacs-31/lisp/completion-preview.el>.
- Emacs NEWS.30 ("New Modes and Packages in Emacs 30.1") and NEWS.31
  ("Minibuffer and Completions") — emacs-mirror/emacs, branches `emacs-30` / `emacs-31`.
- minad/vertico discussion #512 (completion-preview in the minibuffer is not
  recommended).
- Prot's Emacs configuration (2024-11-28) for `corfu-preview-current nil` and the
  popupinfo delay — <https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/>.
