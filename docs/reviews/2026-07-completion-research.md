# Completion, snippets, and input assist — research report

**Date:** 2026-07-26
**Scope:** In-buffer completion, snippet expansion, and adjacent input-assist
facilities for this configuration — built-in Emacs 30/31, the third-party
corfu/cape/tempel stack, and the surrounding landscape.
**Status:** Research only. The companion design document is
[`docs/design/completion.md`](../design/completion.md); no Elisp was changed in
the round that produced this report.

## How to read this

Every claim below carries a source. Claims are marked:

- **[V]** — verified against a primary source (GNU manual, NEWS, or package
  source), and adversarially re-checked.
- **[I]** — a sound inference *from* a verified source, but not something the
  source states in those words. Flagged because the distinction matters when
  someone later quotes this document as authority.
- **[R]** — a claim that was investigated and **refuted**. Recorded so it is not
  re-proposed.
- **[?]** — investigated, unresolved. Do not build on it.

Where Emacs 30 and 31 differ, the difference is called out explicitly. NEWS
files reset per release: `master/etc/NEWS` is Emacs 32 NEWS as of this writing,
so all NEWS citations pin a branch (`emacs-30`, `emacs-31`) or a versioned file
(`etc/NEWS.28`, `etc/NEWS.31`).

---

## Part 1 — The capf protocol

### 1.1 The contract [V]

A function on `completion-at-point-functions` returns either `nil`, or a list
`(START END COLLECTION . PROPS)`. `START`/`END` delimit the text being completed
and must enclose point; `COLLECTION` must be usable as `try-completion`'s second
argument; `PROPS` accepts any property from `completion-extra-properties` plus
the capf-specific `:predicate` and `:exclusive`.

> "Each function should return nil unless it can and wants to take
> responsibility for the completion data for the text at point. Otherwise it
> should return a list of the following form: `(start end collection . props)`"
>
> — *GNU Emacs Lisp Reference Manual*, [Completion in Buffers](https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion-in-Buffers.html)

A third return form exists and is discouraged: a function of no arguments,
"only intended to help convert old code". A fourth, undocumented category is
tracked internally — `completion--capf-misbehave-funs` (`lisp/minibuffer.el`
:3256-3263) records capfs that complete immediately.

**Why this matters here:** `cape-capf-super` and corfu's wrappers only handle
the list form. A capf returning the legacy function form will not compose.

Two further specifics from the same `@defvar`, both directly relevant to
auto-popup tuning:

- Capfs "should generally return quickly, since they may be called very often
  (e.g., from `post-command-hook`)" — expensive collections should use
  `completion-table-dynamic`.
- "the collection should generally not be pre-filtered based on the current text
  between start and end, because that is the responsibility of the caller …
  according to the completion styles it decides to use" — i.e. a capf must not
  do orderless's job.

The manual text was diffed between the Emacs 31 tree and published Emacs 30:
byte-for-byte identical. **No 30-vs-31 drift.**

### 1.2 Chaining terminates at the first non-nil return [V]

> "The first function in `completion-at-point-functions` to return a non-nil
> value is used by `completion-at-point`. The remaining functions are not
> called. The exception to this is when there is an `:exclusive`
> specification."
>
> — *Elisp manual*, Completion in Buffers (minbuf.texi:2223-2226)

The only documented escape is `:exclusive` with the value **`no`** — the manual
documents no other value:

> ":exclusive — If the value is `no`, then if the completion table fails to
> match the text at point, `completion-at-point` moves on to the next function."

Implementation: `completion-at-point` runs
`(run-hook-wrapped 'completion-at-point-functions #'completion--capf-wrapper 'all)`,
and the wrapper's sole fallthrough is

```elisp
(and (eq 'no (plist-get (nthcdr 3 res) :exclusive))
     (null (try-completion ... (nth 2 res) ...))
     (setq res nil))
```

### 1.3 The shadowing folklore is close to inverted [V]

This is the single most consequential correction in this report for the current
configuration.

The common framing — "prepending tempel shadows the LSP" — does not hold:

- **tempel is non-exclusive.** `tempel.el` (main, lines 806 and 833) returns
  `:exclusive 'no` from **both** `tempel-expand` and `tempel-complete`. Prepended
  tempel is precisely the case the `:exclusive no` exception rescues: it falls
  through to eglot unless a template name actually prefix-matches.
- **eglot is exclusive.** `lisp/progmodes/eglot.el` on master contains **no
  occurrence of the string `exclusive` at all**, so eglot's capf is exclusive by
  default.

The real-world shadowing is therefore **eglot suppressing capfs placed *after*
it** — which is exactly why cape ships `cape-capf-nonexclusive` /
`cape-wrap-nonexclusive` (`cape.el`:1173, README:264).

**Caveat on the fallthrough [V]:** it is approximate by design.
`completion--capf-wrapper` carries its own FIXME —

> "depends on the actual completion UI … We approximate this result by checking
> whether prefix completion might work, which means that non-prefix completion
> will not work (or not right) for completion functions that are non-exclusive."

cape mirrors this in `cape-wrap-choose`. Since this config runs orderless (a
non-prefix style), whether the approximation misfires here is a live question —
see the gap-pass findings in Part 6.

A grep of `etc/NEWS` (31 dev) and `etc/NEWS.30` shows **zero** changes to
`completion-at-point-functions` or `:exclusive` semantics: 30 and 31 behave
identically.

### 1.4 Hook ordering: buffer-local, global, and the `t` element [V]

> "the Capfs which occur earlier in the list take precedence, such that the
> first Capf returning a result will win and the later Capfs may not get a
> chance to run. In order to merge Capfs you can try the function
> `cape-capf-super`."
>
> "The buffer-local value of the list takes precedence, but if the buffer-local
> list contains the symbol `t` at the end, it means that the functions specified
> in the global list should be executed afterwards. The special meaning of the
> value `t` is a feature of the `run-hooks` function."
>
> — [cape README](https://github.com/minad/cape/blob/main/README.org)

Independently corroborated by the Elisp manual's
[Running Hooks](https://www.gnu.org/software/emacs/manual/html_node/elisp/Running-Hooks.html)
node: "if the buffer-local variable contains the element `t`, the global hook
variable will be run as well."

**Precision [V]:** the `t` semantics are *positional* — global capfs run where
`t` appears in the list, not necessarily last. cape says "at the end" because
that is what `add-hook` produces. `completion-at-point` iterates via
`run-hook-wrapped`, which honors the same convention.

### 1.5 Merging vs. trying in turn [V]

`cape-capf-super` is **not** the tool for sequential fallback:

> "`cape-capf-super` is not needed if multiple Capfs should be tried one after
> another … only necessary if you want to combine multiple Capfs, such that the
> candidates from multiple sources appear *together* in the completion list at
> the same time."

The try-in-turn combinator is `cape-capf-choose` (`defalias` of
`cape-wrap-choose`), whose docstring reads "Call each of CAPFS in turn and
return first non-nil result."

`cape-capf-super`'s documented restrictions:

> "Capf merging requires completion functions which are sufficiently well-behaved
> and completion functions which do not define completion boundaries.
> `cape-capf-super` has the same restrictions as `completion-table-merge` and
> `completion-table-in-turn`. As a simple rule of thumb, `cape-capf-super` works
> for static completion functions like `cape-dabbrev`, `cape-keyword`,
> `cape-dict`, etc., but not for multi-step completions like `cape-file`."

And on caching:

> "`cape-capf-super` creates a Capf, which caches the candidates for the whole
> lifetime of the Capf. Therefore you may want to combine a merged Capf with a
> cache buster."

`cape.el`:930 adds: "cape-capf-super currently cannot merge Capfs which trigger
at different beginning positions." `cape-wrap-super` also accepts a `:with`
keyword marking auxiliary capfs.

**Bearing on the current config:** `lisp/init-snippets.el`:70 merges
`tempel-complete` with `eglot-completion-at-point` via `cape-capf-super`. Given
1.3, the *shadowing* rationale for that merge is weaker than the comment in the
file states — but the merge still buys a genuine benefit the fallthrough does
not: snippet and server candidates appear in **one** popup simultaneously rather
than the server's list only appearing when no template matches. The caching
caveat above applies and is unaddressed (no cache buster).

### 1.6 `:exit-function` — what a composed capf must preserve [V]

`:exit-function` takes two arguments `(STRING STATUS)`, where STATUS is exactly
one of `finished`, `sole`, `exact`:

> "The function should accept two arguments, string and status … `finished` if
> text is now complete, `sole` if the text cannot be further completed but
> completion is not finished, or `exact` if the text is a valid completion but
> may be further completed."
>
> — *Elisp manual*, [Completion Variables](https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion-Variables.html)

This is the mechanism by which **eglot expands LSP snippets and applies
`textEdit`/`additionalTextEdits`**, and by which **tempel expands a template**.
Concretely:

```elisp
;; eglot.el:4068ff
:exit-function (lambda (proxy status)
                 (eglot--capf-session-flush)
                 (when (memq status '(finished exact)) …))
```

Note eglot does **nothing** on `sole`. tempel returns
`:exit-function (apply-partially #'tempel--exit templates nil)` from
`tempel-complete`, and `(… templates region)` from `tempel-expand`.

cape re-splices these per-candidate. `cape--super-functions` is

```elisp
'(:company-docsig :company-location :company-kind :company-doc-buffer
  :company-deprecated :annotation-function :exit-function)
```

dispatching back to the originating capf via a `cape--super` text property.
`cape.el`:528 and :734 carry the comment "No cycling since it breaks the
`:exit-function`."

corfu's `corfu--exit-function` restores text properties precisely because eglot
reads `(get-text-property 0 'eglot--lsp-item proxy)`.

**Design rule [I]:** a custom dwim command that wraps or re-implements
`completion-at-point` must preserve `:exit-function` and should guard on
`(memq status '(finished exact))` as eglot does. `unknown` exists only as an
internal pre-normalization sentinel in `completion--done` and never reaches an
exit-function.

### 1.7 `completion-styles` is a fallback chain, not a union [V]

> "Emacs looks for the first style listed in `completion-styles` and calls its
> `try-completion` function. If this function returns nil, Emacs moves to the
> next listed completion style … until one of the try-completion functions
> successfully performs completion and returns a non-nil value. A similar
> procedure is used for listing completions, via the `all-completions`
> functions."

Implementation: `completion--nth-completion` dispatches with
`(seq-some (lambda (style) …) (completion--styles md))` — `seq-some` stops at the
first non-nil — and is the shared entry point for both `completion-try-completion`
and `completion-all-completions`.

Candidate sets from different styles are therefore **never merged**. This is the
documented reason the position of `orderless` relative to `basic` /
`partial-completion` / `flex` matters. The orderless README's own note that
"the basic completion style needs to be tried *first* (not as a fallback) for
TRAMP hostname completion to work" would be meaningless under a union model.

**Refinements [V]:** the per-category escape hatch is
`completion-category-overrides` / `completion-category-defaults`, so ordering
must be reasoned about per category (`file`, `buffer`, `eglot-capf`, …), not
globally. Elements may take the form `(STYLE ((VARIABLE VALUE) …))` to bind
variables only while that style runs.

**Scope note [I]:** the manual paragraph is framed around minibuffer commands.
It applies to in-buffer corfu completion because corfu routes through
`completion-all-completions` — that last step is inference, not manual text.

---

## Part 2 — TAB

### 2.1 "TAB indents only" is the stock default, not a deviation [V]

`tab-always-indent` defaults to **`t`** in both Emacs 30 and Emacs 31:

```elisp
;; indent.el (master)
(defcustom tab-always-indent t
  "Controls the operation of the TAB key.
If t, hitting TAB always just indents the current line. …
Some programming language modes have their own variable to control this,
e.g., `c-tab-always-indent', and do not respect this variable.")
```

`indent-for-tab-command`'s **only** call to `completion-at-point` sits in a cond
arm whose first conjunct is `(eq tab-always-indent 'complete)`. With the default
`t` that branch is provably unreachable: TAB cannot invoke a capf.

A verifier diffed master's `indent.el` against the `emacs-30` branch: byte-identical
except the copyright year. **No 30-vs-31 drift.**

The strongest available counter-evidence cuts the same way: Emacs 31 NEWS
announces a new **opt-in** theme `newcomers-presets` whose body contains
`'(tab-always-indent 'complete)` — i.e. 31 still defaults to `t` and needs an
explicit theme to put completion on TAB.

**[R] Refuted 0-3:** the claim that corfu's README recommends
`tab-always-indent` = `complete`, i.e. that moving completion off TAB deviates
from upstream corfu guidance. There is no evidence of such a recommendation.

**[R] Refuted 0-3:** the claim that `tab-always-indent` = `complete` is the
*only* documented way TAB invokes completion. Modes that rebind TAB in their own
keymap are a counterexample — see 2.2.

### 2.2 What "TAB never completes" still requires [V]

The claim in 2.1 is scoped to `indent-for-tab-command`. End-to-end, four things
bypass the variable:

1. **corfu's own keymap.** `corfu-map` binds `"TAB"` → `corfu-complete` (plus
   `"M-TAB"` → `corfu-expand`, `"RET"` → `corfu-insert`). A literal
   "TAB never completes" therefore requires unbinding TAB in `corfu-map`.
   *This is a live decision for the design document.*
2. **Modes that rebind TAB in their keymap** bypass the variable entirely:
   `org-cycle`, message-mode's `message-tab`, shell-mode's
   `(keymap-set shell-mode-map "TAB" 'completion-at-point)`.
3. **Modes with their own variable**: `c-tab-always-indent` (also default `t`),
   `cperl-tab-always-indent`. Some modes `setq-local tab-always-indent nil`
   (asm-mode, change-log-mode, snmp-mode) — that inserts a literal tab past the
   indentation, still not completion.
4. **Minibuffer and comint** TAB never routes through `tab-always-indent`.

Additionally, this repo's `lisp/init-snippets.el`:42 binds `TAB` → `tempel-next`
in `tempel-map`, which is active only while a template session is live. That is
a fifth TAB consumer, and an intentional one.

### 2.3 `tab-first-completion` — scope correction [V]

**The variable was added in Emacs 28.1, not Emacs 30.** It carries
`:version "28.1"`, is announced in `etc/NEWS.28` ("New user option
'tab-first-completion'"), and is **absent from `etc/NEWS.30`**. Its defcustom
body is byte-identical across the `emacs-30`, `emacs-31`, and `master` branches.

**It is not a predicate option.** The `:type` is a closed choice of exactly five
symbols:

```elisp
(choice (const :tag "Always complete" nil)
        (const :tag "Only complete at the end of a line" eol)
        (const :tag "Unless looking at a word" word)
        (const :tag "Unless at a word or parenthesis" word-or-paren)
        (const :tag "Unless at a word, parenthesis, or punctuation." word-or-paren-or-punct))
```

Dispatch is a `pcase` on a syntax class, not a `funcall` (indent.el ~189-194):

```elisp
(let ((syn (syntax-class (syntax-after (point)))))
  (pcase tab-first-completion
    ('nil t)
    ('eol (eolp))
    ('word (not (eql 2 syn)))
    ('word-or-paren (not (memq syn '(2 4 5))))
    ('word-or-paren-or-punct (not (memq syn '(2 4 5 1))))))
```

A lambda would match no arm and silently degrade to "never complete on first
TAB". The predicate reading is not merely undocumented — it is *unimplemented*.

**It is inert unless `tab-always-indent` is `complete`.** That pcase sits inside
a cond clause guarded by `(eq tab-always-indent 'complete)`, and a repo-wide code
search finds only four references (indent.el, doc/emacs/indent.texi,
doc/lispref/text.texi, etc/NEWS.28). There is no second code path. The docstring
closes: "This variable has no effect unless `tab-always-indent' is `complete'."

**Consequence for this configuration:** with TAB on indent-only,
`tab-first-completion` is irrelevant and should be documented as such rather than
tuned. (This retires the "'complete + tab-first-completion guard" option that was
on the table during scoping.)

Note also that the Emacs *user* manual documents only one of the five values
(`eol`) and defers to the Elisp node Mode-Specific Indent — that page alone
cannot enumerate the option.

### 2.4 The explicit key: `C-M-i` / `ESC TAB`, never `M-TAB` [V]

> "In most programming language modes, `C-M-i` (or `M-TAB`) invokes the command
> `completion-at-point` …"
>
> "On graphical displays, the `M-TAB` key is usually reserved by the window
> manager for switching graphical windows, so you should type `C-M-i` or
> `ESC TAB` instead."
>
> — *Emacs manual*, [Symbol Completion](https://www.gnu.org/software/emacs/manual/html_node/emacs/Symbol-Completion.html)

The Elisp manual independently states "In many major modes, in-buffer completion
is performed by the `C-M-i` or `M-TAB` command, bound to `completion-at-point`."
Two primary manuals agree.

**Mechanism [V]:** the window manager steals the *function-key event* `<M-tab>`
(Alt+Tab). `C-M-i` is the ASCII meta-C-i event, which is why Emacs still sees it.
They are distinct events **only on window systems** — on a TTY the terminal
cannot distinguish TAB from C-i, so `C-M-i`, `M-TAB` and `ESC TAB` all arrive as
the same ESC+C-i sequence.

**Consequences for the design:**

- Binding a dwim command to `C-M-i` is safe on GUI **and** TTY.
- It cannot be made *distinct* from `ESC TAB` in a terminal.
- Doing so **overrides a documented standard binding** (the global binding is
  historically `complete-symbol` in `esc-map`, delegating to
  `completion-at-point`). That is a deliberate choice to document, not a
  correctness problem.
- The manual's "in many/most major modes" is a real hedge — modes do rebind
  these.

**Existence proof [I]:** vanilla Emacs — `tab-always-indent` = `t`, completion on
`C-M-i` — *is* the configuration this design wants, minus the snippet chaining.

---

## Part 3 — Core completion preview (Emacs 30/31)

### 3.1 What it is, and the conditional TAB conflict [V]

Completion Preview mode is core inline ghost text, new in Emacs 30. Both
`completion-preview-mode` and `global-completion-preview-mode` are **off by
default**.

> "Completion Preview mode is a minor mode that shows completion suggestions as
> you type. … Emacs automatically displays the suggested completion for text
> around point as an in-line preview right after point; type TAB to accept the
> suggestion."

The TAB binding lives in the **transient** `completion-preview-active-mode-map`
(binds `"C-i"` → `completion-preview-insert`, `"M-i"` →
`completion-preview-complete`), active only while a preview overlay is displayed.

Eshel Yaron, the implementer:

> "Crucially, whenever the preview is shown, TAB inserts its contents.
> Otherwise, Completion Preview mode doesn't bind any keys."

**So [I]:** with `tab-always-indent` = `t`, TAB still indents normally; it is
hijacked only during the preview window, via minor-mode keymap precedence over
`indent-for-tab-command`. This is a genuine conflict for a TAB-indents-only
design, but *conditional* and *only if opted into*. The manual documents the
binding; the conflict is a mechanically verifiable inference, not manual text.

Defcustoms present: `completion-preview-minimum-symbol-length` (default **3**),
`completion-preview-idle-delay` (default **nil**), `completion-preview-commands`,
`completion-preview-exact-match-only`, `completion-preview-ignore-case`,
`completion-preview-adapt-background-color`, `completion-preview-sort-function`.

`global-completion-preview-mode` carries a `:predicate` excluding archive-mode,
calc-mode, compilation-mode, diff-mode, dired-mode, image-mode, minibuffer-mode,
minibuffer-inactive-mode, org-agenda-mode, special-mode, wdired-mode.

Documented escapes from the TAB binding: rebind inside
`completion-preview-active-mode-map`, or (on 31) use
`completion-preview-inhibit-functions`.

### 3.2 Emacs 31 adds the sanctioned per-context gate [V]

`etc/NEWS.31` line 309:

> "*** New user option 'completion-preview-inhibit-functions'. This option
> provides fine-grained control over Completion Preview mode activation. You can
> use it to specify arbitrary conditions in which to inhibit the mode's
> operation."

The defcustom (`completion-preview.el`:179-185) carries `:version "31.1"`:

> "Completion Preview mode calls the functions on this hook without arguments
> during `post-command-hook'. If any of these functions returns non-nil, it
> inhibits the preview display."

Wired in at line 648:
`(not (run-hook-with-args-until-success 'completion-preview-inhibit-functions))`
short-circuits `completion-preview--show`.

**Absent in Emacs 30 [V]:** a grep of the `emacs-30` branch finds only internal
`completion-preview--inhibit-update-p` / `--inhibit-update`. Emacs 30 NEWS
mentions completion-preview exactly once ("** New minor mode
'completion-preview-mode'.").

Upstream's own Commentary shows the intended pattern:
`(add-hook 'completion-preview-inhibit-functions (lambda () executing-kbd-macro))`.
A `(nth 4 (syntax-ppss))` comment/string predicate is a straightforward
application — **[I]**, that specific recipe is inference, not upstream wording.

**Precision [V]:** it inhibits the preview *display* per command; it does not
toggle the mode off. An already-shown preview is torn down via
`(completion-preview-active-mode -1)`.

### 3.3 `completion-preview-sort-function` and the eglot no-op [V]

`etc/NEWS.31`:

> "If you use this mode together with an in-buffer completion popup interface,
> such as the interfaces that the GNU ELPA packages Corfu and Company provide,
> you can set this option to the same sort function that your popup interface
> uses for a more integrated experience. ('completion-preview-sort-function' was
> already present in Emacs 30.1, but as a plain Lisp variable, not a user
> option.)"

Confirmed by commit `8b9194ae` (Eshel Yaron, 2025-02-28), whose diff shows
`defvar` → `defcustom` with `:version "31.1"`.

**Decision-relevant caveat [V]:** the docstring adds "If the completion table
that produces the candidates already specifies a sort function, it takes
precedence over this option." Since eglot's capf supplies
`:display-sort-function`, aligning this option with corfu's sorting is a **no-op
in eglot-driven buffers**.

**[I]** NEWS never contrasts coexistence with replacement. "Completion preview is
intended to coexist with, rather than replace, corfu" is a fair reading of the
Corfu/Company sentence above, but it is an editorial gloss, not upstream wording.

---

## Part 4 — corfu

### 4.1 `corfu-auto` latches at mode-enable time [V]

This is the mechanism the "auto in code, manual in prose" requirement depends on.

> "Auto completion is disabled by default in Corfu. … Auto completion can be
> enabled by setting `corfu-auto` to t locally or globally **before enabling**
> the local `corfu-mode` or the `global-corfu-mode`."
>
> — [corfu README](https://raw.githubusercontent.com/minad/corfu/main/README.org)

A verifier grepped `corfu.el` for `corfu-auto` and found exactly **two** sites:
the defcustom, and **one** runtime read in the mode body —

```elisp
(when corfu-auto
  (require 'corfu-auto)
  (add-hook 'post-command-hook 'corfu-auto--post-command 10 'local))
```

The verifier specifically hunted for a runtime re-check that would refute the
ordering requirement. `corfu-auto--post-command` gates on
`completion-in-region-mode`, `defining-kbd-macro`, `buffer-read-only`,
`(corfu--match-symbol-p corfu-auto-commands this-command)`,
`corfu--popup-support-p`, then `corfu-auto-delay` / `corfu-auto-trigger` — it
**never examines `corfu-auto`**. The value genuinely latches at mode-enable time.
The ordering requirement is real, not folklore.

**Why the `prog-mode-hook` pattern works [V]:** `global-corfu-mode` is a
`define-globalized-minor-mode` dispatching through `corfu--on` from
`after-change-major-mode-hook`, which `run-mode-hooks` runs **after** major-mode
hooks. So:

```elisp
(add-hook 'prog-mode-hook (lambda () (setq-local corfu-auto t)))
```

is correctly ordered against `global-corfu-mode`.

**Two failure modes [V]:**

1. `define-minor-mode` runs the mode hook *after* the body, so setting
   `corfu-auto` in `corfu-mode-hook` is **too late**.
2. Buffers already open when `global-corfu-mode` is first enabled do not re-run
   their major-mode hooks and keep whatever `corfu-auto` value they had. Relevant
   at startup ordering and after a config reload.

**Honest scoping [I]:** calling this "*the* prog/prose mechanism" is
editorializing. Upstream's own documented per-mode knob is `global-corfu-modes`
(the `:predicate`-generated defcustom), which is an **on/off knob for corfu-mode
itself, not an auto/manual knob**. The README also documents enabling
`corfu-mode` per-mode via hooks instead of globally. The setq-local-in-hook
technique is *a* mechanism licensed by the README sentence quoted above, not
presented upstream as *the* recipe.

corfu 2.x moved auto-completion into `extensions/corfu-auto.el`, which makes the
latch behavior stronger, not weaker.

### 4.2 Trigger-character gating — upstream prior art [V]

Upstream ships a first-party recipe for gating a merged snippet capf behind a
**trigger character**. From the cape README (appears twice — in the
`cape-capf-super` section and as "Example 1: Configure a merged Capf with a
trigger prefix character"):

```elisp
;; Trigger completion only after trigger character.
(setq-local corfu-auto-trigger "/"
            completion-at-point-functions
            (list (cape-capf-trigger
                   (cape-capf-super #'cape-abbrev #'tempel-complete) ?/)))
```

Both APIs exist in shipped code, not just README prose:

- `cape.el`:1286 — `(defun cape-wrap-trigger (capf trigger) "Ensure that TRIGGER
  character occurs before point and then call CAPF. See also
  `corfu-auto-trigger'.")`. It searches back to `pos-bol` for the trigger char,
  requires no whitespace between it and point, treats the trigger as punctuation
  syntax, and rewrites `beg` to `(1+ pos)` with `:company-prefix-length t`.
- `corfu-auto.el`:35 — `(defcustom corfu-auto-trigger "" "Characters which
  trigger auto completion. If a trigger character is detected `corfu-auto-prefix'
  is ignored." :type 'string)`, consumed via `seq-contains-p` on
  `last-command-event`, bypassing **both** `corfu-auto-prefix` and
  `corfu-auto-delay`.

**Version floors [V]:** cape ≥ 2.3 and corfu ≥ 2.5 (both changelogs dated
2025-11-15); still present in cape 2.7 / corfu 2.11. corfu 2.x requires
`extensions/corfu-auto.el` to be loaded.

**Two qualifications [V]:**

- This is a **gated auto-popup** recipe, **not a keybinding recipe** — the README
  proposes no key. Do not cite it as prior art for a dwim keybinding; cite it
  only for trigger-character gating.
- It replaces `completion-at-point-functions` buffer-locally with a **single**
  capf. **No eglot coexistence is demonstrated.**

### 4.3 TTY child frames — a live 30-vs-31 split for this repo [V]

corfu's child-frame popup renders natively in terminal Emacs starting with
**Emacs 31**; the separate `corfu-terminal` package is needed only on 30 and
below.

corfu README, twice: "Corfu relies on child frames to show the popup. On Emacs
31 this works for terminal Emacs. Use the `corfu-terminal` package on older Emacs
versions."

Decisive upstream corroboration — `etc/NEWS.31`:

> "Child frames are now supported on TTY frames. This supports use-cases like
> Posframe, Corfu, and child frames acting like tooltips."

NEWS names Corfu explicitly and documents the feature-detection predicate
`(featurep 'tty-child-frames)`. No opt-in variable gates general child-frame
support (`tty-tip-mode` is tooltip-specific). Further confirmed by corfu commit
`f17fe365d2` "Emacs 31 supports child frames on TTY 🎉", which removed the old
README text about falling back on non-graphical terminals.

**Relevance to this repo [V]:** per `CLAUDE.md` the default build is the
emacs-overlay `unstable` variant — the Emacs 31 release branch, currently the
**31.0.90 pretest** — while the declared floor is **30.1**. This split is live
here. Because the pretest is 31.0.90 rather than 31.1, **a
`(featurep 'tty-child-frames)` gate is correct where a version comparison would
be wrong.** The repo ships a terminal-only distribution
(`jotainEmacsPackagesNoGui`, used by the nix-on-droid module), so this is not
hypothetical.

Adjacent known bug: [bug#76052](https://debbugs.gnu.org/76052), TTY child frames
for Corfu offset upward at the bottom of a window — a polish issue that confirms
rather than contradicts the mechanism.

**Stale-source warning:** corfu-terminal's NonGNU ELPA page (v0.7, 2024-03-31)
still says flatly "Corfu uses child frames … This makes Corfu unusable on
terminal" and never mentions Emacs 31. Stale silence, not a contradiction — but
a reader consulting only that page would be misled.

---

## Part 5 — Prose-mode completion (the `M-TAB` question), resolved

The first research round left this contradictory: one claim that "M-TAB in Text
mode completes from the spell-checker's dictionary" was refuted 0-3, while a
different verifier quoted the manual saying exactly that. A dedicated pass over
source resolved it. **Both prior positions were partly right.**

### 5.1 What text-mode actually installs [V]

Since **Emacs 30.1**, the body of `(define-derived-mode text-mode …)` ends with —
byte-identical on `emacs-30` (lines 157-158), `emacs-31` and `master` (155-156):

```elisp
(add-hook 'context-menu-functions 'text-mode-context-menu 10 t)
(when (eq text-mode-ispell-word-completion 'completion-at-point)
  (add-hook 'completion-at-point-functions #'ispell-completion-at-point 10 t))
```

Decoded against `add-hook`'s signature `(hook function &optional depth local)`:
**depth 10, buffer-local**. This is the *only* executable reference to
`completion-at-point-functions` in `text-mode.el` (the other two occurrences are
inside a docstring).

`ispell-completion-at-point` is a real capf (`ispell.el`:3718 on emacs-30, :3783
on emacs-31), docstring "Word completion function for use in
`completion-at-point-functions'", returning `(list beg end (cdr all) :exclusive 'no)`.

**Consequence for this config [V]:** depth 10 appends **late**. Any capf added at
default depth 0 — `tempel-complete`, `cape-dabbrev`, eglot — runs *before* it. The
ispell capf is a **fallback, not a shadow**. ("Depth 10 = last" is a loose gloss:
depths range −100..100, and with `LOCAL=t` the `t` sentinel sits at the very end
of the buffer-local list.)

### 5.2 The option [V]

```elisp
(defcustom text-mode-ispell-word-completion 'completion-at-point
  "How Text mode provides Ispell word completion. …"
  :group 'text
  :type '(choice (const completion-at-point) boolean)
  :version "30.1"
  :set (lambda (sym val) …))
```

Three meaningful values: `completion-at-point` (capf — the default), any other
non-nil (bind `C-M-i` directly to `ispell-complete-word`), `nil` (neither).

Introduced via bug#67527 (patch by Eshel Yaron, installed 2024-01-27). Emacs 29
has no such defcustom at all — only an unconditional
`"C-M-i" #'ispell-complete-word` in `text-mode-map`.

**Emacs 30 vs 31 doc-only delta [V]:** emacs-30's docstring carries the sentence
"This user option only takes effect when you customize it in Custom or with
`setopt', not with `setq'." — **deleted in 31 and master** while the `:set` lambda
that motivated it was kept. Anyone citing that sentence for an Emacs 31 config is
citing removed text.

### 5.3 The key path — a correction worth propagating [V]

By default `text-mode-map` has **no** `C-M-i` / `M-TAB` binding. The defcustom's
`:set` lambda *removes* it via `(keymap-unset text-mode-map "C-M-i" t)` whenever
the value is `nil` or `completion-at-point`. Because `text-mode.el` is preloaded
(`loadup.el`:283), the dump-time `custom-initialize-reset` already ran that
branch — C-M-i is unbound in `text-mode-map` out of the box.

So `M-TAB` falls through to the **global** `esc-map` binding. And NEWS and the
manual both say "completion-at-point, globally bound to M-TAB" — but the actual
binding (`bindings.el`:952 on 30, :1061-1063 on 31) is:

```elisp
(define-key esc-map "\t" 'complete-symbol)
```

where `complete-symbol` is `(if arg (info-complete-symbol) (completion-at-point))`.
Bare `C-M-i` is behaviorally identical; **`C-u C-M-i` runs `info-complete-symbol`
instead**. Precise phrasing: *global `C-M-i` → `complete-symbol`, which calls
`completion-at-point`.*

### 5.4 What setting it to nil removes [V]

Exactly two things: the (default-absent) `C-M-i` binding, and the buffer-local
`ispell-completion-at-point` capf entry. Nothing else. Proven by grep, not
inference: a tree-wide search for the option returns `total_count=3` —
`text-mode.el`, `doc/emacs/text.texi`, `etc/NEWS.30`. No other Lisp file reads it;
`ispell.el` never references it and has no `remove-hook` for the capf.

It is read at **mode-entry** time, so `nil` does not retroactively strip the capf
from already-live text-mode buffers.

**A precision correction [V]:** the blanket "setq silently fails here" is an
overreach. Only the *keymap* half lives in `:set`, and since text-mode.el is
preloaded a `setq` can never update `text-mode-map`. But the *capf* half is read
from the live variable inside the mode body, so `(setq text-mode-ispell-word-completion nil)`
**does** stop the capf from being installed in newly-entered buffers — the
practically decisive effect. `setq` genuinely fails only when moving to a non-nil,
non-`completion-at-point` value. Given this repo's `setopt` rule the point is
moot, but the mechanism should be stated correctly.

**Bearing on `init-writing.el`:41-42 [V]:** the existing comment says the Emacs 30
ispell capf throws "No plain word-list found" on NixOS where
`ispell-alternate-dictionary` has no file. Setting the option to `nil` is a
correct and sufficient fix for that, and it removes nothing else.

### 5.5 The manual now agrees [V]

`emacs-31 doc/emacs/fixit.texi` (Spelling node):

```texinfo
@item M-@key{TAB}
@itemx @key{ESC} @key{TAB}
@itemx C-M-i
Complete the word before point based on the spelling dictionary and
other completion sources (@code{completion-at-point}).
```

The Emacs **29** manual said the opposite — "(@code{ispell-complete-word})" —
which is almost certainly what the first round's 0-3 refutation was reasoning
from. `ispell-complete-word` itself is **not** obsolete: a live defun on both
branches, with no `make-obsolete`.

**Adjudication:** M-TAB in text modes *does* complete from the spell-checker's
dictionary — but as **one capf among others**, at depth 10, reached via
`complete-symbol` → `completion-at-point`, not via a dedicated key binding.

### 5.6 A key-safety hazard the design must dodge [V]

**With `flyspell-mode` enabled, `M-TAB` / `ESC TAB` / `C-M-i` is not a completion
key at all.** `flyspell.el`:430-438:

```elisp
(defvar flyspell-mode-map
  (let ((map (make-sparse-keymap)))
    (if flyspell-use-meta-tab (define-key map "\M-\t" 'flyspell-auto-correct-word))
    …))
```

`flyspell-use-meta-tab` defaults to `t` on both 30 and 31. As a **minor-mode**
keymap it outranks the global `esc-map` binding — so the completion key is fully
shadowed by spell correction in exactly the buffers where dictionary completion
was supposed to help.

**Opt-out:** set `flyspell-use-meta-tab` to `nil`; its `:set` lambda re-binds
`"\M-\t"` to nil live, so it works both before and after flyspell loads.

**Why this repo is *probably* already safe [I]:** `init-writing.el` uses **jinx**,
not flyspell (`global-jinx-mode` "replaces both `flyspell-mode` and
`flyspell-prog-mode` in one shot"). So `flyspell-mode-map` should never be active.
This is inference from the config, not a verified runtime check — any future move
back to flyspell, or any package enabling it transitively, silently breaks the
dwim key. The design document treats this as a documented risk.

---

## Part 6 — tempel, hippie-expand, and what remains unknown

### 6.1 tempel's capf surface [V]

Verified against `tempel.el` v1.14 (939 lines) on `main`; the emacs-straight
released mirror `diff -q`s **identical**, so `main` == what a NixOS user installs.

| Symbol | Kind | Returns |
| --- | --- | --- |
| `tempel-complete` | capf | **all** templates for the buffer |
| `tempel-expand` | capf | **single** exactly-matching template |
| `tempel-insert` | command (`completing-read`) | — |

Both capfs return `:category 'tempel`, `:exclusive 'no`, and an `:exit-function`
of `tempel--exit`. `tempel-expand`'s docstring: "returns only the single exactly
matching template name. As a consequence the completion UI (e.g. Corfu) does not
present the candidates for selection." `tempel-complete`'s: "returns a list of all
possible template names, which are then displayed in the completion UI."

Only `tempel-complete` emits `:company-kind` (`(lambda (_) 'snippet)`),
`:company-doc-buffer`, `:company-location`, and a **conditional**
`:annotation-function` — nil if `tempel-complete-annotation` (default 20) is nil.
`tempel-expand` emits none of these.

### 6.2 `tempel-trigger-prefix` does not exist [V]

A scope correction: **`tempel-trigger-prefix` is not in tempel 1.14.** `grep` for
`trigger-prefix` returns zero matches on both `main` and the released mirror.
Prefix detection is the internal `tempel--prefix-bounds` (lines 770-780), which
walks back over non-whitespace and accepts that span only if `try-completion`
against the template list succeeds, else falls back to
`(bounds-of-thing-at-point 'symbol)`.

The complete defcustom set is **nine**: `tempel-path`, `tempel-mark`,
`tempel-insert-annotation` (40), `tempel-complete-annotation` (20),
`tempel-user-elements` (nil), `tempel-template-sources`, `tempel-done-on-region`
(t), `tempel-done-on-next` (t), `tempel-auto-reload` (t). `tempel-map` is a
`defvar-keymap`, not a defcustom, but is still user-configurable.

**[?]** The suggested modern equivalent — `cape-capf-trigger` wrapped around
`tempel-complete` — was *not* itself verified. Treat as unverified.

### 6.3 Template and file format [V]

`.eld` files are a flat sequence `(MODES PLIST TEMPLATES MODES PLIST TEMPLATES…)`.
Leading non-keyword symbols are mode names; keyword/value pairs form a per-section
plist whose `:when` supplies a condition (defaulting to `t`); remaining conses are
templates. Mode matching is `derived-mode-p` plus `major-mode-remap-alist`
indirection, with `fundamental-mode` sections always matching.

Only `:when` is consumed from the *section* plist — any other section-level
keyword is **silently discarded**. Per-template `:pre`/`:post` are a separate
mechanism: a trailing plist on each template cons.

- `:pre` — a Lisp form evaluated **first** in `tempel--insert`, before the element
  loop.
- `:post` — stashed on the range overlay, evaluated by the finalizer
  `tempel--done` in the lexical scope of the named fields. **Not evaluated on
  `tempel-abort`.**

Element syntax (from `tempel--element`'s docstring): strings, `nil`, `p`, `r`,
`r>`, `n`, `n>`, `>`, `&`, `%`, `o`, `q`, `(s NAME)`, `(p PROMPT <NAME> <NOINSERT>)`,
`(r PROMPT …)`, `(r> PROMPT …)`, `(l ELEMENTS…)`, and arbitrary Lisp forms
dispatched first through `tempel-user-elements`, with string results dynamically
updated. Upstream's own warning: "Use caution with templates which execute
arbitrary code!"

`tempel-auto-reload` compares a whole `(file . mtime)` alist with `equal`, so file
**addition and removal** also trigger a reload, not just mtime bumps.

**Bearing on this repo [I]:** `templates/jotain.eld` uses exactly the documented
shape — bare mode symbols as section headers, `p`/`n`/`n>`/`r>`/`q` elements — and
`tempel-path` is set to `templates/*.eld`, which the wildcard support covers.

### 6.4 hippie-expand — the prior art for a dwim chain key [V]

This is the most directly applicable finding for the "separate chain key" design.

**Default `hippie-expand-try-functions-list`** (byte-identical on emacs-30,
emacs-31 and master; `hippie-exp.el`:204-218):

```elisp
'(try-complete-file-name-partially
  try-complete-file-name
  try-expand-all-abbrevs
  try-expand-list
  try-expand-line
  try-expand-dabbrev
  try-expand-dabbrev-all-buffers
  try-expand-dabbrev-from-kill
  try-complete-lisp-symbol-partially
  try-complete-lisp-symbol)
```

`:type '(repeat function)`, autoloaded, no `:version`/`:group`/`:set`. Note the
default is a **proper subset** of what the file defines — `try-expand-line-all-buffers`,
`try-expand-list-all-buffers` and `try-expand-whole-kill` are deliberately excluded.

**The sanctioned way to build a bespoke chain key [V]** —
`make-hippie-expand-function`, autoloaded, `(TRY-LIST &optional VERBOSE)`,
returning an interactive closure that dynamically binds
`hippie-expand-try-functions-list` to TRY-LIST and calls `hippie-expand`:

> "instead of loading the variable with all kinds of try-functions above, it might
> be an idea to use `make-hippie-expand-function' to construct different
> `hippie-expand'-like functions, with different try-lists and **bound to different
> keys**."
>
> — `hippie-exp.el` Commentary, lines 114-118

with `fset` examples at lines 394-400. Nothing in the file is marked obsolete
(`grep -n obsolete` → zero hits). It is a **defun returning a closure**, not a
macro — the file's own preceding comment calls it "a macro" sloppily.

**Cycling semantics [V]:** repeated invocation steps through candidates and then
forward through the try-list. A positive numeric ARG jumps ARG functions forward;
a negative arg, `C-u 0`, or plain `C-u` **undoes** the tried expansion. Three
precisions: (a) it does **not** wrap around — on exhaustion it messages "No further
expansions found" and `(ding)`; (b) with no ARG a repeat re-invokes the *same*
try-function with `old=t` to get its next candidate, advancing only once that
function returns nil — candidate-by-candidate, not one function per keypress;
(c) `C-u 0` also reaches the undo branch, which the manual wording omits.

**[R] Refuted 0-3:** the claim that the Commentary "explicitly sanctions" the
buffer-local/mode-hook pattern as *the* documented pattern for prose-vs-code
chains. The text at lines 118-120 does offer that path — "It is also possible to
make `hippie-expand-try-functions-list' a buffer local variable, and let it depend
on the mode (by setting it in the mode-hooks)" — but it carries no such normative
weight.

### 6.5 corfu's keymap — the RET and TAB question [V]

Default `corfu-map`, from the corfu README:

| Binding | Command |
| --- | --- |
| `completion-at-point`, `TAB` | `corfu-complete` |
| `M-TAB` | `corfu-expand` |
| `RET` | `corfu-insert` |
| `next-line`, `down`, `M-n` | `corfu-next` |
| `previous-line`, `up`, `M-p` | `corfu-previous` |
| `C-g` | `corfu-quit` |
| `M-SPC` | `corfu-insert-separator` |
| `M-h` | `corfu-info-documentation` |
| `M-g` | `corfu-info-location` |

> "The current candidate is inserted with `TAB` and selected with `RET`."

Upstream documents freeing RET explicitly:

```elisp
;; Free the RET key for less intrusive behavior.
;; Option 1: Unbind RET completely
(keymap-unset corfu-map "RET")
;; Option 2: Use RET only in shell modes
(keymap-set corfu-map "RET" `( menu-item "" nil :filter
                               ,(lambda (&optional _)
                                  (and (derived-mode-p 'eshell-mode 'comint-mode)
                                       #'corfu-send))))
```

This is decisive for the "Enter must never accept a completion" requirement: it is
a **supported, upstream-documented configuration**, not a hack. The `menu-item`
`:filter` form is also the general pattern for making a corfu binding
context-conditional.

**Not established this round:** exhaustive accepted-value lists for
`corfu-preselect`, `corfu-on-exact-match`, and `corfu-quit-no-match`. The README
shows `corfu-preselect` values `prompt` and `directory` by example only, and
`corfu-on-exact-match` via `'insert` by example. `corfu-quit-no-match` is
described with `separator` (default) and `t`. **Do not reproduce a complete value
table from this report** — read the docstrings before relying on any value not
listed here.

### 6.6 What remains unresearched

Reported as **not found** rather than inferred. Two rounds produced zero verified
claims on these, so the design document must not present them as evidence-backed:

- **Capf ordering, concretely (GAP 3).** Whether `cape-capf-nonexclusive` around
  `eglot-completion-at-point` is upstream-endorsed; how it compares with
  `cape-capf-super` merging; whether `completion--capf-wrapper`'s prefix-
  `try-completion` approximation for `:exclusive 'no` **misfires under orderless**
  (the `minibuffer.el` FIXME says non-prefix completion "will not work (or not
  right) for completion functions that are non-exclusive" — and this config runs
  orderless); and the behavior of `eglot--capf-session` / `eglot--capf-session-flush`,
  including whether `cape-capf-buster` is needed around eglot.

  The two things GAP 3 *can* lean on from verified findings: tempel's capfs are
  both `:exclusive 'no`, and the ispell capf sits at depth 10 — so tempel and
  ispell both fall through, and neither shadows eglot.

- **Opt-out / kill-switch idiom (GAP 5).** Neither the normative half (Elisp manual
  Defining Variables / Customization / Minor Mode Conventions on `defcustom` vs
  `defvar`, `:set`/`:initialize`, load-time vs runtime togglability) nor the
  descriptive half (Doom module flags, Prelude, Crafted, purcell, Prot, karthink,
  minimal-emacs.d) was gathered. **The design document's opt-out section is
  therefore grounded in this repo's own established convention**, which is
  verifiable locally, rather than in external evidence.

- **Performance (GAP 6).** Nothing on `corfu-auto-delay`/`-prefix` tuning guidance,
  the per-keystroke cost of `corfu-auto--post-command`, `eglot-send-changes-idle-time`
  or capf latency reports, `cape-dabbrev-check-other-buffers` cost, `corfu-history`
  + savehist boundedness, or native-comp's effect on completion responsiveness.
  Every performance number in circulation for this stack should be treated as
  folklore until measured.

- **The rest of the built-in layer.** `dabbrev` internals and cache invalidation,
  `abbrev`/`abbrev-suggest`, `skeleton`/`tempo`, and the exact accepted values of
  `completion-auto-help` and `completion-auto-select`.
