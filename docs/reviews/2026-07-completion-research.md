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
