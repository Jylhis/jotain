# Emacs sources research — package & workflow survey (2026-08)

A deep-research pass over ~38 Emacs packages, configuration gems, and workflow
articles. For each source the goal was to establish **what it is**, **what
problem it solves**, its **current status/maturity**, and **whether it is worth
adopting** in a modern Emacs 30/31 configuration like Jotain.

## Method

- Decomposed the source list into 6 search angles (AI/agent tooling, Emacs 31
  features and completion changes, note-taking/PKM, git/magit and diff/merge,
  LSP/flymake/tree-sitter/dired, niche packages and Nix integration).
- Fetched 25 sources, extracted 102 candidate claims.
- **Verified the top 25 claims adversarially** (3-vote, refute-by-default,
  needing 2/3 to kill). All 25 survived (0 refuted, 0 unverified).
- Merged semantic duplicates into 14 findings, ranked by confidence.

**Caveat:** Emacs 31 is still a pretest (31.0.90/31.0.91) at research date, so
every Emacs 31 API below describes pre-release behaviour that could still change
before 31.1 final. Package findings that rest on a single primary source (a
project's own README) establish description, not independent maturity.

## Bottom line

The strongest adoption candidates are the plain-text PKM packages (**Denote**,
**obsidian.el**) and **md-ts-mode**. The AI-agent stack (**agent-shell/acp.el**,
**efrit**) is genuinely useful but self-declared experimental, so treat it as a
moving target, not a stable pin. Several listed packages are being superseded by
Emacs 31 itself.

## Findings by theme

### AI / agent tooling

- **agent-shell + acp.el** (xenodium) — Native Emacs interface to LLM coding
  agents over the **Agent Client Protocol (ACP)**, consolidating Claude Code,
  Gemini CLI, Codex, Goose, etc. behind one protocol. `acp.el` is the
  UI-agnostic library; `agent-shell` is the comint/shell-maker front-end.
  **Caveat:** `acp.el`'s README states verbatim it "is in the very early
  stages, isn't yet API stable, and is bound to change." Risky to pin into a
  stable config. Jotain already wires gptel + eca + claude-code-ide in
  `lisp/init-ai.el`; these are alternative front-ends, not obvious replacements.
- **efrit** (Steve Yegge) — Coding agent written entirely in Elisp, running
  natively in Emacs 28.1+, with the Anthropic Claude API as its only external
  dependency. "Pure Executor" design: Claude makes all decisions, efrit only
  executes. Maturity/adoption rests on a single source (its own README).

### Note-taking / PKM — strongest candidates

- **Denote** (Protesilaos, GNU ELPA) — Organizes notes purely via a
  deterministic file-naming scheme (`ID--TITLE__KEYWORDS.EXT`), no database,
  portable plain text usable with standard Unix tools, and deliberately imposes
  no methodology (avoids lock-in). Actively maintained, minimal deps.
  **Best fit for adoption.**
- **obsidian.el** (licht1stein) — Emacs front-end for Obsidian vaults; works
  without the Obsidian app (vaults are plain Markdown directories). Provides
  capture, wikilinks, follow-link, search, tag management, and a backlinks mode.
  Good for anyone with an existing vault.

### Markdown / tree-sitter

- **md-ts-mode** (dnouri) — Tree-sitter Markdown major mode derived from Emacs
  31's `markdown-ts-mode`: syntax highlighting, task-list checkbox glyphs, table
  formatting, per-language code-block fontification, hide-markup toggle.
  Requires Emacs 29.1+ with tree-sitter; tested on 29.4, 30.2, and a 31
  snapshot. **Solid, current adoption candidate.**

### Git / magit

- **Magit conflict resolution** — Magit does *not* ship its own merge-conflict
  engine; it makes built-in **Smerge** and **Ediff** more convenient (`e`
  launches Ediff from the status buffer). Relevant to the ediff/conflict
  workflow articles in the list: the built-ins remain the underlying tools.

### Completion (largely superseded by Emacs 31)

- **MCT** (Protesilaos) — *Enhances* (not replaces) the built-in
  minibuffer/`*Completions*` buffer with live updating plus passlist/blocklist.
  Deliberately narrow niche for users who like the built-in UI rather than
  Vertico/Ivy. Still maintained (v1.1.0), but low-activity.
- **MCT vs Emacs 31** — Emacs 31's new `crm-prompt` option duplicates MCT's
  `mct-completing-read-multiple-indicator` (the MCT indicator is explicitly a
  stopgap "for Emacs versions before 31"). Given Jotain is corfu/vertico/eglot
  based, MCT is not a fit.

### Emacs 31 features (pre-release)

- **Completion overhaul** — 31.1 rewrites the `flex` style (faster, more
  accurate) and adds opt-in **eager `*Completions*` display**
  (`completion-eager-display`/`completion-eager-update`, default `'auto`), plus
  `minibuffer-visible-completions` `'up-down` for independent arrow-key
  navigation. Strengthens the built-in UI.
- **Tree-sitter into core** — `treesit-enabled-modes t` switches modes with a
  tree-sitter variant over to it, and `treesit-auto-install-grammar` offers to
  fetch/build a missing grammar. This folds the third-party **treesit-auto**
  experience into core, so it is droppable on 31. *(Medium confidence: primary
  NEWS not directly verified for this claim.)*
- **Legacy dumper removed** — The 31.0.91 pretest removes the deprecated
  `unexec` dumper, leaving only pdumper. Jotain's Nix build layer is already
  pdumper-based, so this is a no-op here. *(Medium confidence.)*

### Misc / weaker fits

- **lisp-semantic-hl.el** (calsys456) — Minor mode giving semantic highlighting
  for Common Lisp/Elisp from the live Lisp environment rather than
  pattern-matching. Early-stage, low adoption (~20 stars, ~31 commits), core
  features still aspirational. Interesting, not a safe production dependency.
- **nix-emacs** (nix-community) — `nixos-options.el` plus **helm/ivy/company**
  front-ends for NixOS-option search. **Poor fit:** built on the old
  Helm/Ivy/Company stack (not corfu/vertico/capf), and effectively frozen (only
  a license change since 2021). For a Nix config, an nixd/eglot path is the
  modern route.

## Coverage gaps

The budget verified only the top 25 claims, so **many listed sources did not
surface a verified claim** and remain uncovered: keycoach, let-completion,
magit-browse-commit, conflict-buttons, project-store, shannonmax,
pre-commit-elisp, magit-standup, protesilaos/coach, howm (partial), the
flymake-vs-flycheck article, the eww/dired-Ubuntu/debugging posts, the taonaw
config-gem roundups, fancy-fill-paragraph, nix-haskell-mode, emacs-semantic, the
text-expander-for-Android post, and doc.emacsen.de. The flymake/flycheck source
(utcc.utoronto.ca) was marked low-reliability and yielded no claims. A second
focused pass would be needed to cover these.

## Open questions

1. What is efrit's real-world adoption and stability beyond its own README, and
   how does it compare operationally to the ACP-based agent-shell for a
   Claude-centric workflow?
2. Do the Emacs 31 completion and tree-sitter APIs
   (`completion-eager-display`, `treesit-enabled-modes`,
   `treesit-auto-install-grammar`, `crm-prompt`) survive unchanged into 31.1
   final, and what migration is needed from third-party packages (treesit-auto,
   MCT)?
3. For a Nix-managed config specifically, is there a modern corfu/capf-based
   alternative to nix-emacs' frozen Helm/Ivy/Company NixOS-options completion
   (e.g. an nixd/eglot-driven path)?

## Sources

Primary / high-confidence:

- <https://github.com/xenodium/agent-shell>, <https://github.com/xenodium/acp.el>
- <https://github.com/steveyegge/efrit>
- <https://protesilaos.com/emacs/denote>
- <https://github.com/licht1stein/obsidian.el>
- <https://github.com/dnouri/md-ts-mode>
- <https://docs.magit.vc/magit/Resolving-Conflicts.html>
- <https://protesilaos.com/emacs/mct-changelog>
- <https://github.com/calsys456/lisp-semantic-hl.el>
- <https://github.com/nix-community/nix-emacs>

Emacs 31 (blog + secondary, pre-release):

- <https://www.rahuljuliato.com/posts/emacs-31-around-the-corner>
- <https://www.rahuljuliato.com/posts/completions-buffer-is-now-enough>
- <https://github.com/emacs-mirror/emacs/blob/master/etc/NEWS.31>
- <https://www.linuxcompatible.org/story/gnu-emacs-311-rc1-completion-overhaul-window-commands-and-unicode-170>
- <https://www.linuxcompatible.org/story/gnu-emacs-31091-pretest-drops-legacy-dumper-flex-completion-rewrite-and-new-window-layout-commands>
