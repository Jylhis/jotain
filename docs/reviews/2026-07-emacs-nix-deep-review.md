# Deep-research review — Emacs + Nix layers (2026-07)

A comprehensive adversarial review of the whole Jotain setup, treating it as two
coupled systems: the Nix layer that builds and pins the Emacs environment, and
the Elisp layer that configures runtime behavior — with particular attention to
the seam between them. The stated goal for the config going forward is to
**target Emacs 31**; the relevant findings below are marked accordingly.

## Method

- 87 agents in a five-phase workflow: a Phase-0 survey agent established the
  factual setup; seven parallel reviewers covered Nix purity/reproducibility,
  the Nix↔Elisp seam, startup & deferred loading, Elisp correctness, modernity
  & Emacs-31 readiness, consistency & keybindings, and structure/docs/secrets;
  a dedup stage merged semantic duplicates; **every finding was then
  adversarially verified by an independent agent instructed to refute it**
  (7 claims were refuted and are listed at the end so they don't get
  re-reported); a completeness critic swept for gaps.
- Nix 2.34.8 was installed in the review sandbox and used for firsthand
  verification of the lock-file findings. The full store-path cache-parity
  eval could **not** run here (the sandbox proxy blocks GitHub tarball fetches
  outside the repo scope), so parity remains a CI/local check — see Open
  questions.
- Elisp semantics were checked against the repo's own
  `.claude/skills/{elisp-dev,emacs-internals}` references, not memory.
  Line numbers are against the tree at review time
  (a2fafd3, 2026-07-21).

## The setup, as established (Phase 0)

**Delivery.** The config is delivered by FIVE mechanisms, all rooted in the same checked-in .el files. (1) Source checkout: `just run`/`just debug`/`just tty` are all currently DISABLED stubs (Justfile:26-76 print a notice and exit 0) because Emacs was removed from the devenv shell (devenv.nix:4-15); the live path is `just run-built` (Justfile:220-236), which nix-builds Emacs and launches `./result/bin/emacs --init-directory=<repo>`; in this mode package.el installs anything not on load-path from MELPA into elpa/. (2) Nix full distribution: overlay.nix is now a 12-line wrapper (`import ./nix/mk-overlay.nix { }`, overlay.nix:12 — CLAUDE.md drift, it describes overlay.nix as the real implementation); nix/mk-overlay.nix builds `jotainEmacs` (emacs.nix), `jotainEmacsNoGui`, `jylhisEmacs`, `eca`, `jotainInfo`, and `mkJotainEmacsPackages` (mk-overlay.nix:15-112) which runs the use-package scanner over ../lisp, adds extraEmacsPackages, lndirs the emacsWithPackages wrapper and re-wraps every bin/* with INFOPATH pointing at jotainInfo (mk-overlay.nix:98-112). (3) Flake outputs (flake.nix:115-132): packages.default = jotainEmacsPackages, emacs-lite (curated ~26 grammars via pkgsForLite, flake.nix:69-80 + mk-overlay.nix:39-75), emacs, emacs-jylhis/jylhis-emacs, info, docs, packages-doc; default.nix (default.nix:10-19) is flake-compat glue for `nix-build`, reading the flake-compat rev from flake.lock; emacs.nix and emacs-jylhis.nix are also standalone nix-build entry points that read nixpkgs + emacs-overlay from flake.lock via fetchTarball (emacs.nix:44-79, honoring the x86_64-darwin pin at emacs.nix:52-56). (4) Modules: module.nix (Home Manager) installs the config verbatim into ~/.config/emacs via xdg.configFile (module.nix:338-343: early-init.el, init.el, lisp/, templates/), runs a systemd/launchd daemon, and wraps `emacs` with --init-directory plus a runtime-deps PATH (ripgrep, fd, git, jujutsu, direnv, coreutils, eca, rsync — module.nix:78-91); module-system.nix (NixOS + nix-darwin, shared file) applies the overlay and adds jotainEmacsPackages + eca to environment.systemPackages (module-system.nix:112-118); module-nix-on-droid.nix installs jotainEmacsPackagesNoGui + eca into environment.packages (module-nix-on-droid.nix:101-105). (5) devenv shell (devenv.nix): ships tooling only — NO Emacs (top-of-file note devenv.nix:4-15, contradicting CLAUDE.md's "devenv.nix builds the dev-shell emacs from emacs.nix"): nil, nixfmt, statix, deadnix, meson/ninja, buildifier, sonarlint-ls, rassumfrassum (built from PyPI, devenv.nix:21-36), eca, tagref, dockerfile-language-server, pandoc/texinfo, nerd-fonts + google-fonts (devenv.nix:51-108). CI pushes/pulls the jylhis cachix cache; devenv pulls jylhis + nix-community (devenv.nix:132-138).

**Package source of truth.** Mode (a) source checkout: the use-package forms in lisp/*.el themselves, with `use-package-always-ensure t` set in early-init.el:59-62 (via plain setq before use-package loads) so every block without `:ensure nil` is "install from archive if missing"; archives are MELPA, MELPA-stable, NonGNU appended to the default GNU ELPA in init.el:31-33. 181 use-package forms total, 85 `:ensure nil` opt-outs (counted across lisp/*.el). Nothing pins archive package versions — elpa/ contents are whatever MELPA serves on install day. Mode (b) Nix distribution: the SAME .el files are the declared source, but resolved differently — nix/use-package.nix regex-scans every .el under lisp/ recursively (scanDirectory, use-package.nix:265-272; note this includes devenv.el, not just init-*.el), extracts `(use-package NAME` heads, drops `:ensure nil` and `:disabled t` blocks and honors `:ensure alias` renames (use-package.nix:97-118), maps names to emacs-overlay's epkgs with only `emacs → null` excluded (use-package.nix:297-299), and *silently falls through* (trace-warn only, use-package.nix:317-322) when an epkgs attr is missing — a missing attr therefore degrades to a runtime MELPA install at first startup, the main parity seam between the modes. On top of the scan, mk-overlay.nix:31-79 hard-adds epkgs.claude-code-ide, combobulate, jylhis-emacs-themes, majutsu, nix-ts-mode, tagref (the first five defined as trivialBuild fetchFromGitHub derivations in nix/extra-packages.nix:5-78) plus treesit-grammars (all-grammars by default, curated 26 when curatedGrammars). Deliberately NOT emacs-overlay's fromElisp parser — a documented constraint that use-package forms must never be nested/conditional (use-package.nix:14-18). Note asymmetry: `:ensure nil` means "Nix/built-in provides it" to the scanner, but in mode (a) an `:ensure nil` non-built-in (e.g. claude-code-ide, init-ai.el:77-78) simply doesn't load without the Nix load-path.

**Config source.** Real, hand-written .el files checked into the repo: early-init.el, init.el, 30 files under lisp/ (28 init-*.el modules plus devenv.el, a full standalone library, and init-* helpers). No org-tangle, no Elisp generated from Nix strings, no home-manager `programs.emacs.extraConfig`. The identical files are consumed verbatim by every delivery mode: `--init-directory` in source checkout, symlinked into ~/.config/emacs by module.nix:338-343, and regex-scanned by nix/use-package.nix to derive the Nix package set. Tradeoffs observed here: (+) one source of truth, byte-compilable and lintable as ordinary Elisp (checks.nix elisp-lint/elisp-compile), diffable, works without Nix at all; (+) `;;; @doc` comment convention (use-package.nix:120-212) doubles as the package-reference doc generator, so docs live next to code. (−) Because the Nix layer parses rather than evaluates the Elisp, the config carries an invisible contract: use-package forms must be top-level, never nested in :config, never conditionally :disabled — enforced only by comment (use-package.nix:14-18), not by any check; a violation silently shifts a package from Nix provisioning to runtime MELPA install. (−) No Nix-side templating means all host variation goes through env vars at runtime (OPENROUTER_API_KEY, WAKATIME_API_KEY, JOTAIN_NO_PACKAGE_REFRESH, JOTAIN_INFO_DIR) and `executable-find` probes, which is clean but means the .el files cannot reference store paths — hence the PATH-wrapper machinery in module.nix:78-106.

**Emacs build.** Base package per variant (emacs.nix:205-215): mainline (default) = `pkgs.emacs` from the pinned nixpkgs-unstable — per emacs.nix:17 and the flake description (flake.nix:2, \"Emacs 31+ configuration\", vs CLAUDE.md's \"Emacs 30+\": doc drift, though init.el:5 still declares Package-Requires ((emacs \"30.1\"))) this is now the Emacs 31 release; git = emacs-overlay `emacs-git` (master), unstable = `emacs-unstable` (emacs-31 branch per gitBranch map, emacs.nix:186-190), igc = `emacs-igc` (feature/igc3); macport = nixpkgs `emacs-macport` (jdtsmith fork, still Emacs 30.x, emacs.nix:203-204). All variants go through `basePackage.override` with the arg set filtered by `lib.intersectAttrs (lib.functionArgs basePackage.override)` for 24.05+ portability (emacs.nix:299-301); overrideAttrs only for custom revs / Darwin patches (emacs.nix:363-400) to preserve the cache-parity invariant. Native compilation: on by default (`withNativeCompilation ? canExecute`, emacs.nix:115); at runtime early-init.el:104-121 sets native-comp-speed 2, caps async jobs at 3 (hardcoded to one specific 4-core machine per its own comment, early-init.el:108-110), pre-seeds the Emacs-31-only `native-comp-async-on-battery-power` via defvar (early-init.el:93-117), and redirects eln-cache to var/eln-cache. GUI toolkits: Linux default is X11+cairo (withX, emacs.nix:103,129 — NOT pgtk; pgtk/gtk3/nox are opt-in flags exercised by just build-pgtk/build-gtk3/build-nox, Justfile:184-196); Darwin default is NS (withNS, emacs.nix:101); noGui build for nix-on-droid (mk-overlay.nix:120-123, module-nix-on-droid.nix:34); jylhis Meson fork (emacs-jylhis.nix) defaults noGui=true, withNativeCompilation=false, GTK3 off (emacs-jylhis.nix:33-35) and is explicitly experimental (Justfile:177-181: fork \"still crashes byte-compiling some bundled Emacs packages\"). Tree-sitter: compiled in (withTreeSitter, emacs.nix:132); grammars come from epkgs.treesit-grammars — with-all-grammars (~275) in the default/cached path, curated 26-grammar set behind curatedGrammars/emacs-lite (mk-overlay.nix:39-78); grammar discovery at runtime relies on nixpkgs site-start setting treesit-extra-load-path (early-init.el:123-126), which is propagated to async native-comp workers by init-prog.el:98-105 — no grammar path exists in mode (a) source-checkout on a non-Nix Emacs. Platform matrix matches the modules: x86_64/aarch64-linux + x86_64/aarch64-darwin (flake.nix:45-52), x86_64-darwin specially pinned to nixpkgs-26.05-darwin because unstable dropped it (flake.nix:6-9,56); Darwin cosmetic patches fetched from nix-giant with per-branch pinned hashes, 31/unstable/30 branch selection at emacs.nix:306-330.

**Pinning.** flake.lock direct inputs (staleness vs 2026-07-21): nixpkgs → node `nixpkgs_2` rev 3b32825de172d0bc85664f495edb096b10862524 (nixpkgs-unstable, 2026-07-12, 9d); nixpkgs-x86_64-darwin rev 76fe02659c00bef75e12fbfdd45ca665115e9302 (26.05-darwin, 2026-07-12, 9d); emacs-overlay rev c184b1fa06e3b675df9c88ca529f0d7168f9ef2b (2026-07-13, 8d); treefmt-nix → node `treefmt-nix_2` rev db947814a175b7ca6ded66e21383d938df01c227 (2026-05-31, 51d); flake-compat rev 5edf11c44bc78a0d334f6334cdaf7d60d732daab (2025-12-29, 204d); jylhis-emacs (dev branch) rev c39fcf76cca8e04771f2d3592c38630e25f79e93 (2026-07-04, 17d); nix-on-droid rev 55b6449b4582a4ba3ce712543c973360a026db7d (2025-12-06, 227d). Notable transitive pins: jylhis-emacs carries its OWN nixpkgs = node `nixpkgs` rev ffa10e26ae11d676b2db836259889f1f571cb14f (2026-06-02, 49d) and emacs-overlay_2 (2026-06-05, 46d) — no follows from the root; nix-on-droid drags in very stale deps used only by that output: home-manager 2024-03-03 (870d), nixpkgs-for-bootstrap 2024-07-06 (745d), nixpkgs-docs 2024-01-22 (911d), nmd 921d, nmt 1581d, scss-reset 1773d, flake-utils 615d, systems 1199d. devenv.lock: devenv 407080fe (2026-07-13, 8d), nixpkgs ffa10e26 (2026-06-02, 49d), treefmt-nix db947814 (in sync), emacs-overlay c184b1fa (in sync), nixpkgs-stable 8f0500b9 (26.05, 11d). CRITICAL DESYNC FACT: the flake's real nixpkgs (root input → nixpkgs_2, 3b32825d, 2026-07-12) and devenv's nixpkgs (ffa10e26, 2026-06-02) differ by ~6 weeks, so the CLAUDE.md invariant \"devenv.yaml pins shared inputs to the exact same commits as flake.lock\" is currently FALSE — and `just verify` reports OK anyway because both `verify` (Justfile:341-342) and `update` (Justfile:323-325) read `.nodes.\"nixpkgs\".locked.rev` from flake.lock, which since the jylhis-emacs input joined the graph is jylhis-emacs's nixpkgs node, not the root's (root maps nixpkgs→nixpkgs_2; verified from lock nodes). The same aliasing affects treefmt-nix (node vs treefmt-nix_2) but both happen to hold the same rev today. Practical blast radius is reduced because the dev shell no longer builds Emacs, but any devenv-shell tool built from ffa10e26 diverges from flake-built artifacts. Unpinned-at-source (branch-tracking, locked only by flake.lock): nixpkgs-unstable, emacs-overlay HEAD, jylhis/emacs dev, nix-on-droid master, flake-compat, treefmt-nix. Other pins: emacs-jylhis.nix fallback rev eaf289b4+hash (emacs-jylhis.nix:27-28); eca 0.136.2 with per-platform sha256 (nix/eca-server.nix:11-35); five GitHub Elisp sources hash-pinned in nix/extra-packages.nix; Darwin patches hash-pinned but fetched from the mutable `main` branch URL (emacs.nix:336-338). Mode-(a) elpa/ MELPA installs are entirely unpinned (no version constraints anywhere in Elisp).

### Every runtime network fetch (reproducibility holes, enumerated)

- init.el:31-33 — registers MELPA (https://melpa.org/packages/), MELPA-stable, NonGNU as package archives; every subsequent package.el operation in either mode can hit these hosts.
- init.el:101-109 — background `(package-refresh-contents t)` fired by an emacs-startup-hook + 2s idle timer whenever any configured archive's cached archive-contents is missing or >7 days old (jotain--package-archives-stale-p, init.el:78-95); async, error-demoted, opt-out only via JOTAIN_NO_PACKAGE_REFRESH env var (init.el:68-70). Fires in BOTH modes, including the Nix distribution (fresh ~/.config/emacs has no archive cache, so first daemon start always downloads archive indexes unless the env var is set).
- early-init.el:59-62 — `use-package-always-ensure t`: the generic mechanism by which ANY use-package block whose package is neither built-in, on the Nix load-path, nor already in elpa/ triggers `package-install` → MELPA download during init. In mode (a) this is the entire bootstrap (~96 ensured packages on first `just run-built` startup); in mode (b) it is the fallback for scanner misses.
- lisp/init-terminal.el:39 — `(ghostel-module-auto-install 'download)`: downloads a prebuilt libghostty-vt native dynamic module into the writable elpa/ package dir on first `M-x ghostel` when the module is absent (comment at init-terminal.el:29-38 confirms the download path is the source-checkout/MELPA case; the Nix package ships the module). ghostel itself has no :ensure nil and is not in extra-packages.nix, so it must resolve via epkgs in mode (b) or MELPA in mode (a).
- lisp/init-ai.el:104-106 — eca-emacs downloads the eca server binary on first `M-x eca` when no `eca` is on PATH (stated by nix/eca-server.nix:3-6: "eca-emacs would otherwise download the server on first M-x eca"); mitigated by Nix-provided eca on PATH in devenv.nix:80-82, module.nix:87, module-system.nix:117, module-nix-on-droid.nix:104 — but NOT mitigated in a bare source checkout outside the dev shell (inferred from those files; the download path itself lives in the eca-emacs package, not this repo).
- lisp/init-ai.el:124-141 — gptel default backend POSTs to https://openrouter.ai/api/v1/chat/completions (user-initiated C-c RET); key from OPENROUTER_API_KEY or auth-source.
- lisp/init-ai.el:144-150 — gptel Anthropic backend → api.anthropic.com (user-initiated).
- lisp/init-ai.el:153-159 — gptel Gemini backend → generativelanguage.googleapis.com (user-initiated).
- lisp/init-ai.el:162-165 — gptel Ollama backend → localhost:11434 (local only).
- lisp/init-ai.el:77-97 — claude-code-ide drives the external Claude Code CLI (Anthropic API network traffic via that CLI; user-initiated C-c C-').
- lisp/init-vc.el:196-202 — forge: GitHub/GitLab REST/GraphQL API calls on user action (forge-pull etc.), token from ~/.authinfo via auth-source (comment init-vc.el:196).
- lisp/init-tracking.el:29-35 — wakatime-mode sends heartbeats through wakatime-cli to the WakaTime/Wakapi API continuously while editing, gated on `(executable-find "wakatime-cli")` and WAKATIME_API_KEY at init time.
- lisp/init-tracking.el:41-43 — activity-watch-mode POSTs to a local ActivityWatch server, only when manually enabled (:commands, no hook).
- lisp/init-systems.el:18-23 — auth-source-1password shells out to the `op` CLI, which contacts the 1Password service on every credential lookup (feeds gptel/forge keys).
- lisp/init-devenv.el / lisp/devenv.el:1438-1448 — devenv MCP server registration with mcp.el; mcp servers are user-started processes that may open network connections (user action).
- bench/early-init.el:31-38 — advises package-refresh-contents to TIME it (evidence the network bootstrap is an acknowledged startup cost; not itself a fetch).
- Runtime local BUILDS (not fetches) in mode (a), inferred: jinx (init-writing.el:57-60) compiles its jinx-mod C module against enchant on first load after a MELPA install, and pdf-tools (init-writing.el:84-86) prompts to build epdfinfo on first PDF open — both no-ops in mode (b) where nixpkgs melpaBuild pre-builds them; failure mode in (a) is a missing-toolchain error, not a network fetch.
- NOT present (verified by grep over lisp/): no treesit-install-language-grammar (ts remaps are treesit-ready-p-guarded, init-prog.el:84-91, and grammars come from Nixpkgs site-start per early-init.el:123-126); no nerd-icons-install-fonts (init-ui.el:248-256 only selects among already-installed fonts; fonts ship via devenv.nix:101-103 / module fonts); no straight/elpaca/quelpa/package-vc-install; no url-retrieve calls in repo code.
- Nix BUILD-time network (fixed-output, hash-pinned — for completeness): nix/eca-server.nix:45-48 fetchurl of the eca 0.136.2 GitHub release binary; nix/extra-packages.nix:10-14,22-27,37-41,52-56,72-76 five fetchFromGitHub sources; emacs.nix:332-340 darwinPatch fetchpatch from the MUTABLE `main` branch of nix-giant/nix-darwin-emacs (content-hash-pinned but branch-addressed, so upstream rewrites break the fetch); emacs.nix:44-79 + default.nix:12-17 + emacs-jylhis.nix:8-24 fetchTarball of flake.lock-pinned nixpkgs/emacs-overlay/flake-compat.

## Overall assessment

This is an unusually disciplined two-mode design — the same `.el` files drive
both a MELPA-bootstrapped checkout and a fully Nix-provisioned distribution,
with a regex use-package scanner as the bridge — and most of the hard
invariants (cache parity, lock sync, warning-clean compilation) are the *right*
invariants. The problem is that several of the mechanisms that are supposed to
*enforce* those invariants are currently lying: the lock-sync tooling reads the
wrong lock node (so dev shell and flake builds silently diverged by 40 days of
nixpkgs), the documented parity-verification snippet now yields guaranteed
false results, and CLAUDE.md/AGENTS.md/docs describe a dev workflow (dev-shell
Emacs, `just run`, `just compile`, `just bench`) that has been stubbed out.
Meanwhile the Elisp layer has a handful of real runtime bugs — most visibly
diff-hl never enabling globally, idle auto-save mutating text at point, and a
prog-mode hook that hard-errors the entire MELPA-fallback mode — plus a
systematic pattern of primary entry points bound in major-mode-reserved C-c
key space. Nothing here is architecturally rotten; it is drift, and the fixes
are mostly small and local.

## Findings

Severity: **Critical** = breaks reproducibility/correctness/startup ·
**High** = real bug or real parity/perf cost · **Medium** = works but
wrong/wasteful/fragile · **Nitpick** = taste.

| # | Sev | Location | Finding |
|---|-----|----------|---------|
| 1 | Critical | `Justfile:325` | just update/verify read the wrong flake.lock node — devenv pinned to jylhis-emacs's stale transitive nixpkgs, verify reports false OK, and update re-creates the desync |
| 2 | High | `CLAUDE.md:100` | Documented cache-parity verification snippet reads the wrong lock node — the prescribed check for the repo's 'most important invariant' now yields guaranteed false results |
| 3 | High | `CLAUDE.md:7` | CLAUDE.md, AGENTS.md, and four docs pages document a dev workflow that no longer exists (no Emacs in the devenv shell; 10 just recipes are disabled stubs) |
| 4 | High | `Justfile:227` | `just run-built` on aarch64-linux launches a bare noGui Emacs against the full config — Nix-only :ensure nil packages break, ~96 packages MELPA-bootstrap, zero tree-sitter grammars |
| 5 | High | `early-init.el:42` | package-quickstart caches absolute /nix/store paths in mutable var/, going stale across Nix redeploys |
| 6 | High | `flake.nix:26` | jylhis-emacs is a full flake input though only its source tree is used — drags duplicate nixpkgs/emacs-overlay_2/flake-utils/treefmt-nix into the lock and caused the node aliasing |
| 7 | High | `lisp/init-ai.el:120` | Primary AI/help/mc entry points bound in major-mode-reserved C-c space are silently shadowed in the buffers they matter most |
| 8 | High | `lisp/init-editing.el:42` | Global before-save whitespace-cleanup defeats super-save's except-current-line and mutates text at point during idle auto-saves |
| 9 | High | `lisp/init-prog.el:503` | tagref prog-mode hook hard-errors in the MELPA-fallback mode, aborting mode setup for every project file |
| 10 | High | `lisp/init-vc.el:218` | diff-hl `:after magit` + `:hook (after-init . global-diff-hl-mode)` — global-diff-hl-mode never runs; pre-refresh hook is wired to `ignore` |
| 11 | High | `lisp/init-vc.el:533` | `(eval-when-compile (require 'doom-modeline))` loads doom-modeline eagerly during init in interpreted runs |
| 12 | Medium | `CLAUDE.md:84` | CLAUDE.md and modules.mdx cite a corfu/completion-preview pairing that no longer exists in the config |
| 13 | Medium | `CLAUDE.md:17` | CLAUDE.md's CI description no longer matches ci.yml, and deploy.yml (which carries the heavy checks and cache pushes) is undocumented |
| 14 | Medium | `CLAUDE.md:136` | Accumulated CLAUDE.md architecture drift: overlay.nix is a wrapper, extra-packages has 5 packages not 2, `just fmt`/`just test` misdescribed, flake outputs and init.el behavior stale, three inconsistent Emacs-version claims |
| 15 | Medium | `devenv.nix:103` | Dev shell ships google-fonts (plus two nerd-font sets) for an Emacs that is no longer in the shell |
| 16 | Medium | `docs/architecture/nix-build.mdx:75` | nix-build.mdx describes a default.nix and grammar-wiring mechanism that no longer exist |
| 17 | Medium | `early-init.el:113` | native-comp-async-jobs-number hardcoded to 3 for one specific machine; use num-processors |
| 18 | Medium | `emacs.nix:107` | Explicit withXwidgets pass diverges from emacs-overlay's defaults on Darwin, silently busting cache parity for git/unstable/igc there |
| 19 | Medium | `emacs.nix:306` | patchBranch picks the '31' Darwin patch branch for the mainline variant, which is actually Emacs 30.2 |
| 20 | Medium | `flake.nix:97` | Flake modules hand consumers only the jotain overlay, not emacs-overlay — module-installed distributions resolve epkgs from the consumer's nixpkgs and never match CI-cached artifacts |
| 21 | Medium | `lisp/devenv.el:419` | Native env loader runs a synchronous `devenv hook-should-activate` subprocess on the find-file path, re-paying it every 30 seconds per project |
| 22 | Medium | `lisp/init-completion.el:255` | zoxide-add runs on every find-file but the zoxide binary is wired into no Nix layer |
| 23 | Medium | `lisp/init-completion.el:117` | consult `:demand t` forces the largest minibuffer package to load at startup for no functional gain |
| 24 | Medium | `lisp/init-core.el:38` | GC threshold restored at load of the FIRST init module, defeating the early-init bump for ~95% of init |
| 25 | Medium | `lisp/init-core.el:279` | exec-path-from-shell :if guard omits pgtk — PATH inheritance never runs on the Wayland/pgtk variant |
| 26 | Medium | `lisp/init-core.el:45` | Minibuffer GC hooks are unremovable lambdas that hardcode the threshold and misbehave with recursive minibuffers |
| 27 | Medium | `lisp/init-docs.el:36` | Pre-seeding Info-directory-list suppresses Info's default initialization — all other manuals vanish |
| 28 | Medium | `lisp/init-editing.el:102` | Entire `use-package newcomment` block (including its @doc comment) is duplicated verbatim |
| 29 | Medium | `lisp/init-prog.el:425` | jotain-sonarlint let-binds eglot-server-programs, which can leave it void if eglot first loads inside the call |
| 30 | Medium | `lisp/init-prog.el:367` | fset of jsonrpc--log-event to ignore silences logging for every jsonrpc client (dape, mcp) — redundant with the eglot-events-buffer-config already set |
| 31 | Medium | `lisp/init-prog.el:290` | eglot-confirm-server-initiated-edits is an obsolete alias (Eglot 1.16) — rename to eglot-confirm-server-edits |
| 32 | Medium | `lisp/init-prog.el:313` | eglot-documentation-renderer adoption is permanently inert: markdown-ts-view-mode is never fboundp when the guard runs |
| 33 | Medium | `lisp/init-snippets.el:66` | Merged tempel+eglot capf survives eglot shutdown — completion then signals "No current JSON-RPC connection" in every previously-managed buffer |
| 34 | Medium | `lisp/init-systems.el:20` | auth-source-1password and sops are `:demand t` — two eager loads for lazily-needed features |
| 35 | Medium | `lisp/init-ui.el:248` | nerd-icons loads eagerly at startup (use-package with only :config), dragging nerd-icons-completion with it |
| 36 | Medium | `lisp/init-ui.el:250` | nerd-icons-font-family selection is gated on display-graphic-p at load time, so it never applies under the daemon |
| 37 | Medium | `lisp/init-vc.el:46` | Binding C-x C-g removes the C-g escape from the C-x prefix |
| 38 | Medium | `lisp/init-vc.el:550` | Re-declaring doom-modeline's 'main modeline as a frozen copy will silently drift from upstream |
| 39 | Medium | `lisp/init-writing.el:58` | global-jinx-mode is unconditional but no Nix layer provides spell-check dictionaries for enchant |
| 40 | Medium | `lisp/init-writing.el:58` | global-jinx-mode on emacs-startup-hook has no failure guard; a jinx module build failure aborts the remaining startup hooks |
| 41 | Medium | `module-system.nix:113` | NixOS/nix-darwin and nix-on-droid modules ship Emacs without the runtime binaries the config invokes unconditionally |
| 42 | Medium | `module.nix:86` | Darwin: `coreutils` in the wrapper does not provide `gls`, so init-navigation's GNU-ls detection never fires (and GNU ls shadows BSD ls as plain `ls`) |
| 43 | Medium | `module.nix:341` | No delivery mode byte-compiles or AOT-native-compiles the config — all ~30 modules load as interpreted source every daemon start |
| 44 | Medium | `module.nix:96` | HM module ships the emoji fallback font but none of the Nerd Fonts the icon stack requires |
| 45 | Medium | `nix/checks.nix:328` | elisp-test hard-codes test/devenv-test.el while AGENTS.md tells contributors to add test-*.el files — new test files are silently never run |
| 46 | Medium | `nix/mk-overlay.nix:115` | Repo claims to target Emacs 31 but the default build ships Emacs 30.2, leaving every guarded 31 adoption in lisp/ dead |
| 47 | Medium | `nix/mk-overlay.nix:125` | jylhisEmacs builds a different source per entry point: flake uses the dev-branch input, standalone overlay.nix falls back to a hardcoded 6-month-old rev |
| 48 | Medium | `nix/use-package.nix:99` | Scanner parse gaps are latent landmines: bare :disabled unrecognized, alias regex drops '+', comments/strings scanned as real forms — the top-level-forms contract is enforced only by a comment |
| 49 | Medium | `plan.md:63` | Stale planning debris in repo root: plan.md lists shipped work as open, TODO.md lists adopted packages, questions.md is a one-note scratch file |
| 50 | Nitpick | `Justfile:11` | Eager `system` backtick assignment shells out to `nix eval --impure` on every just invocation |
| 51 | Nitpick | `Justfile:234` | `just run-built` — the only live launch path — hardwires --debug-init and global debug-on-error into every ordinary session |
| 52 | Nitpick | `bench/early-init.el:29` | Bench harness only times `require` — autoload-driven loads and the network refresh are invisible |
| 53 | Nitpick | `config/eca/config.json:6` | OpenRouter model list is hand-duplicated verbatim between config/eca/config.json and init-ai.el with no sync check |
| 54 | Nitpick | `emacs.nix:336` | Darwin patches are fetched from the mutable `main` branch of nix-giant/nix-darwin-emacs |
| 55 | Nitpick | `emacs.nix:66` | Standalone emacs.nix sets config.allowUnfree = true while the flake's pkgsFor does not — asymmetric eval config across entry points |
| 56 | Nitpick | `init.el:32` | melpa-stable is registered but can never win, and only adds a staleness probe |
| 57 | Nitpick | `lisp/init-ai.el:170` | `(use-package mcp :defer t :after gptel)` is dead config — the :after chain never loads anything |
| 58 | Nitpick | `lisp/init-core.el:40` | Idle GC timer defeats the minibuffer GC pause, and the 16 MiB literal is duplicated |
| 59 | Nitpick | `lisp/init-core.el:349` | Dead capability guards for features at or below the Emacs 30 floor |
| 60 | Nitpick | `lisp/init-core.el:127` | diminish is invisible under doom-modeline and superseded on Emacs 31 by mode-line-collapse-minor-modes |
| 61 | Nitpick | `lisp/init-devenv.el:36` | init-devenv @doc still claims devenv-reload works through envrc, contradicting the config's own 'envrc intentionally not enabled' design |
| 62 | Nitpick | `lisp/init-editing.el:146` | Sweep of remaining avoidable eager loads: super-save, pulsar, kkp, corfu/cape/marginalia :demand, keyfreq |
| 63 | Nitpick | `lisp/init-keys.el:143` | jotain-window-resize-repeat-map re-implements the built-in resize-window-repeat-map; comment claims a gap that doesn't exist |
| 64 | Nitpick | `lisp/init-keys.el:8` | init-keys commentary sanctions `:general` though general.el is not in the config; which-key label table omits C-c m |
| 65 | Nitpick | `lisp/init-lang-devops.el:28` | setq on the defcustom dockerfile-mode-command violates the repo's setopt rule |
| 66 | Nitpick | `lisp/init-lang-go.el:64` | go-ts-mode-indent-offset is renamed to go-ts-indent-offset in Emacs 31 |
| 67 | Nitpick | `lisp/init-prog.el:547` | apheleia loads eagerly at startup; format-on-save only matters at first save |
| 68 | Nitpick | `lisp/init-prog.el:61` | C/C++ missing from the tree-sitter remap table while the rest of the config assumes c-ts modes; the gap-detector spams every C file |
| 69 | Nitpick | `lisp/init-project.el:81` | with-eval-after-load 'project inside projection's :config is a redundant self-require |
| 70 | Nitpick | `lisp/init-vc.el:119` | Global C-x C-j binding shadows the Emacs 28+ default dired-jump |
| 71 | Nitpick | `lisp/init-vc.el:571` | smerge :bind block re-states smerge-mode's own defaults verbatim |
| 72 | Nitpick | `lisp/init-writing.el:78` | Owner-machine path defaults (~/Documents/notes, ~/Developer, ~/Projects, ~/Documents) baked into a config shipped to all module consumers |
| 73 | Nitpick | `module.nix:85` | HM wrapper ships direnv for an envrc integration the config removed, and omits devenv, which the config's env loader actually shells out to |
| 74 | Nitpick | `nix/mk-overlay.nix:134` | sonarlintLs overlay attr is dead code, and the INFOPATH wrapper comment says 'prepend' while the code appends |


## Critical

### 1. just update/verify read the wrong flake.lock node — devenv pinned to jylhis-emacs's stale transitive nixpkgs, verify reports false OK, and update re-creates the desync

**`Justfile:325`** · layer: nix

**What:** Both `just update` (Justfile:323-325) and `just verify` (Justfile:341-342) resolve revs with `jq -r '.nodes."$input".locked.rev' flake.lock`, assuming node name == input name. Since the `jylhis-emacs` input joined the graph, the root's nixpkgs maps to node `nixpkgs_2` (`.nodes.root.inputs.nixpkgs` = "nixpkgs_2", rev 3b32825d, 2026-07-12), while the bare `nixpkgs` node is jylhis-emacs's transitive pin (ffa10e26, 2026-06-02). `update` therefore wrote the wrong rev into devenv.yaml (which currently pins ffa10e26), and `verify` compares that same wrong node against devenv.lock (also ffa10e26), printing OK while the two environments are ~6 weeks apart. Same aliasing hits treefmt-nix (root -> treefmt-nix_2); latent only because both nodes hold db947814 today. Worse: if jylhis-emacs re-pins its nixpkgs, devenv.yaml follows a third party's transitive pin.

**Why it matters:** CLAUDE.md:142's lock-sync invariant ('devenv.yaml pins shared inputs to the exact same commits as flake.lock') is currently false and the enforcement tool lies about it. Every devenv-shell tool (nixfmt, statix, deadnix, treefmt, pandoc, sonarlint-ls, ...) builds from nixpkgs ffa10e26 while flake checks/CI build from 3b32825d — local-passes/CI-fails formatting flips, zero shared store paths (cache misses), and every future `just update` is a ratchet in the wrong direction. [Verified firsthand during this review: nix-instantiate of emacs.nix fetched 3b32825de1 (the root's real nixpkgs, node nixpkgs_2) while devenv.lock pins ffa10e26ae — which is jylhis-emacs's non-follows transitive nixpkgs node, 40 days older.]

**Fix:**

Resolve nodes through the root input map in BOTH recipes (emacs.nix:52-57 already does this correctly — copy that discipline):
```just
    node=$(jq -r ".nodes.root.inputs.\"$input\"" flake.lock)
    owner=$(jq -r ".nodes.\"$node\".locked.owner" flake.lock)
    repo=$(jq -r ".nodes.\"$node\".locked.repo" flake.lock)
    rev=$(jq -r ".nodes.\"$node\".locked.rev" flake.lock)
```
(verify: resolve via `.nodes.root.inputs` in both flake.lock and devenv.lock.) Then remediate the drift: set devenv.yaml nixpkgs to github:NixOS/nixpkgs/3b32825de172d0bc85664f495edb096b10862524 and run `devenv update`; confirm `just verify` fails before the fix and passes after. Optionally assert `.nodes.root.inputs."$input"` exists so future graph-shape changes fail loudly.

[verifier correction] What/why/fix stand as written — confirmed by direct inspection: Justfile:323-325 and 341-342 resolve `.nodes."$input".locked.rev` assuming node name == input name, but since jylhis-emacs joined the input graph, root's nixpkgs is node `nixpkgs_2` (3b32825d) while the bare `nixpkgs` node (ffa10e26) is jylhis-emacs's transitive pin. `just update` therefore syncs devenv.yaml to a third party's transitive pin, and `just verify` compares that same wrong node against devenv.lock and prints OK while the flake and devenv environments diverge by ~6 weeks; treefmt-nix has the same aliasing, latent only because both nodes currently hold db947814. Fix as proposed: resolve through `.nodes.root.inputs."$input"` in both recipes (mirroring emacs.nix:52-57, and devenv.lock also has a root.inputs map so the same resolution works on the devenv side), then re-pin devenv.yaml nixpkgs to 3b32825de172d0bc85664f495edb096b10862524 and run `devenv update`. Severity lowered from critical to high: no build breaks, shipped artifact, or security exposure — the impact is silent dev-shell/CI environment skew, binary-cache misses, a false-OK verification tool, and an update recipe that recreates the desync.

<details><summary>Evidence</summary>

Justfile:323-325 and 341-342 use `.nodes.\"$input\".locked.*`. jq on flake.lock: root.inputs = {nixpkgs: nixpkgs_2, treefmt-nix: treefmt-nix_2}; nodes.nixpkgs.rev = ffa10e26 (owned by jylhis-emacs per its inputs map), nodes.nixpkgs_2.rev = 3b32825d. devenv.yaml pins ffa10e26; devenv.lock nodes.nixpkgs.rev = ffa10e26. CLAUDE.md:142 states the invariant; emacs.nix:52-57 reads lock.nodes.root.inputs.nixpkgs (correct).

</details>


## High

### 2. Documented cache-parity verification snippet reads the wrong lock node — the prescribed check for the repo's 'most important invariant' now yields guaranteed false results

**`CLAUDE.md:100`** · layer: docs

**What:** The nix-instantiate snippet at CLAUDE.md:96-108 (line 100: `n = lock.nodes.nixpkgs.locked;`) and its copy at docs/architecture/nix-build.mdx:37-56 predate the jylhis-emacs input. Node `nixpkgs` is now jylhis-emacs's transitive nixpkgs (ffa10e26), not the root's (nixpkgs_2 = 3b32825d). But `import ./emacs.nix {}` correctly resolves `lock.nodes.root.inputs.nixpkgs` (emacs.nix:52-57), so the documented check compares emacs.nix-on-nixpkgs-A against pkgs.emacs-from-nixpkgs-B and reports a spurious parity failure (mainline = false, likely the overlay variants too since the overlay is applied to the wrong nixpkgs) even when parity holds. emacs.nix's in-file copy of the snippet (lines 238-246) was already fixed to root-mapped resolution.

**Why it matters:** CLAUDE.md calls cache parity 'the most important invariant in the repo' and this snippet is the only prescribed verification after editing emacs.nix. Anyone running it gets a false negative and may chase a phantom regression — or 'fix' correct emacs.nix defaults against the wrong nixpkgs and genuinely break parity against the real pin.

**Fix:**

Docs-only, in both CLAUDE.md:100-101 and docs/architecture/nix-build.mdx:41-42, matching emacs.nix:239-241:
```nix
let lock = builtins.fromJSON (builtins.readFile ./flake.lock);
    n  = lock.nodes.${lock.nodes.root.inputs.nixpkgs}.locked;
    ov = lock.nodes.${lock.nodes.root.inputs.emacs-overlay}.locked;
```
(emacs-overlay currently maps to the bare node so it happens to be right; root-mapping both is future-proof.)

<details><summary>Evidence</summary>

CLAUDE.md:100 `n = lock.nodes.nixpkgs.locked;`; docs/architecture/nix-build.mdx:41 identical. emacs.nix:52-57/239-241 use lock.nodes.root.inputs.nixpkgs. jq: nodes.nixpkgs.rev (ffa10e26) != nodes.nixpkgs_2.rev (3b32825d). False-negative outcome inferred from the differing revs (not executed — needs network).

</details>

### 3. CLAUDE.md, AGENTS.md, and four docs pages document a dev workflow that no longer exists (no Emacs in the devenv shell; 10 just recipes are disabled stubs)

**`CLAUDE.md:7`** · layer: docs

**What:** CLAUDE.md:7 claims 'devenv.nix builds the dev-shell emacs from emacs.nix' — devenv.nix:4-15 explicitly removed Emacs from the shell. CLAUDE.md:23-29,43,45 document `just check-elisp`, `just compile`, `just run/debug/tty`, `just bench`, and `emacs-smoke`/`emacs-run` as working; in the Justfile, run/debug/tty/daemon/client/client-tty/quick (26-76), check-elisp (89-94), compile (97-106), compile-native (112-121), bench/bench-open (134-146) are all [DISABLED] echo-and-exit-0 stubs, and the devenv scripts are commented out (devenv.nix:152-173). AGENTS.md:11-15 repeats the dead commands; docs/installation.mdx:131-137, docs/quickstart.mdx:26-52, docs/usage/launching.mdx:17-22,134-137, docs/architecture/overview.mdx:58 repeat them for end users; Justfile:278's `just info` recipe echoes 'Open with just run'. (Also line 7's 'Emacs 30+' conflicts with flake.nix:2 'Emacs 31+'.)

**Why it matters:** CLAUDE.md is loaded as authoritative, overriding instructions for every agent session; agents run `just run`/`just compile` expecting a launch/byte-compile and get exit 0 — scripted callers see success. A human following quickstart hits the same dead end on their first command. The one live launch path (`just run-built`) is buried, and the published Info manual/Pages site ships wrong instructions.

**Fix:**

Docs-only: (1) rewrite CLAUDE.md 'What this is' + 'Common commands' + 'Dev shell's Emacs' around current reality (dev shell = tooling only; launch = `just run-built`; check/compile coverage = flake checks elisp-lint/elisp-compile/elisp-test); delete or mark disabled the emacs-smoke/bench paragraphs. (2) Same in AGENTS.md:11-15,28. (3) Update installation.mdx, quickstart.mdx, launching.mdx, overview.mdx:58,60, modules.mdx:156. (4) Justfile:278: point at `just run-built`. If the removal is temporary, add one canonical 'current state' note all docs point at.

<details><summary>Evidence</summary>

devenv.nix:4-15 top note; Justfile:26-146 stubs echoing '... is disabled — emacs is not in the devenv shell' with exit 0; devenv.nix:152-173 commented scripts; CLAUDE.md:7,23-29,43,45; AGENTS.md and docs grep hits at cited lines; Justfile:278.

</details>

### 4. `just run-built` on aarch64-linux launches a bare noGui Emacs against the full config — Nix-only :ensure nil packages break, ~96 packages MELPA-bootstrap, zero tree-sitter grammars

**`Justfile:227`** · layer: nix

**What:** run-built (Justfile:220-236) maps `Linux-aarch64` to `build-android` (Justfile:214-216), which is `nix-build --arg noGui true --argstr system aarch64-linux emacs.nix` — the bare binary with no epkgs, no grammars, no site-lisp — then launches `./result/bin/emacs --init-directory=<repo>`. Every other platform maps to `build` = full jotainEmacsPackages distribution. With `use-package-always-ensure t` (early-init.el:59-62), first startup on aarch64 fetches ~96 packages from MELPA; jinx/pdf-tools attempt local C builds; treesit-extra-load-path is never set (no emacsWithPackages site-start) so every ts remap silently degrades. Nix-only `:ensure nil` packages simply don't exist: `nix-ts-mode` (init-lang-nix.el:17-19) leaves .nix files with a broken :mode autoload; `tagref`'s prog-mode hook (init-prog.el:503-512) errors per buffer; majutsu/claude-code-ide/combobulate keybindings dangle.

**Why it matters:** The one supported launch recipe (all `just run/debug/tty` are disabled stubs, Justfile:26-76) has per-platform semantics: reproducible Nix-provisioned session on x86_64/Darwin, unpinned network bootstrap on aarch64-linux — precisely the platform (nix-on-droid/Termux/Asahi/ARM servers) where a mobile-network MELPA bootstrap and grammar-less ts-modes hurt most. It also contradicts the module layer, which ships jotainEmacsPackagesNoGui (the full nox distribution) on the same platform.

**Fix:**

Expose the existing full terminal distribution as a flake output and use it:
```nix
# flake.nix packages:
emacs-nox = (pkgsFor system).jotainEmacsPackagesNoGui;
```
```just
build-nox-full:
    nix build .#emacs-nox -o result
# run-built case:
        Linux-aarch64) target=build-nox-full ;;
```
(keep the bare emacs.nix recipe under another name for cache-parity testing). No Elisp change.

<details><summary>Evidence</summary>

Justfile:214-216 (build-android = bare noGui emacs.nix), 224-229 (case mapping), 234-236 (launch); Justfile:26-76 (direct run recipes disabled); mk-overlay.nix:31-79 (extraEmacsPackages: nix-ts-mode/tagref/majutsu/claude-code-ide/combobulate only in full distribution), mk-overlay.nix:162-165 (jotainEmacsPackagesNoGui exists, no flake/Justfile consumer outside droid module); init-prog.el:500-512, init-lang-nix.el:17-19, early-init.el:59-62 and 123-126 read directly.

</details>

### 5. package-quickstart caches absolute /nix/store paths in mutable var/, going stale across Nix redeploys

**`early-init.el:42`** · layer: seam

**What:** early-init.el:41-44 pins `package-quickstart-file` to var/package-quickstart.el and sets `package-quickstart t`. In the Nix distribution, packages live under /nix/store via the emacsWithPackages wrapper; the first runtime `package-install` triggers `package--quickstart-maybe-refresh`, writing a quickstart file with absolute store paths of every activated package. var/ persists across deployments (repo checkout or ~/.config/emacs/var under the HM module) and nothing invalidates the file when the Nix closure changes.

**Why it matters:** After the next `nix build`/`home-manager switch` bumps package store paths, `package-activate-all` short-circuits into the stale quickstart: Emacs silently loads OLD package versions from old store paths until those are GC'd, after which activation errors out; new Nix-provided packages absent from the stale file are never activated. Exactly the reproducibility seam the two-mode design is supposed to avoid.

**Fix:**

Elisp (early-init.el, before the quickstart block) — invalidate when the Nix load-path generation changes:
```elisp
(let* ((qs (expand-file-name "var/package-quickstart.el" user-emacs-directory))
       (stamp (expand-file-name "var/package-quickstart.gen" user-emacs-directory))
       (gen (secure-hash 'sha256 (or (getenv "EMACSLOADPATH") "")))
       (old (ignore-errors (with-temp-buffer
                             (insert-file-contents stamp) (buffer-string)))))
  (when (and (file-exists-p qs) (not (equal gen old)))
    (ignore-errors (delete-file qs))
    (ignore-errors (delete-file (concat qs "c"))))
  (unless (equal gen old)
    (ignore-errors
      (make-directory (file-name-directory stamp) t)
      (write-region gen nil stamp nil 'silent))))
```
No Nix change needed (EMACSLOADPATH already encodes the deps derivation store path).

<details><summary>Evidence</summary>

early-init.el:36-44 read; init.el:62-67 comment confirms quickstart short-circuits activation; module.nix:338-343 installs only .el sources, nothing manages var/. Inference (flagged): exact quickstart content (absolute dirs incl. /nix/store system packages) is from package.el semantics, not observed in a running build.

</details>

### 6. jylhis-emacs is a full flake input though only its source tree is used — drags duplicate nixpkgs/emacs-overlay_2/flake-utils/treefmt-nix into the lock and caused the node aliasing

**`flake.nix:26`** · layer: nix

**What:** flake.nix:26 declares `jylhis-emacs.url = "github:jylhis/emacs/dev"` with neither `flake = false` nor `follows`. It is only consumed as a source path: flake.nix:76,90 pass `jylhisEmacsSrc = inputs."jylhis-emacs"` into mk-overlay.nix, and emacs-jylhis.nix:47 uses only `src.rev or rev` and the path — its flake outputs are never touched. As a flake input it locks its own nixpkgs (ffa10e26, 49d stale), emacs-overlay_2 (1294ccb9, diverging from root's c184b1fa), flake-utils, and a shadow treefmt-nix — exactly what renamed the root's inputs to nixpkgs_2/treefmt-nix_2 and broke the Justfile lock-sync tooling (critical finding above).

**Why it matters:** Lock-graph pollution with zero benefit: four dead transitive pins `nix flake update` keeps refreshing, a second emacs-overlay revision inviting confusion, and the input-name/node-name aliasing that silently broke `just update`/`just verify`. `flake = false` shrinks the lock and makes naive node lookups unambiguous again.

**Fix:**

```nix
jylhis-emacs = {
  url = "github:jylhis/emacs/dev";
  flake = false;
};
```
Non-flake inputs still expose `.rev` and coerce to a source path, so `jylhisEmacsSrc` and emacs-jylhis.nix's `src.rev or rev` keep working. Re-lock; node `nixpkgs` becomes the root's nixpkgs again.

<details><summary>Evidence</summary>

flake.nix:26 (no flake=false/follows); flake.nix:76,90; emacs-jylhis.nix:47 `sourceRev = src.rev or rev;`. jq: nodes.jylhis-emacs.inputs = {emacs-overlay: emacs-overlay_2, flake-utils, nixpkgs, treefmt-nix}; nodes.emacs-overlay_2.rev = 1294ccb9 vs root c184b1fa. That non-flake inputs carry .rev is standard Nix behavior (inference).

</details>

### 7. Primary AI/help/mc entry points bound in major-mode-reserved C-c space are silently shadowed in the buffers they matter most

**`lisp/init-ai.el:120`** · layer: elisp

**What:** Global bindings sit in key space the conventions reserve for major modes (C-c C-<letter>, C-c <punctuation>): C-c RET gptel-send and C-c M-RET gptel-menu (init-ai.el:120-121); C-c C-e eca (init-ai.el:106); C-c C-' claude-code-ide-menu (init-ai.el:80); C-c C-d helpful-at-point (init-help.el:51); C-c C-< mc/mark-all-like-this (init-editing.el:133); C-c ; comment-line (init-editing.el:69/104). Global-map bindings lose to major-mode maps.

**Why it matters:** In org-mode, C-c RET/C-c C-e/C-c C-d/C-c ; are org-ctrl-c-ret/org-export-dispatch/org-deadline/org-toggle-comment — so gptel-send, eca, helpful-at-point, and comment-line are unreachable in exactly the prose buffers where gptel is most used. In python(-ts)-mode C-c C-< is python-indent-shift-left; in markdown-mode C-c C-d is markdown-do. The repo even shadows itself: init-navigation.el:108 binds C-c C-e to wdired-change-to-wdired-mode in dired-mode-map, hiding eca in every dired buffer. No error — the command just doesn't run, or another runs.

**Fix:**

Move to the user-reserved C-c <letter> space (free letters per init-keys.el:96-125: e, s, q, u, w, x, y, z), e.g. `C-c e` eca, `C-c q` claude-code-ide-menu, `C-c s`/`C-c S` gptel-send/gptel-menu, `[remap display-local-help]` (C-h .) for helpful-at-point, `C-S-c C-S-c` for mc/mark-all-like-this. Update the which-key label table in init-keys.el:96-125 to match. If any binding is kept deliberately (e.g. C-c ;), its @doc should state that org-mode shadows it.

<details><summary>Evidence</summary>

All bindings read directly at cited lines, incl. init-navigation.el:108 dired-mode-map wdired binding. Specific org/python/markdown defaults cited from knowledge of those modes' standard keymaps (inference flagged); reserved-space rule per Elisp manual Key Binding Conventions via .claude/skills/elisp-dev/references/conventions.md.

</details>

### 8. Global before-save whitespace-cleanup defeats super-save's except-current-line and mutates text at point during idle auto-saves

**`lisp/init-editing.el:42`** · layer: elisp

**What:** init-editing.el:42 hooks `whitespace-cleanup` into `before-save-hook` globally, while init-editing.el:146-154 enables super-save with `super-save-auto-save-when-idle t` and `super-save-delete-trailing-whitespace 'except-current-line`. super-save's idle save runs before-save-hook, so whitespace-cleanup strips trailing whitespace on the current line too — exactly what 'except-current-line was chosen to prevent. Also `whitespace-action '(auto-cleanup)` (line 45) is dead: it only takes effect in whitespace-mode, never enabled anywhere.

**Why it matters:** Type "foo ", pause past super-save-idle-duration, and the space before point is silently deleted — continuing to type yields "foobar". Every idle pause in a buffer with trailing whitespace mutates text under the cursor and causes spurious diffs on merely-auto-saved files.

**Fix:**

```elisp
(use-package whitespace
  :ensure nil
  :preface
  (defun jotain-editing--whitespace-cleanup-on-manual-save ()
    "Run `whitespace-cleanup' only for user-initiated saves."
    (when (memq this-command '(save-buffer save-some-buffers))
      (whitespace-cleanup)))
  :hook (before-save . jotain-editing--whitespace-cleanup-on-manual-save)
  :custom
  (whitespace-style '(face trailing tabs tab-mark)))
```
(drop the dead whitespace-action).

[verifier correction] WHAT: init-editing.el:42 hooks `whitespace-cleanup` into `before-save-hook` globally while init-editing.el:146-154 configures super-save with `super-save-delete-trailing-whitespace 'except-current-line`. super-save's idle/focus saves call `basic-save-buffer`, which runs `before-save-hook`, so `whitespace-cleanup` (whose `trailing` style item deletes ALL end-of-line whitespace, no whitespace-mode needed) strips the current line too — defeating the exclusion the config's own comment ("so you don't fight your own cursor") says it wants. Also `whitespace-action '(auto-cleanup)` (line 45) is dead: it is only consulted by whitespace-mode, which nothing in the repo enables. WHY: Type "foo ", pause past super-save-idle-duration, and the space before point is silently deleted; continuing to type yields "foobar". FIX: Guard the hook with super-save's purpose-built `super-save-in-progress` variable (bound to t around its save; its docstring documents exactly this pattern) rather than `this-command` gating, which would also silently disable cleanup for C-x C-c, save-buffers-kill-emacs, magit's save-some-buffers calls, and other legitimate programmatic saves:

(use-package whitespace
  :ensure nil
  :preface
  (defun jotain-editing--whitespace-cleanup-unless-auto-save ()
    "Run `whitespace-cleanup' except during super-save's automatic saves."
    (unless (bound-and-true-p super-save-in-progress)
      (whitespace-cleanup)))
  :hook (before-save . jotain-editing--whitespace-cleanup-unless-auto-save)
  :custom
  (whitespace-style '(face trailing tabs tab-mark)))

and drop the dead `(whitespace-action '(auto-cleanup))`. (If the pinned MELPA super-save predates `super-save-in-progress`, `bound-and-true-p` degrades to current behavior instead of erroring.)

<details><summary>Evidence</summary>

init-editing.el:40-45 and 146-154 read directly. That super-save's save path runs before-save-hook is inferred from it calling the standard save machinery.

</details>

### 9. tagref prog-mode hook hard-errors in the MELPA-fallback mode, aborting mode setup for every project file

**`lisp/init-prog.el:503`** · layer: elisp

**What:** `use-package tagref` is `:ensure nil` (Nix-only, not on MELPA) but installs `:hook (prog-mode . jotain-tagref--maybe-enable)` whose body calls `(tagref-mode 1)`. `:commands (tagref-mode)` makes use-package create an autoload stub even when the library is absent, so in a source checkout against a non-Jotain Emacs (the documented MELPA-fallback mode, init.el:21-26) every prog-mode buffer inside a project triggers the autoload, which fails with "Cannot open load file: tagref".

**Why it matters:** Surfaces as "File mode specification error" on every code file and aborts the remaining prog-mode-hook entries for that buffer (flymake, tempel capf, eglot auto-start, dtrt-indent — whichever run after it), so the fallback delivery mode is functionally broken for programming buffers. (claude-code-ide/majutsu/combobulate share the seam but only error on explicit keypress, which degrades acceptably.)

**Fix:**

```elisp
(defun jotain-tagref--maybe-enable ()
  "Enable `tagref-mode' only inside a project, when tagref is installed."
  (when (and (project-current)
             (require 'tagref nil t))
    (tagref-mode 1)))
```

<details><summary>Evidence</summary>

init-prog.el:500-512 read directly (`:ensure nil ... :commands (tagref-mode) :hook (prog-mode . jotain-tagref--maybe-enable)` with `(tagref-mode 1)` unguarded). tagref comes via nix/extra-packages.nix; no MELPA fallback. That an unfulfilled :commands autoload errors on call is inferred from use-package's documented autoload generation.

</details>

### 10. diff-hl `:after magit` + `:hook (after-init . global-diff-hl-mode)` — global-diff-hl-mode never runs; pre-refresh hook is wired to `ignore`

**`lisp/init-vc.el:218`** · layer: elisp

**What:** The diff-hl block (init-vc.el:217-232) combines `:after magit`, `:demand t`, and `:hook ((after-init . global-diff-hl-mode) …)`. use-package's :after wraps the entire expansion — including the :hook add-hook forms — in `with-eval-after-load 'magit`. magit is bind-deferred (init-vc.el:157-161) and nothing loads it during init, so `add-hook 'after-init-hook #'global-diff-hl-mode` only executes when the user first presses C-x g — long after after-init-hook fired. A hook added after its one-shot moment never runs. Additionally `(magit-pre-refresh . ignore)` (init-vc.el:228) is a no-op placeholder where upstream's canonical pairing is `diff-hl-magit-pre-refresh` (present post-refresh half is on the next line).

**Why it matters:** global-diff-hl-mode is never enabled in any session — no fringe VC indicators, ever; the module's headline feature is dead. Even after magit loads, diff-hl-magit-post-refresh only refreshes buffers where diff-hl-mode is already on (none), and without the pre-refresh half indicators would be stale around stage/unstage anyway.

**Fix:**

```elisp
(use-package diff-hl
  :custom
  (diff-hl-draw-borders nil)
  (fringes-outside-margins t)
  (diff-hl-side 'left)
  :hook
  ((after-init . global-diff-hl-mode)
   (dired-mode . diff-hl-dired-mode)
   (magit-pre-refresh  . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-flydiff-mode 1))
```
Drop `:after magit` and `:demand t` entirely — adding functions to magit-{pre,post}-refresh-hook is safe before magit loads (hooks are just variables).

<details><summary>Evidence</summary>

init-vc.el:217-232 read directly (`:after magit`, `:demand t`, `(magit-pre-refresh . ignore)`); magit deferred via :bind at init-vc.el:158-161; grep verified no other module requires magit at startup. :after wrapping :hook registration is inferred from use-package :after handler semantics; the canonical hook pair is from diff-hl upstream docs (inference flagged).

</details>

### 11. `(eval-when-compile (require 'doom-modeline))` loads doom-modeline eagerly during init in interpreted runs

**`lisp/init-vc.el:533`** · layer: elisp

**What:** init-vc.el:533 has a top-level `(eval-when-compile (require 'doom-modeline))` for the doom-modeline-def-segment/-def-modeline macros. In interpreted execution eval-when-compile behaves like progn — the require runs at load time. The config normally runs uncompiled (module.nix installs raw .el; `just run-built` launches checkout sources; .elc only exists after a manual `just compile`, and `just clean` removes them). So doom-modeline plus its chain (doom-modeline-core, shrink-path, nerd-icons and its multi-hundred-KB glyph data) loads mid-init-vc — defeating init-ui.el:74's deliberate after-init deferral.

**Why it matters:** One line silently moves one of the heaviest UI packages from after-init back onto the pre-first-frame critical path. Estimated 50-150 ms with compiled package files, more cold (estimate).

**Fix:**

```elisp
;; init-vc.el:533 — only require at actual byte-compile time:
(eval-when-compile
  (when (bound-and-true-p byte-compile-current-file)
    (require 'doom-modeline nil t)))
```
The runtime users of the two macros are already inside `(with-eval-after-load 'doom-modeline …)` at line 535, so at run time the macros are available when the forms execute.

<details><summary>Evidence</summary>

init-vc.el:533; init-ui.el:73-75 defers doom-modeline to after-init. eval-when-compile==progn when interpreted per the GNU Elisp manual (Eval During Compile). That the deployed config is interpreted is from module.nix:338-343 (verbatim .el install) — inferred.

</details>


## Medium

### 12. CLAUDE.md and modules.mdx cite a corfu/completion-preview pairing that no longer exists in the config

**`CLAUDE.md:84`** · layer: docs

**What:** CLAUDE.md:84 gives as the canonical no-split example: '`corfu` and `completion-preview` are both in `init-completion.el`', and docs/architecture/modules.mdx:8 repeats it (additionally miscasting corfu as an enhancer of completion-preview). Grep across the repo: completion-preview appears only in these two doc files and the skill's NEWS digest — zero matches under lisp/; init-completion.el (read in full, 319 lines) is corfu+cape only. Emacs 31 also promotes completion-preview-sort-function to a user option specifically to pair with Corfu (NEWS.31).

**Why it matters:** The flagship example of the repo's most-emphasized convention points at removed code. Anyone auditing convention compliance chases a ghost, or 're-adds' completion-preview to make the doc true, or misfiles a package based on the inverted relationship in modules.mdx.

**Fix:**

Either (a) docs-only: replace the example with a real pair (e.g. dirvish→dired, magit→vc are still accurate; drop the corfu→completion-preview line from modules.mdx:8), or (b) adopt the built-in alongside corfu:
```elisp
(use-package completion-preview
  :ensure nil
  :hook ((prog-mode text-mode) . completion-preview-mode)
  :config
  (when (get 'completion-preview-sort-function 'custom-type) ; user option in 31
    (setopt completion-preview-sort-function corfu-sort-function)))
```
(making the sentence true again — a feature decision, not just a doc fix).

### 13. CLAUDE.md's CI description no longer matches ci.yml, and deploy.yml (which carries the heavy checks and cache pushes) is undocumented

**`CLAUDE.md:17`** · layer: docs

**What:** CLAUDE.md:17 says ci.yml's check job runs `nix flake check` (building packages-default etc.), that the jylhis cachix cache pulls AND pushes, and that a concurrency group preserves protected-branch runs. Reality: ci.yml is pull_request-only (22-23), runs eight individual check builds (56-78), explicitly skips elisp-compile and package builds (2-13), and PR runs are pull-only with no CACHIX_AUTH_TOKEN (16-19). The full `nix flake check`, devenv-test-with-push, Pages deploy, and all cachix pushes live in .github/workflows/deploy.yml (33-48, 45, 64), never mentioned; protected-branch concurrency is deploy.yml:23-25.

**Why it matters:** An agent asked to 'make CI pass' runs `nix flake check` locally believing it equals PR CI — it is strictly heavier and can fail on things PR CI never exercises; conversely deploy-only checks gate main invisibly. Anyone reasoning about cachix token exposure from CLAUDE.md gets the security model backwards (pushing is post-merge only — the correct design ci.yml:16-19 documents).

**Fix:**

Rewrite the CI paragraph: 'PR CI (ci.yml) runs a lightweight per-check subset (formatting, statix, deadnix, module-eval, packages-doc[-in-sync], options-doc, elisp-lint) plus devenv test, pull-only cachix. Full validation — nix flake check incl. elisp-compile, package builds, elisp-test — plus cachix pushes and the Pages deploy run in deploy.yml on pushes to main/next.' Also update the 'Check / test responsibility split' section (CLAUDE.md:154) to enumerate the current nix/checks.nix check set (module-eval, nix-on-droid-module-eval, jylhis-emacs-smoke, emacs-binaries, elisp-test, packages-doc, packages-doc-in-sync, options-doc — checks.nix:120-332).

### 14. Accumulated CLAUDE.md architecture drift: overlay.nix is a wrapper, extra-packages has 5 packages not 2, `just fmt`/`just test` misdescribed, flake outputs and init.el behavior stale, three inconsistent Emacs-version claims

**`CLAUDE.md:136`** · layer: docs

**What:** (a) CLAUDE.md:136 says devenv.nix builds jotainEmacs and extra-packages.nix adds 'two MELPA-absent packages'; it defines five (jylhis-emacs-themes, claude-code-ide, combobulate, majutsu, tagref at lines 6,19,34,49,69), and the languages.emacs-lisp/eask-cli/ellsp/elsa claims are commented out (devenv.nix:48,114-123). (b) The overlay section describes overlay.nix as the implementation; overlay.nix:12 is a 12-line wrapper over nix/mk-overlay.nix:114-171, which also exports `eca` (139) and jotainEmacsPackagesNoGui under a parameterized mkJotainEmacsPackages/curatedGrammars design; overlay.nix:3-8's own header is likewise incomplete. (c) CLAUDE.md:25 'no tests yet' — test/devenv-test.el exists (14 KB) and `just test` builds the elisp-test flake check (Justfile:129-130). (d) CLAUDE.md:29 says `just fmt` = `nixfmt .`; the recipe is `nix fmt` (Justfile:305-306). (e) The flake-outputs list omits emacs-lite/emacs-jylhis/packages-doc (flake.nix:119,121,128) and six Justfile recipes are undocumented. (f) CLAUDE.md:78 'refreshes archive contents on first run' vs init.el:68-109's staleness-gated 7-day background idle refresh with JOTAIN_NO_PACKAGE_REFRESH opt-out. (g) CLAUDE.md:7 'Emacs 30+' vs flake.nix:2 'Emacs 31+' vs nix-build.mdx:18 'Emacs 30 release'. Also line 115 documents a sonarlintLs overlay attr with zero consumers.

**Why it matters:** CLAUDE.md is the contract agents are told overrides default behavior; each stale claim sends an agent to the wrong file (editing overlay.nix expecting package logic), wrong command (`nixfmt .` formats differently than the treefmt wrapper), or wrong mental model (no tests to keep passing; synchronous first-run refresh). The 30-vs-31 inconsistency matters given the stated Emacs 31 target.

**Fix:**

Docs-only, all in CLAUDE.md unless noted: rewrite line 136 (dev shell = tooling only; extra-packages = 5 pkgs consumed by mk-overlay); rewrite the overlay paragraph to point at nix/mk-overlay.nix and list the real attrs (jotainEmacs, jotainEmacsNoGui, jylhisEmacs, sonarlintLs, eca, jotainInfo, jotainEmacsPackages{,NoGui}, jylhisEmacsPackages, curatedGrammars); fix lines 25, 29, 78, the flake outputs list, and line 7 (one version story: 31 target, 30.1 floor per init.el:5). Update overlay.nix:3-8's header. Align nix-build.mdx:18.

### 15. Dev shell ships google-fonts (plus two nerd-font sets) for an Emacs that is no longer in the shell

**`devenv.nix:103`** · layer: nix

**What:** devenv.nix:4-6 removed Emacs from the shell because the ~1 GB closure dominated direnv allow time, yet packages still include `nerd-fonts.jetbrains-mono`, `nerd-fonts.iosevka`, and `google-fonts` (devenv.nix:101-103), justified as 'Fonts used by the Emacs configuration' — but no Emacs in the shell can use them, and terminal Emacs uses the terminal's font. google-fonts is the entire Google Fonts collection (order ~1-2 GB unpacked; inference, not measured).

**Why it matters:** Every cold-cache `direnv allow`/CI `devenv test` pulls a font collection nothing in the shell consumes — the same latency/closure cost the Emacs removal was meant to eliminate.

**Fix:**

Drop all three until Emacs returns to the shell. If a subset is needed (e.g. `just screenshot` under Xvfb), pin only the named families: `(google-fonts.override { fonts = [ "Source Serif 4" ]; })` and keep the two nerd-fonts.

[verifier correction] devenv.nix:103 pulls the entire google-fonts collection (~1 GB class) into the dev-shell closure although lisp/init-ui.el only ever probes one Google family, "Literata" (init-ui.el:104), for the variable-pitch face used by the Xvfb screenshot Emacs (`just screenshot` / `just run-built`). This partially undercuts the closure-size rationale documented at devenv.nix:4-6 for removing Emacs from the shell. The two nerd-fonts sets (devenv.nix:101-102) are correctly present — init-ui.el:94-95 probes them for the default face and init-ui.el:251-256 for nerd-icons glyphs, so they must NOT be dropped. Fix: replace `google-fonts` with `(google-fonts.override { fonts = [ "Literata" ]; })` (the `fonts` subset argument is verified to exist in current nixpkgs); keep both nerd-fonts packages and xvfb-run unchanged. No impact on nix-build outputs, macport/nox/droid, or CI flake checks — this is dev-shell-only.

### 16. nix-build.mdx describes a default.nix and grammar-wiring mechanism that no longer exist

**`docs/architecture/nix-build.mdx:75`** · layer: docs

**What:** nix-build.mdx:75 says default.nix 'imports emacs.nix... and, when withTreeSitterGrammars is true (default), wraps the result with... with-all-grammars. ...early-init.el wires them in via TREE_SITTER_DIR / treesit-extra-load-path.' Reality: default.nix:1-19 is a 19-line flake-compat shim with no withTreeSitterGrammars argument; grammar bundling lives in nix/mk-overlay.nix:31-79 (extraEmacsPackages + curatedGrammars); early-init.el:123-126 explicitly states 'No TREE_SITTER_DIR handling is needed here' — discovery is nixpkgs' site-start.el setting treesit-extra-load-path.

**Why it matters:** This page is the Nix-layer architecture reference and is compiled into the shipped Info manual. Someone extending grammar handling will look for a nonexistent argument or re-add TREE_SITTER_DIR plumbing the config deliberately removed.

**Fix:**

Replace the default.nix section with the flake-compat description (CLAUDE.md's is correct here) and rewrite the grammar paragraph: grammars come from epkgs.treesit-grammars in nix/mk-overlay.nix (with-all-grammars default, curated 26-set behind curatedGrammars / packages.emacs-lite), discovered via nixpkgs site-start's treesit-extra-load-path (early-init.el:123-126) and propagated to async native-comp workers by init-prog.el.

### 17. native-comp-async-jobs-number hardcoded to 3 for one specific machine; use num-processors

**`early-init.el:113`** · layer: elisp

**What:** early-init.el:106-113 sets `native-comp-async-jobs-number 3` with a comment literally saying 'This machine has 4 physical / 8 logical cores'. The same early-init.el ships unchanged to every delivery target: macOS, multi-core Linux workstations, and nix-on-droid phones.

**Why it matters:** On a 2-core Android device or small VM, 3 async native-comp jobs oversubscribe the CPU and cause exactly the input/redisplay starvation the comment tries to prevent; on a 16-core workstation it needlessly slows eln-cache warm-up. `num-processors` has shipped since 28.1 (below the 30 floor — no guard needed).

**Fix:**

```elisp
        native-comp-async-jobs-number (max 1 (min 3 (/ (num-processors) 2)))
```
with the comment rewritten to the policy ('half the cores, max 3, min 1') instead of one machine's specs.

[verifier correction] WHAT: early-init.el:106-113 hardcodes `native-comp-async-jobs-number 3` with a comment describing one specific machine (4c/8t). plan.md §1 documents this as deliberate tuning for the author's primary machine, but the same early-init.el ships unchanged to every delivery target, including the nix-on-droid terminal build (module-nix-on-droid.nix uses jotainEmacsPackagesNoGui, and emacs.nix enables native compilation by default, so the `native-comp-available-p` guard passes there). WHY: Emacs' own default (0) means "half the logical CPUs, min 1" — so on a 2-core Android device or small VM the hardcoded 3 is strictly worse than stock Emacs (which would run 1 job), oversubscribing the CPU during eln warm-up. Exposure is transient and limited to MELPA-fallback installs and JIT compilation of .elc files without a matching .eln (Nix pre-builds most .eln), but it is exactly the input/redisplay starvation the comment tries to prevent. FIX: `native-comp-async-jobs-number (max 1 (min 3 (/ (num-processors) 2)))` — `num-processors` shipped in 28.1, below the repo's Emacs 30 floor, so no guard is needed. This evaluates to exactly 3 on the author's 8-thread machine, so it preserves the tuning plan.md documents while scaling down on smaller hosts; rewrite the comment to state the policy ("half the logical cores, capped at 3, min 1") instead of one machine's specs. Note the cap at 3 is retained deliberately (responsiveness over warm-up speed on big machines) — this fix does not, and need not, speed up 16-core workstations. Evidence correction: the overlay file is overlay.nix (mk-overlay.nix does not exist); droid wiring is module-nix-on-droid.nix.

### 18. Explicit withXwidgets pass diverges from emacs-overlay's defaults on Darwin, silently busting cache parity for git/unstable/igc there

**`emacs.nix:107`** · layer: nix

**What:** withXwidgets defaults to `!noGui && (withGTK3 || withPgtk || withNS || variant == "macport")` (emacs.nix:107) and is always forwarded (line 280). The comment at 111-112 states emacs-overlay's git/unstable/igc bases default xwidgets OFF and 'our value wins there too'. On Linux both sides are false (parity holds); on Darwin withNS is true, so jotain passes withXwidgets = true to a base whose cached attr was built with xwidgets off — a different store path: `nix-build emacs.nix --arg variant '"git"'` on aarch64-darwin is a from-source Emacs+WebKit build, contradicting the invariant block's 'only rev pins and the Darwin patch flags diverge' (lines 218-233).

**Why it matters:** A Darwin developer running just build-git/build-igc pays a multi-hour source build while the docs promise cache hits, and the parity documentation is wrong about its own exception list, so future edits validate against a false model.

**Fix:**

Only include withXwidgets in overrideArgs when it differs from the base's default, e.g. `lib.optionalAttrs (withXwidgets != xwidgetsBaseDefault) { inherit withXwidgets; }`, or default it to the base's own default for overlay variants. At minimum amend the comment at emacs.nix:230-233 to document the intentional Darwin rebuild.

### 19. patchBranch picks the '31' Darwin patch branch for the mainline variant, which is actually Emacs 30.2

**`emacs.nix:306`** · layer: nix

**What:** patchBranch (emacs.nix:306-312) selects 'unstable' for git/igc, '30' for macport, and '31' for everything else — including default mainline. But mainline is pkgs.emacs = 30.2 on the current lock (evaluated). So `--arg withSystemAppearancePatch true` on a Darwin mainline build fetches patches from `patches-31` and applies them to 30.2 source; the file's own comment (314-315) says the 30 branch content differs from 31.

**Why it matters:** The patch either fails to apply (build error on the exact documented macOS path) or applies with fuzz and silently misbehaves. The bug flips silently again whenever nixpkgs promotes Emacs 31 to pkgs.emacs — the branch is keyed on variant name, not on what the base package actually is.

**Fix:**

```nix
  patchBranch =
    if variant == "git" || variant == "igc" then "unstable"
    else if lib.versionOlder basePackage.version "31" then "30"
    else "31";
```
The pinned darwinPatchHashes (316-330) already carry per-branch hashes, so mainline uses the '30' hashes macport already exercises.

### 20. Flake modules hand consumers only the jotain overlay, not emacs-overlay — module-installed distributions resolve epkgs from the consumer's nixpkgs and never match CI-cached artifacts

**`flake.nix:97`** · layer: nix

**What:** packages.default is built with both overlays (flake.nix:61-64: emacs-overlay then self.overlays.default), so the use-package scan resolves against emacs-overlay's MELPA snapshot. But homeManagerModules/nixosModules/nixOnDroidModules (flake.nix:93-111) pass only `_module.args.jotainOverlay = self.overlays.default`, and module.nix:24-25 does `pkgs.extend jotainOverlay` on consumer pkgs; mk-overlay.nix:24 then uses `inherit (final) emacsPackagesFor` — without emacs-overlay that is nixpkgs' own older emacs package set. Same repo revision, two different jotainEmacsPackages: flake-built vs module-built, with different package versions and a different set of silently-missing attrs (see fallthrough finding).

**Why it matters:** CI builds and pushes the flake variant to the jylhis cachix cache, so every HM/NixOS/nix-darwin user activating via the modules gets a guaranteed cache miss and rebuilds ~96 melpaBuild derivations locally; package-version behavior differs between `nix build .#default` and what `services.jotain` actually installs, so 'works in the flake build' does not transfer to deployments.

**Fix:**

Compose emacs-overlay into the overlay handed to modules (emacs-overlay supports 24.05+, so portability holds):
```nix
homeManagerModules.default = { ... }: {
  imports = [ ./module.nix ];
  _module.args.jotainOverlay = nixpkgs.lib.composeExtensions
    emacs-overlay.overlays.default
    self.overlays.default;
};
# same for nixosModules.default / nixOnDroidModules.default
```
Or document the divergence and stop implying the cachix cache covers module users.

[verifier correction] WHAT (corrected): flake.nix builds packages.default with emacs-overlay + the jotain overlay (flake.nix:61-64), so the use-package scan resolves epkgs against emacs-overlay's MELPA/ELPA snapshot (emacs-overlay's package.nix overrides emacsPackagesFor, confirmed at the pinned rev). The HM/NixOS/darwin/droid module outputs (flake.nix:93-111) hand consumers only self.overlays.default, and all three modules pkgs.extend it over consumer pkgs, so mk-overlay.nix:24's `inherit (final) emacsPackagesFor` resolves to the consumer's nixpkgs snapshot. Same repo rev therefore yields two different jotainEmacsPackages: package versions differ, and use-package.nix's `epkgs.${name} or trace "…falling through" null` means a package present in one snapshot but not the other is silently dropped — CI validates a package set module deployments never install. WHY (corrected): this is a validation-transfer gap, not a rebuild catastrophe — module users' epkgs come Hydra-cached from cache.nixos.org (nixpkgs builds emacsPackages for the default emacs, and bare jotainEmacs keeps pkgs.emacs store-path parity even without emacs-overlay), so no mass local melpaBuild rebuild occurs; the cost is version skew and silently-missing packages relative to what CI byte-compiled and cached. FIX (corrected): either (a) document the divergence — module installs resolve epkgs from the consumer's nixpkgs (Hydra-cached, may lag or lack packages the flake build validated) — or (b) compose emacs-overlay into the module-provided overlay via nixpkgs.lib.composeExtensions, understanding the trade-off: it buys version parity everywhere and jylhis-cachix hits only for consumers following jotain's nixpkgs pin, while consumers on their own nixpkgs (supported 24.05+) lose Hydra-cached epkgs and build the overlay snapshot locally. If (b), keep the standalone `import ./module.nix` fallback (module.nix:24, plain ./overlay.nix, deliberately emacs-overlay-free) working, and verify emacs-overlay's package.nix still evaluates on the oldest supported nixpkgs before claiming 24.05 portability.

### 21. Native env loader runs a synchronous `devenv hook-should-activate` subprocess on the find-file path, re-paying it every 30 seconds per project

**`lisp/devenv.el:419`** · layer: elisp

**What:** devenv-env--turn-on (devenv.el:1155-1172) is the turn-on function of the globalized devenv-env-global-mode (devenv.el:1195-1201), which init-devenv.el:60-64 enables at after-init. For every buffer inside a devenv project it calls (devenv--activation-permits-p (devenv--trust-state root)) at line 1171. devenv--trust-state (419-436) runs `call-process devenv -q hook-should-activate` SYNCHRONOUSLY via devenv--cached under the generic `devenv-cache-ttl` of 30 seconds (defcustom at 118-120; freshness check at 353-358 reads the dynamic devenv-cache-ttl). The env pairs cache deliberately overrides this to 3600s (devenv-env--cache-ttl, 1033-1037, applied in devenv-env--cached-pairs 1077-1082), but the trust cache was not given the same treatment. The turn-on also runs `executable-find devenv-executable` per buffer (1165), a full PATH scan on every miss.

**Why it matters:** Every first file-open in a devenv project — and again after ANY 30 seconds of cache staleness, i.e. effectively on most find-files during normal editing cadence — blocks the UI on spawning the devenv CLI. The library itself documents the opposite discipline for the modeline: devenv-modeline--cached-trust says "Never runs devenv: modeline code must stay subprocess-free on the synchronous path" (1264-1268) and probes asynchronously (1270-1285). The 30s TTL buys nothing: trust state only changes via devenv-allow/devenv-revoke, and both already invalidate the cache explicitly (447, 464). On the HM deployment the cost is a per-buffer PATH-scan miss instead (devenv is absent from the wrapper PATH), so both deployment modes pay something on every buffer.

**Fix:**

Elisp only. Cache trust like the env, not like task lists — move the defconst above devenv--trust-state and bind it:

```elisp
(defconst devenv-env--cache-ttl 3600 ...)  ; moved above devenv--trust-state

(defun devenv--trust-state (root)
  "Return ROOT's auto-activation trust state (cached)."
  ;; Trust only changes via `devenv-allow'/`devenv-revoke', which both
  ;; invalidate this cache explicitly — cache it for the long TTL.
  (let ((devenv-cache-ttl devenv-env--cache-ttl))
    (devenv--cached root 'trust (lambda () ...unchanged...))))
```

Better still, make devenv-env--turn-on subprocess-free: consult only devenv-modeline--cached-trust, and when the state is unknown kick devenv-modeline--probe-trust (already async, writes the same (root . trust) cache key at 1279-1284) with a callback that enables devenv-env-mode in the project's buffers once `allowed` lands — the same replay pattern the env fetch already uses.

### 22. zoxide-add runs on every find-file but the zoxide binary is wired into no Nix layer

**`lisp/init-completion.el:255`** · layer: seam

**What:** init-completion.el:251-255 hooks `zoxide-add` on find-file unconditionally. `zoxide` appears nowhere in module.nix runtimeDeps, devenv.nix packages, module-system.nix, or module-nix-on-droid.nix — unlike every other unconditionally-invoked binary, which module.nix documents.

**Why it matters:** On any Nix-delivered install without user-installed zoxide, every file open shells out to (or errors on) a missing binary — at best a silent no-op leaving M-g z permanently empty, at worst a per-find-file error. Violates the module.nix contract that unconditional runtime deps ride the wrapper PATH.

**Fix:**

Nix (module.nix runtimeDeps): add `zoxide # zoxide-add on find-file-hook, zoxide-find-file (M-g z)`. Elisp (defensive): gate the block with `:if (executable-find "zoxide")`.

### 23. consult `:demand t` forces the largest minibuffer package to load at startup for no functional gain

**`lisp/init-completion.el:117`** · layer: elisp

**What:** init-completion.el:116-118 gives consult :demand t although its 40+ :bind table would defer it perfectly. The only startup-relevant side effects are in :init (register-preview advice + setopt, which run eagerly regardless) and :config (`(require 'consult-xref)`, xref setopts, consult-customize) — all fine at first load. consult is ~5k lines.

**Why it matters:** Pure startup cost, zero user-visible benefit: every entry point is a bound command; first C-x b would load it in single-digit ms. Estimated 15-60 ms saved per start (estimate).

**Fix:**

Drop `:demand t`; in :init set `xref-show-xrefs-function`/`xref-show-definitions-function` to `#'consult-xref` (autoloaded from consult's package autoloads, so setting the symbols doesn't load consult); keep consult-customize and `(setopt consult-narrow-key "<")` in :config. Verify consult-xref carries an autoload cookie in the installed package; if not, wrap the two setopts in `(with-eval-after-load 'xref (require 'consult-xref) …)`.

[verifier correction] What: init-completion.el:117 gives consult `:demand t` although all entry points are autoloaded via :bind/:hook, so the largest minibuffer package loads eagerly at startup for no startup-time-relevant side effect. Why: pure startup cost (~5.6k lines; estimated 15-60 ms, unmeasured — verify with `just bench`); repo actively tunes startup. Fix: drop `:demand t`; move the two xref setopts from :config to :init — verified safe: upstream consult ships `;;;###autoload` cookies on both `consult-xref` (consult-xref.el) and `consult-register-window` (consult-register.el), matching upstream README's recommended lazy config, and the `(require 'consult-xref)` in :config becomes unnecessary; keep consult-customize and `consult-narrow-key` in :config. Additionally handle the knock-on: consult-eglot (init-prog.el:371), consult-eglot-embark (init-prog.el:378), and embark-consult (init-completion.el:231) all use `:after consult`, so their bindings (C-M-. in eglot-mode-map, C-c C-o in minibuffer-local-map) would stay inactive until consult first loads — either accept and document that (any bound consult command, e.g. first C-x b, activates them) or rework consult-eglot's binding to a plain autoloaded :bind not gated on consult. Elisp-only change; no nix-build/platform impact.

### 24. GC threshold restored at load of the FIRST init module, defeating the early-init bump for ~95% of init

**`lisp/init-core.el:38`** · layer: elisp

**What:** early-init.el:16 sets gc-cons-threshold to most-positive-fixnum 'during startup', but init-core.el:38-39 restores 16 MiB / 0.1 at plain load time — and init-core is the first of 28 modules required (init.el:111). Every subsequent module load, every :demand'ed package load (orderless, vertico, marginalia, consult, corfu, cape, auto-dark, nerd-icons, pulsar, sops, auth-source-1password, apheleia, kkp, keyfreq, super-save, exec-path-from-shell…), and all after-init work runs at the small threshold.

**Why it matters:** The point of the early-init bump is avoiding GC during init; restoring before 27 of 28 modules load puts dozens of GC pauses back inside startup (estimate 10-30 GCs — measure via `gcs-done`; bench/init.el:55 prints it). The classic mistake the emacs-internals objects-and-gc reference warns about.

**Fix:**

```elisp
;; init-core.el — replace lines 34-40:
(defconst jotain-core-gc-cons-threshold (* 16 1024 1024)
  "Steady-state `gc-cons-threshold' after startup.")
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold jotain-core-gc-cons-threshold
                  gc-cons-percentage 0.1))
          90)
(run-with-idle-timer 5 t #'garbage-collect)
```
and reference the constant in the minibuffer-exit-hook lambda (init-core.el:48) instead of the duplicated literal.

[verifier correction] WHAT: early-init.el:16 raises gc-cons-threshold to most-positive-fixnum "during startup", but init-core.el:38-39 lowers it back to 16 MiB / 0.1 at plain load time, and init-core is the first of 29 modules required (init.el:111) — so every subsequent module load and eagerly-loaded package runs at the steady-state threshold, re-enabling GC for most of init. The repo's own reference (.claude/skills/emacs-internals/references/objects-and-gc.md:82-84) claims the restore happens "on `emacs-startup-hook`" — the code contradicts its own documentation. WHY: startup pays avoidable GC pauses; magnitude is unmeasured (use `just bench` / gcs-done to quantify — likely hundreds of ms, not seconds, since 16 MiB is already 20x the default), and cost is amortized under the daemon model, hence medium not high. FIX (correct as proposed): define `(defconst jotain-core-gc-cons-threshold (* 16 1024 1024))`, move the `(setq gc-cons-threshold jotain-core-gc-cons-threshold gc-cons-percentage 0.1)` into `emacs-startup-hook` (late depth), keep the idle-timer garbage-collect, and reference the constant in the minibuffer-exit-hook lambda (init-core.el:48) instead of the duplicated literal — the latter deduplication was already recommended by docs/reviews/2026-07-deep-research-review.md finding 6b. Add a comment noting that an init error leaves the threshold at most-positive-fixnum until the idle timer or minibuffer hooks touch it. Safe for batch byte-compile (hook never runs, no effect on the elisp-compile check), daemon mode (emacs-startup-hook runs after daemon init), and all build variants; the skill reference then becomes accurate instead of aspirational.

### 25. exec-path-from-shell :if guard omits pgtk — PATH inheritance never runs on the Wayland/pgtk variant

**`lisp/init-core.el:279`** · layer: elisp

**What:** init-core.el:279-280: `:if (or (daemonp) (memq window-system '(mac ns x)))`. On a pgtk build (`just build-pgtk`, a first-class variant; the HM module on Wayland) a non-daemon GUI session has `window-system` = `pgtk`, so the block is disabled and exec-path-from-shell-initialize never runs.

**Why it matters:** A desktop-launched pgtk Emacs inherits the compositor's minimal PATH: every `executable-find` probe evaluated at init degrades — LSP servers, `devenv` (init-devenv.el:70), wakatime's :if gate (init-tracking.el:30-31) silently disables itself, dired gls probes (init-navigation.el:23-29), `rass`, `op`, jj/git helpers. A quiet feature-off that looks like misconfiguration, and a per-variant behavioral divergence the config otherwise works hard to avoid. (nox/tty is unaffected.)

**Fix:**

```elisp
  :if (or (daemonp)
          (memq window-system '(mac ns x pgtk)))
```

[verifier correction] What: init-core.el:279-280 — the exec-path-from-shell :if guard `(or (daemonp) (memq window-system '(mac ns x)))` omits `pgtk`, so a non-daemon GUI session of the pgtk variant (`just build-pgtk` / `--arg withPgtk true`) never runs exec-path-from-shell-initialize; confirmed against Emacs 30.2 sources that pgtk sessions report window-system 'pgtk. Why: a desktop-launched pgtk Emacs keeps the compositor's minimal PATH, silently degrading every init-time executable-find probe (LSP servers, devenv, wakatime's :if gate, dired gls, etc.) — contradicting the block's own doc comment. Scope correction: the Home Manager module is NOT affected (its desktop entry runs emacsclient against the daemon, where (daemonp) is t, and its default package is the X11 build — XWayland reports window-system 'x); only directly-launched pgtk-variant sessions hit this, which is a supported but non-default path — hence medium, not high. Fix (correct as proposed, safe on tty/nox/macport/android): `:if (or (daemonp) (memq window-system '(mac ns x pgtk)))`.

### 26. Minibuffer GC hooks are unremovable lambdas that hardcode the threshold and misbehave with recursive minibuffers

**`lisp/init-core.el:45`** · layer: elisp

**What:** (1) minibuffer-setup/exit hooks get raw lambdas — impossible to remove-hook and re-added as duplicates on re-load; (2) the exit hook hardcodes `(* 16 1024 1024)`, duplicating line 38, so retuning requires editing two places; (3) with `enable-recursive-minibuffers t` (init-core.el:79), exiting an inner minibuffer restores 16 MiB while the outer is still active, so the 'no GC while completing' guarantee lapses in nested sessions. Same unquoted-lambda-in-hook pattern at init.el:102-109 and init-prog.el:305-308, 324-336.

**Why it matters:** Duplicated magic number invites drift; unremovable hook entries defeat interactive debugging and violate the repo's own elisp-dev guidance; the recursive case reintroduces GC pauses mid-completion exactly where the hooks claim to prevent them.

**Fix:**

```elisp
(defconst jotain-core--gc-threshold (* 16 1024 1024))
(defun jotain-core--gc-defer ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun jotain-core--gc-restore ()
  (when (< (minibuffer-depth) 2)
    (setq gc-cons-threshold jotain-core--gc-threshold)))
(add-hook 'minibuffer-setup-hook #'jotain-core--gc-defer)
(add-hook 'minibuffer-exit-hook  #'jotain-core--gc-restore)
```
Apply the same named-function treatment to init.el:102-109 and init-prog.el:305/324.

### 27. Pre-seeding Info-directory-list suppresses Info's default initialization — all other manuals vanish

**`lisp/init-docs.el:36`** · layer: elisp

**What:** init-docs.el:16 requires info eagerly at startup, then (lines 29-36) does `(add-to-list 'Info-directory-list found)` at load time when a jotain.info is found. `Info-directory-list` is nil-until-initialized: `Info-initialize` only builds the real list (INFOPATH + Info-default-directory-list) `(unless Info-directory-list …)`. Setting it to a one-element list at init makes Info-initialize a no-op, so C-h i shows ONLY the Jotain manual — Emacs's own manuals, the elisp manual, and every system Info dir disappear.

**Why it matters:** In exactly the source-checkout scenario this module serves (result-info/ present), it breaks the entire Info system instead of adding one manual. The eager `(require 'info)` also costs a startup load for a feature most sessions never open.

**Fix:**

```elisp
;; init-docs.el — use the variable designed for additions, no eager require:
(defvar Info-additional-directory-list)
(let ((found (seq-find (lambda (dir)
                         (and (stringp dir)
                              (file-exists-p (expand-file-name "jotain.info" dir))))
                       jotain-info--candidate-paths)))
  (when found
    (with-eval-after-load 'info
      (add-to-list 'Info-additional-directory-list found))))
```
Delete the top-level (require 'info) at line 16.

[verifier correction] WHAT: init-docs.el:36 does `(add-to-list 'Info-directory-list found)` at load time. `Info-directory-list` is nil-until-initialized and `info-initialize` is `(unless Info-directory-list …)` (Emacs 30 info.el:755), so seeding it while still nil suppresses the INFOPATH/default initialization and C-h i would show only the Jotain manual. In the common steady-state source checkout this does NOT fire: package activation runs before init.el and package.el calls `info-initialize` itself when any installed package ships an Info `dir` file (package-activate-1, package.el:912-916; the quickstart file emits the same call, package.el:4637-4643) — MELPA magit/transient do, so the list is already fully built and init-docs merely prepends. The defect is confined to sessions where Info-directory-list is still nil when init-docs loads: (a) the first bootstrap run of a fresh checkout with result-info/ pre-built or JOTAIN_INFO_DIR set (init-docs at init.el:116 loads before init-vc installs magit, and magit's later info-initialize becomes a no-op — Info crippled for that one session, self-heals on restart), and (b) package.el-free sessions (pure-Nix jotainEmacsPackages) with JOTAIN_INFO_DIR set, where it also masks the wrapper's INFOPATH. The eager `(require 'info)` at line 16 is a small avoidable startup cost (largely moot in steady state, since quickstart/package activation loads info anyway). WHY: latent footgun that silently disables the rest of the Info system in edge sessions, and relies on an accidental ordering guarantee (some MELPA package shipping Info docs) to work at all. FIX: as proposed by the reviewer — use `Info-additional-directory-list` (the variable designed for additions, appended after Info-directory-list in Info-find-file) inside `with-eval-after-load 'info` with a `(defvar Info-additional-directory-list)` declaration for warning-clean byte-compilation, and drop the top-level `(require 'info)`. Verified the fix is safe in both nix-build and `just run` modes and on all variants (it never touches Info-directory-list, so INFOPATH-based initialization is preserved).

### 28. Entire `use-package newcomment` block (including its @doc comment) is duplicated verbatim

**`lisp/init-editing.el:102`** · layer: elisp

**What:** init-editing.el:54-74 and 89-109 are byte-identical @doc comment + `(use-package newcomment :ensure nil :bind ("C-c ;" . comment-line) :custom ...)` blocks (same four :custom settings), with the transpose-commands block (76-87) sandwiched between.

**Why it matters:** Harmless at runtime (second evaluation is idempotent) but it double-pays the eager :custom evaluation, doubles the newcomment entry in the generated package reference (nix/use-package.nix harvests ;;; @doc per form), double-registers the C-c ; binding, and is a copy-paste landmine — editing one copy silently diverges from the other.

**Fix:**

Delete lines 89-109 (the second @doc comment plus second use-package form), keeping the first block at 54-74.

### 29. jotain-sonarlint let-binds eglot-server-programs, which can leave it void if eglot first loads inside the call

**`lisp/init-prog.el:425`** · layer: elisp

**What:** `jotain-sonarlint` does `(let ((eglot-server-programs ...)) (call-interactively #'eglot))` without requiring eglot first. eglot is deferred. If `M-x jotain-sonarlint` runs before eglot has ever loaded, the let dynamically binds a previously unbound symbol; eglot.el loads inside the let, its defcustom sees the variable bound and skips initialization, and when the let unwinds the variable reverts to void — every subsequent eglot connection fails with void-variable. Even in the benign case the sonarlint session sees only the sonarlint entry, losing the config's add-to-list entries (340-364) and eglot's built-in contact fallbacks.

**Why it matters:** One early jotain-sonarlint call can permanently break eglot for the session.

**Fix:**

```elisp
(defun jotain-sonarlint ()
  "Start SonarLint analysis in the current project."
  (interactive)
  (require 'eglot)
  (let ((eglot-server-programs
         (cons `(,major-mode . ("sonarlint-ls" "-stdio"))
               eglot-server-programs)))
    (call-interactively #'eglot)))
```

[verifier correction] WHAT: `jotain-sonarlint` (init-prog.el:425) let-binds `eglot-server-programs` without requiring eglot first. If it is the first thing to load eglot in a session, behavior splits by config form. Interpreted (plain `just run`): the symbol is not yet special, the let binds it lexically, and eglot.el's own defvar then signals "Defining as dynamic an already lexical var: eglot-server-programs" — the command errors out (eglot itself recovers on the next plain load). Byte-compiled: the let is dynamic (use-package requires eglot at compile time), the sonarlint session works, and — contrary to the original claim — the variable is properly initialized after unwind (Emacs 30's custom-initialize-reset sets the toplevel value through any active let), so eglot is NOT left void/broken; however, the use-package :config `add-to-list` registrations run inside the let and are silently discarded on unwind, losing the rass TS/Python function contacts for the rest of the session (go/docker survive via eglot's built-in defaults). Shadowing the full list inside the let is intentional (it forces the sonarlint-ls contact), not a defect. WHY: an interactive command that hard-errors on early first use (interpreted case) and can silently drop the session's custom TS/Python server resolvers (compiled case); mitigated by the narrow window — `jotain-prog--maybe-eglot-ensure` loads eglot as soon as any non-elisp project file is visited. FIX (as proposed, correct): add `(require 'eglot)` before the let; the `cons`-onto-existing-list variant is fine since the prepended sonarlint entry still matches first.

### 30. fset of jsonrpc--log-event to ignore silences logging for every jsonrpc client (dape, mcp) — redundant with the eglot-events-buffer-config already set

**`lisp/init-prog.el:367`** · layer: elisp

**What:** eglot's :config ends with `(fset #'jsonrpc--log-event #'ignore)`, permanently replacing a `--`-private function of jsonrpc.el. The block already sets `eglot-events-buffer-config '(:size 0 :format short)` (init-prog.el:292), the supported way to disable eglot's event log since Eglot 1.16 (with :size 0 the logging path short-circuits). dape (init-prog.el:390-400) and mcp.el (init-ai.el:170-172) are also jsonrpc.el clients.

**Why it matters:** Debugging a broken MCP server or DAP adapter loses its primary diagnostic channel with no way to re-enable it short of re-loading jsonrpc; redefining another package's private function breaks silently if jsonrpc renames or changes the signature (likelier with the Emacs 31 target). Also the exact pattern the repo's elisp-dev skill warns against.

**Fix:**

Delete the fset line; `eglot-events-buffer-config :size 0` already suppresses eglot's log.

### 31. eglot-confirm-server-initiated-edits is an obsolete alias (Eglot 1.16) — rename to eglot-confirm-server-edits

**`lisp/init-prog.el:290`** · layer: elisp

**What:** :custom sets `(eglot-confirm-server-initiated-edits nil)`. In the eglot bundled with Emacs 30 and 31 this is `(define-obsolete-variable-alias 'eglot-confirm-server-initiated-edits 'eglot-confirm-server-edits "1.16")` (verified in both emacs-30 and emacs-31 eglot.el; new defcustom with richer alist semantics at emacs-31 eglot.el:502, default `'((t . maybe-summary))`). The runtime `byte-compile-warnings '(not obsolete)` (early-init.el:80) hides the deprecation from native-comp logs.

**Why it matters:** Works only through a 2023-era compat alias; when it is dropped (the repo targets Emacs 31, where such aliases are removal candidates) the :custom entry silently sets a dead variable and 'don't confirm server edits' reverts to the new default — a behavior change nobody is warned about.

**Fix:**

```elisp
-  (eglot-confirm-server-initiated-edits nil)
+  (eglot-confirm-server-edits nil)
```
No guard needed: eglot-confirm-server-edits exists in every eglot >=1.16, i.e. everything at or above the Emacs 30 floor.

### 32. eglot-documentation-renderer adoption is permanently inert: markdown-ts-view-mode is never fboundp when the guard runs

**`lisp/init-prog.el:313`** · layer: elisp

**What:** The Emacs-31 block `(when (and (boundp 'eglot-documentation-renderer) (fboundp 'markdown-ts-view-mode)) (setopt ...))` runs in eglot's :config. On Emacs 31 both symbols are real (verified: defcustom at emacs-31 eglot.el:540; define-derived-mode at markdown-ts-mode.el:5441) — but markdown-ts-mode.el contains NO autoload cookies (grep-verified), is not preloaded, and nothing loads it, so `(fboundp 'markdown-ts-view-mode)` is nil even on 31 and the setopt never executes.

**Why it matters:** The intended 31 feature (tree-sitter markdown rendering of LSP hover docs) never activates on any Emacs version, and the guard style makes this indistinguishable from 'running Emacs 30' — no error, ever. Illustrates the general hazard of the config's 14 guarded 31-blocks: a wrong or unloaded symbol silently degrades to a no-op forever.

**Fix:**

```elisp
  ;; markdown-ts-mode.el ships in 31 but has no autoloads — require it:
  (when (and (boundp 'eglot-documentation-renderer)
             (require 'markdown-ts-mode nil t)
             (fboundp 'markdown-ts-view-mode))
    (setopt eglot-documentation-renderer 'markdown-ts-view-mode))
```
Consider a one-shot startup self-check (or ERT test in the proposed 31-variant CI check) warning for every expected-on-31 symbol still unbound when emacs-major-version >= 31, so guarded adoptions can't rot silently.

### 33. Merged tempel+eglot capf survives eglot shutdown — completion then signals "No current JSON-RPC connection" in every previously-managed buffer

**`lisp/init-snippets.el:66`** · layer: elisp

**What:** jotain-tempel-eglot-capf (init-snippets.el:59-70) replaces the buffer-local capf list with (cape-capf-super #'tempel-complete #'eglot-completion-at-point) and removes the raw eglot entry. When the eglot server later shuts down (M-x eglot-shutdown, server crash, or the shutdown half of eglot-reconnect — which devenv-reload offers to run, devenv.el:976-994), eglot's managed-mode teardown removes only the raw #'eglot-completion-at-point from completion-at-point-functions (verified in the shipped Emacs 30.2 eglot.el line 2076) — the super-capf closure is not that symbol, so it survives. The teardown re-run of eglot-managed-mode-hook is a no-op because the guard is `(when (and (eglot-managed-p) (not jotain-tempel--eglot-merged))` — managed-p is nil and the guard flag stays t (line 63-64), so nothing restores the plain tempel capf either.

**Why it matters:** In the orphaned buffer, the next completion attempt calls the stale super-capf → eglot-completion-at-point → eglot-server-capable → eglot--current-server-or-lose, which SIGNALS (verified in Emacs 30.2 eglot.el: eglot-completion-at-point at 3189-3193 calls eglot-server-capable, which at 1149 calls eglot--current-server-or-lose unconditionally). Neither completion--capf-wrapper nor cape-capf-super demotes capf errors, so C-M-i and corfu's auto-popup error out on every trigger — the config turns eglot's clean degradation (fall back to dabbrev/tempel) into a hard per-keystroke error until the server reconnects. The guard flag also stays t forever, so even after reconnect the tempel merge is never re-evaluated against the NEW eglot capf ordering.

**Fix:**

Elisp only — store the merged capf in the guard variable and handle the teardown branch (eglot-managed-mode-hook runs on both enable and disable):

```elisp
(defvar-local jotain-tempel--eglot-merged nil
  "Merged tempel+eglot capf installed in this buffer, or nil.")

(defun jotain-tempel-eglot-capf ()
  (if (eglot-managed-p)
      (unless jotain-tempel--eglot-merged
        (remove-hook 'completion-at-point-functions #'tempel-complete t)
        (setq jotain-tempel--eglot-merged
              (cape-capf-super #'tempel-complete #'eglot-completion-at-point))
        (setq-local completion-at-point-functions
                    (cons jotain-tempel--eglot-merged
                          (remq #'eglot-completion-at-point
                                completion-at-point-functions))))
    ;; Teardown: drop the merged capf (its eglot half now signals) and
    ;; restore the plain tempel capf; also re-arms the merge for reconnect.
    (when jotain-tempel--eglot-merged
      (setq-local completion-at-point-functions
                  (remq jotain-tempel--eglot-merged
                        completion-at-point-functions))
      (setq jotain-tempel--eglot-merged nil)
      (add-hook 'completion-at-point-functions #'tempel-complete -90 t))))
```

### 34. auth-source-1password and sops are `:demand t` — two eager loads for lazily-needed features

**`lisp/init-systems.el:20`** · layer: elisp

**What:** init-systems.el:18-27 (auth-source-1password :demand t) and :39-50 (sops :demand t, global-sops-mode). auth-source lookups only happen when a consumer (gptel/forge) asks for a credential — registration can be lazy via `with-eval-after-load 'auth-source`. sops' global mode hooks find-file, so after-init suffices (after-init-hook runs before command-line file arguments are visited).

**Why it matters:** Two package loads plus keymap/minor-mode surgery on the init critical path for features used minutes later or never. Estimated 5-25 ms combined (estimate).

**Fix:**

auth-source-1password: `:defer t` + `:init (with-eval-after-load 'auth-source (auth-source-1password-enable))` (if not autoloaded, `(require 'auth-source-1password)` inside the with-eval-after-load). sops: `:hook (after-init . global-sops-mode)`, keymap wiring unchanged in :config.

### 35. nerd-icons loads eagerly at startup (use-package with only :config), dragging nerd-icons-completion with it

**`lisp/init-ui.el:248`** · layer: elisp

**What:** init-ui.el:248-256 `(use-package nerd-icons :config …)` has no deferral keyword, so it's required during init, pulling its glyph data files (nerd-icons-data-*.el, several hundred KB of alists). Everything consuming it is already deferred: doom-modeline at after-init (74), nerd-icons-corfu/-completion/-ibuffer are :after (260-279), dirvish attributes load on first dired. Chain effect: because nerd-icons and marginalia both end up loaded at startup, `nerd-icons-completion :after (nerd-icons marginalia)` (268-272) also fires eagerly, loading a third package.

**Why it matters:** Estimated 30-100 ms of pre-first-frame load (glyph alists are notoriously slow; estimate) that would be free if it loaded with doom-modeline at after-init.

**Fix:**

Add `:defer t` to the nerd-icons block; :config still runs at first load (triggered by doom-modeline at after-init) and the :after chains follow it there.

### 36. nerd-icons-font-family selection is gated on display-graphic-p at load time, so it never applies under the daemon

**`lisp/init-ui.el:250`** · layer: elisp

**What:** nerd-icons' :config wraps the font-family probe in `(when (display-graphic-p) ...)` evaluated once at load. Under the HM daemon (module.nix runs Jotain as a user-session daemon) no graphical frame exists at load time, so nerd-icons-font-family is never set for subsequently created GUI frames. The file's own font machinery (init-ui.el:130-131, 157-158) solves exactly this with server-after-make-frame-hook; this block doesn't use it.

**Why it matters:** Daemon-created frames fall back to nerd-icons' default family instead of the matched 'Nerd Font' from jotain-font-preferences — icons in dired/corfu/marginalia/modeline render with the wrong font or as tofu, on the primary deployment mode (daemon + emacsclient).

**Fix:**

```elisp
(use-package nerd-icons
  :preface
  (defun jotain-ui--apply-nerd-icons-font (&optional frame)
    (when (display-graphic-p frame)
      (when-let* ((nerd-font
                   (cl-loop for (family . _height) in jotain-font-preferences
                            when (and (string-match-p "Nerd Font" family)
                                      (find-font (font-spec :family family) frame))
                            return family)))
        (setopt nerd-icons-font-family nerd-font))))
  :config
  (jotain-ui--apply-nerd-icons-font)
  (add-hook 'server-after-make-frame-hook #'jotain-ui--apply-nerd-icons-font))
```

### 37. Binding C-x C-g removes the C-g escape from the C-x prefix

**`lisp/init-vc.el:46`** · layer: elisp

**What:** `use-package vc-git :bind ("C-x C-g" . jotain-switch-git-status-buffer)` binds a sequence ending in C-g. In stock Emacs, C-g after a prefix aborts the pending key sequence; once bound, 'pressed C-x, changed my mind, hit C-g' launches a completing-read over git-changed files (or a 'Not inside a git repository' message) instead of cancelling.

**Why it matters:** C-g-after-prefix is a deeply ingrained abort gesture; the key-binding conventions explicitly discourage sequences ending in the quit character because users rely on C-g always meaning quit. Nothing destructive (the resulting minibuffer aborts on the next C-g), but a quit gesture that must never do anything now does something — and it doubles down with its conventions-violating jj twin on C-x C-j.

**Fix:**

```elisp
-  :bind ("C-x C-g" . jotain-switch-git-status-buffer)
+  :bind ("C-x G" . jotain-switch-git-status-buffer)
```
(C-x G is free and sits mnemonically next to C-x g magit-status / C-x M-g magit-dispatch, init-vc.el:159-160.)

### 38. Re-declaring doom-modeline's 'main modeline as a frozen copy will silently drift from upstream

**`lisp/init-vc.el:550`** · layer: elisp

**What:** To append one segment, the config re-issues `doom-modeline-def-modeline 'main` with a hand-copied list of doom-modeline's default segments (comment at 548-549 admits it 'mirrors doom-modeline's default main definition'). doom-modeline is installed unpinned from MELPA in mode (a) and tracks emacs-overlay's snapshot in mode (b), so the upstream default segment list changes under this copy.

**Why it matters:** Any segment upstream adds/renames/removes (doom-modeline does this regularly) is silently dropped or becomes void in this config's modeline; the two package-resolution modes can also disagree, giving different modelines per delivery mode.

**Fix:**

Attach the counter through the generic mechanism instead of forking 'main:
```elisp
(add-to-list 'mode-line-misc-info
             '(:eval (when-let* ((file buffer-file-name)
                                 (rb (and (mode-line-window-selected-p)
                                          (jotain-git-stats--root-and-backend file))))
                       (progn (jotain-git-stats--maybe-refresh (car rb) (cdr rb))
                              (or (jotain-git-stats--render (car rb)) ""))))
             t)
```
(doom-modeline renders misc-info in its default 'main, so the counter survives upstream changes).

### 39. global-jinx-mode is unconditional but no Nix layer provides spell-check dictionaries for enchant

**`lisp/init-writing.el:58`** · layer: seam

**What:** init-writing.el:57-60 enables global-jinx-mode at emacs-startup in every delivery mode. The Nix side ships the jinx package (jinx-mod links against enchant) but no module provides an enchant backend dictionary: no aspell/aspellDicts/hunspellDicts/nuspell in module.nix runtimeDeps, module-system.nix:113-118, module-nix-on-droid.nix:101-105, or the wrapper (mk-overlay.nix:98-111 only touches INFOPATH).

**Why it matters:** On a fresh NixOS/HM/droid deployment with no system dictionaries, jinx cannot resolve any language: buffers either error ('No dictionaries available') or silently check nothing — the config's only spell checker (flyspell deliberately replaced) is dead out of the box on the platforms the modules claim to provision turnkey.

**Fix:**

Nix (module.nix runtimeDeps, mirrored into the shared runtime-deps list):
```nix
      (aspellWithDicts (d: [ d.en ]))  # enchant backend + base dictionary for jinx
      enchant
```
plus an option (e.g. services.jotain.spell.dictionaries) so users can extend languages. No Elisp change.

[verifier correction] WHAT: init-writing.el:57-60 enables global-jinx-mode unconditionally, but no Nix delivery layer provisions an enchant backend dictionary: module.nix runtimeDeps (78-91), module-system.nix systemPackages (113-118), module-nix-on-droid.nix packages (101-105), and the mk-overlay.nix wrapper (INFOPATH only, 105-111) all lack aspell/hunspell dicts. nixpkgs' emacsPackages.jinx links jinx-mod against enchant_2 only, and enchant_2 propagates no English dictionaries (only hspell/Hebrew plus bare libaspell). WHY: on a fresh NixOS/HM/nix-on-droid deployment the config's only spell checker (flyspell deliberately replaced) does nothing: jinx logs "Jinx: No dictionaries available for ..." and silently highlights nothing (it does not error). The gap is acknowledged in journal/2026-06-09.md as a deferred follow-up, and journal/2026-07-20.md:58 incorrectly claims jotainEmacsPackages already ships the dictionary — the code contradicts that. FIX (the reviewer's PATH-based fix is a no-op — libaspell/libenchant run in-process via jinx-mod and never consult PATH; aspellWithDicts sets ASPELL_CONF only in its own bin wrappers): deliver dictionaries where libaspell actually looks. Either (a) add pkgs.aspellDicts.en (plus a services.jotain.spell.dictionaries option for more languages) to home.packages in module.nix, environment.systemPackages in module-system.nix, and environment.packages in module-nix-on-droid.nix — nixpkgs' libaspell NIX_PROFILES patch then finds $profile/lib/aspell at runtime; or (b) export ASPELL_CONF "dict-dir ${aspellWithDicts (d: [ d.en ])}/lib/aspell; data-dir ..." in module.nix's emacsWrapper and the module-system/droid editor scripts, which also covers shells where NIX_PROFILES is absent. No Elisp change. Do not add enchant or aspellWithDicts to runtimeDeps/PATH — it has no effect on jinx.

### 40. global-jinx-mode on emacs-startup-hook has no failure guard; a jinx module build failure aborts the remaining startup hooks

**`lisp/init-writing.el:58`** · layer: elisp

**What:** `(use-package jinx :hook (emacs-startup . global-jinx-mode))`. jinx compiles a native module against enchant on first load; in MELPA-fallback mode (no prebuilt module, possibly no cc/enchant) global-jinx-mode signals during emacs-startup-hook. run-hooks stops at the first erroring entry, and because use-package's add-hook prepends, jinx runs before the background archive-refresh lambda registered in init.el:101-109 — which then never fires.

**Why it matters:** One missing system dependency becomes (a) a startup error and (b) silent loss of later emacs-startup-hook entries. The config's stated policy is that optional tooling degrades gracefully.

**Fix:**

```elisp
(use-package jinx
  :preface
  (defun jotain-writing--enable-jinx ()
    (with-demoted-errors "jotain: jinx unavailable: %S"
      (global-jinx-mode 1)))
  :hook (emacs-startup . jotain-writing--enable-jinx)
  :bind (("M-$" . jinx-correct) ("C-M-$" . jinx-languages)))
```

### 41. NixOS/nix-darwin and nix-on-droid modules ship Emacs without the runtime binaries the config invokes unconditionally

**`module-system.nix:113`** · layer: nix

**What:** module.nix:78-91 documents and provides runtimeDeps (ripgrep, fd, git, jujutsu, coreutils, eca, rsync) because the Elisp config invokes them unconditionally. module-system.nix:113-118 installs only selectedPackage, editor scripts, and eca; module-nix-on-droid.nix:101-105 likewise — no ripgrep, fd, git, jj, rsync.

**Why it matters:** On hosts not independently installing these: `xref-search-program 'ripgrep` (init-prog.el:488-491) breaks xref fallback search; consult-ripgrep/consult-fd (init-completion.el:153,158) fail; modeline VC stats probes spawn git/jj every 30s (init-vc.el:456-477) and the error path (418-421) silently maps to 0/0 so the segment lies; magit/vc/vc-jj need git/jj; dired-rsync (init-navigation.el:81-85) needs rsync. Same config, three module entry points, one satisfies the documented runtime contract.

**Fix:**

Factor the list into nix/runtime-deps.nix:
```nix
{ pkgs, pkgsWithOverlay }: with pkgs; [ ripgrep fd git jujutsu coreutils pkgsWithOverlay.eca rsync ]
```
module.nix keeps wrapping; module-system.nix adds `++ import ./nix/runtime-deps.nix { inherit pkgs pkgsWithOverlay; }` to environment.systemPackages, and module-nix-on-droid.nix the same for environment.packages. No Elisp change.

[verifier correction] WHAT: module.nix:78-91 documents and satisfies the config's unconditional runtime dependencies (ripgrep, fd, git, jujutsu, direnv, coreutils, eca, rsync) by prepending them to the Emacs wrapper's PATH; module-system.nix:113-118 and module-nix-on-droid.nix:101-105 install only the Emacs package, editor scripts, and eca — and the package itself (nix/mk-overlay.nix) wraps binaries only for INFOPATH, so ripgrep/fd/git/jj are absent unless the host installs them independently. The modules' inclusion of eca (surfaced on the overlay expressly for this purpose, mk-overlay.nix:136-139) shows the runtime-dep contract is intended to apply to them too. WHY: on hosts without these tools, xref-search-program 'ripgrep (init-prog.el:488-491) breaks xref search, consult-fd/consult-ripgrep (init-completion.el:153,158) fail, and the modeline VC-stats probes (init-vc.el:456-477) silently map missing git/jj binaries to 0/0 (418-421), so the segment shows wrong data; magit/vc/vc-jj need git/jj. (rsync is a marginal case: NixOS ships it via environment.defaultPackages and macOS ships 2.6.9.) FIX: do NOT dump the module.nix list verbatim into environment.systemPackages — coreutils there shadows the BSD userland system-wide on nix-darwin, and on darwin systemPackages never reaches a Dock/launchd-launched Emacs anyway (the very gap module.nix's wrapper closes). Instead, factor the tool list into nix/runtime-deps.nix and have module-system.nix and module-nix-on-droid.nix wrap the selected package's bin/* with a `--suffix PATH : ${lib.makeBinPath runtimeDeps}` (extending the existing makeBinaryWrapper re-wrap step in mk-overlay.nix, or a per-module outer wrapper like module.nix's), keeping coreutils and direnv wrapper-scoped. Adding ripgrep/fd/git/jujutsu (not coreutils) to systemPackages/environment.packages is an acceptable simpler alternative on NixOS and nix-on-droid, but does not fix darwin GUI launches.

### 42. Darwin: `coreutils` in the wrapper does not provide `gls`, so init-navigation's GNU-ls detection never fires (and GNU ls shadows BSD ls as plain `ls`)

**`module.nix:86`** · layer: nix

**What:** module.nix:86 adds `coreutils # gls, used by dirvish-listing-switches on darwin`. nixpkgs' coreutils installs unprefixed binaries; g-prefixed names come from `coreutils-prefixed`. init-navigation.el:23-31 and :91 branch exclusively on `(executable-find "gls")`.

**Why it matters:** On macOS under the HM module the gls probes fail, so the config takes the degraded BSD-ls path (dired-use-ls-dired nil, no --group-directories-first, ls-lisp fallback per init-navigation.el:89-97) — even though GNU ls IS first on PATH as plain `ls`, which additionally swaps GNU userland in front of BSD tools for every subprocess Emacs spawns.

**Fix:**

```diff
-      coreutils # gls, used by dirvish-listing-switches on darwin
+      coreutils-prefixed # gls — init-navigation.el probes the g-prefixed name on darwin
```
(or keep plain coreutils Linux-only and add coreutils-prefixed via `lib.optional isDarwin`). No Elisp change.

### 43. No delivery mode byte-compiles or AOT-native-compiles the config — all ~30 modules load as interpreted source every daemon start

**`module.nix:341`** · layer: nix

**What:** module.nix:338-343 installs early-init.el, init.el, and lisp/ as raw .el symlinks; mk-overlay.nix compiles only packages; the in-tree compile paths are disabled stubs (Justfile:97-121). Loading plain .el never triggers deferred native compilation (that path requires .elc), so the config executes interpreted and var/eln-cache holds nothing for it. Justfile:108-111's own comment concedes AOT 'belongs in the Nix/home-manager deploy'.

**Why it matters:** The repo pays for a warning-clean byte-compile gate in CI yet no user ever runs the compiled artifact: measurable startup/runtime cost across ~30 modules (init-vc.el alone is 600 lines of hot modeline code). The CI-verified artifact (.elc semantics, eager macroexpansion) differs from the artifact actually executed.

**Fix:**

Nix (module.nix): build a compiled copy and point xdg.configFile."emacs/lisp".source at it:
```nix
compiledConfig = pkgs.runCommand "jotain-config-compiled" { buildInputs = [ selectedPackage ]; } ''
  cp -r ${./.}/lisp $out; ...
  emacs --batch -L $out/lisp \
    --eval '(setq byte-compile-error-on-warn t)' \
    -f batch-byte-compile $out/lisp/*.el $out/early-init.el $out/init.el
'';
```
(keep .el next to .elc; optionally `-f batch-native-compile` for AOT .eln). No Elisp change.

[verifier correction] What/why stand as claimed (module.nix:338-343 ships raw .el symlinks; mk-overlay.nix compiles only packages; JIT native comp requires .elc, so the config runs interpreted forever and var/eln-cache holds at most trampolines for it — an AOT gap the Justfile:108-111 comment itself concedes belongs in the Nix/HM deploy). Corrected fix — model the derivation on the already-working nix/checks.nix elisp-compile invocation, not the sketch: (a) copy the config out of the store and `chmod -R u+w` before compiling; (b) invoke `${selectedPackage}/bin/emacs --batch -L lisp --eval "(require 'pcre2el)" --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile early-init.el init.el lisp/init-*.el` — the pcre2el require is load-bearing (journal/2026-04-16.md: magit-todos pulls in pcre2el, whose defadvice byte-compiles its advice body and fails under error-on-warn); (c) install the results keeping .el beside .elc so deferred native comp can later produce .eln into the writable var/eln-cache: point xdg.configFile entries "emacs/lisp" at the compiled lisp/ AND add separate "emacs/early-init.elc" / "emacs/init.elc" (plus the existing .el) entries, since repointing only "emacs/lisp" leaves the two entry files interpreted. Optionally add `-f batch-native-compile` for full AOT .eln. No Elisp change; module.nix-only, so `just run`, droid, macport/nox paths are unaffected.

### 44. HM module ships the emoji fallback font but none of the Nerd Fonts the icon stack requires

**`module.nix:96`** · layer: nix

**What:** module.nix:93-96 adds only noto-fonts-color-emoji (Linux). But init-ui.el's default face preferences are Nerd Fonts (init-ui.el:93-96) and the entire icon stack — nerd-icons (248-256), nerd-icons-corfu/-completion/-ibuffer, doom-modeline with doom-modeline-icon t (78), dirvish's nerd-icons attribute (init-navigation.el:157) — needs Nerd Font glyphs. devenv.nix:98-103 ships these fonts for the dev shell and says 'on your real system they come from home-manager or equivalent' — yet this repo's own HM module doesn't provide them.

**Why it matters:** A fresh `services.jotain.enable = true` deployment renders in DejaVu fallback with tofu boxes across the modeline, corfu margins, dired/dirvish, and ibuffer — the out-of-the-box experience the module exists to guarantee, on both Linux and macOS.

**Fix:**

```diff
   emojiFontPackages = lib.optional isLinux pkgs.noto-fonts-color-emoji;
+  iconFontPackages = [ pkgs.nerd-fonts.jetbrains-mono ]; # matches jotain-font-preferences
```
append `++ iconFontPackages` to home.packages (module.nix:322-332). fonts.fontconfig.enable is already forced on Linux (320); macOS HM installs fonts to ~/Library/Fonts automatically.

### 45. elisp-test hard-codes test/devenv-test.el while AGENTS.md tells contributors to add test-*.el files — new test files are silently never run

**`nix/checks.nix:328`** · layer: seam

**What:** The elisp-test check loads exactly one file: `-l test/devenv-test.el` (checks.nix:327-329). AGENTS.md's Testing Guidelines say 'Place ERT files under test/ using test-*.el names… Run just test for ERT.' A contributor's test/test-foo.el gets paren-checked by elisp-lint (checks.nix:283 globs all test/*.el) but never loaded by elisp-test, so `just test` (Justfile:129-130) and deploy.yml's `nix flake check` stay green with the new tests never executed. The existing file also violates the documented naming.

**Why it matters:** A test harness that silently ignores new tests manufactures false confidence exactly when someone starts adding coverage — no error, just a green check.

**Fix:**

```nix
  ''
    cd $src
    emacs --batch -L lisp -L test \
      --eval '(dolist (f (directory-files "test" t "\\.el$")) (load f nil t))' \
      -l ert -f ert-run-tests-batch-and-exit
    touch $out
  '';
```
Docs: fix AGENTS.md's Testing Guidelines to the glob convention (or the actual current name). No Elisp change.

### 46. Repo claims to target Emacs 31 but the default build ships Emacs 30.2, leaving every guarded 31 adoption in lisp/ dead

**`nix/mk-overlay.nix:115`** · layer: nix

**What:** flake.nix:2 says 'GNU Emacs 31+ configuration' and emacs.nix:17 labels the default build 'Emacs 31 release branch (default)', but mainline resolves to `pkgs.emacs` on the pinned root nixpkgs — evaluated to 30.2. mk-overlay.nix:115/120 build jotainEmacs/jotainEmacsNoGui with no `variant`, so every delivery mode ships 30.2. At least 14 guarded Emacs-31 adoptions are inert on the shipped binary (init-core.el:204,228; init-keys.el:158-167; init-editing.el:33-36; init-navigation.el:47-48; init-vc.el:33-36; init-prog.el:28-29/313-320/480-481; init-help.el:22-23; init-shell.el:44; init-terminal.el:243-244). No CI gate byte-compiles against 31 (nix/checks.nix:298-314 elisp-compile uses the 30.2 distribution).

**Why it matters:** The stated goal (target Emacs 31) is silently unmet, and 31 obsoletions (if-let, ts-indent-offset renames) cannot fail the warnings-as-errors gate before the switch happens.

**Fix:**

Option A (target 31 now): in nix/mk-overlay.nix pass `variant = "unstable"` (emacs-overlay's emacs-unstable = 31 release branch per emacs.nix:186-190) to both jotainEmacs and jotainEmacsNoGui. Tradeoffs: default moves from Hydra to nix-community.cachix.org (add to CI substituters and flake nixConfig); cache parity per-variant is preserved (the outPath check covers `unstable`); macport stays 30; verify emacs-unstable.version on the pinned overlay and the x86_64-darwin 26.05 pin first. Option B (stay on 30 honestly): revert the '31' claims in flake.nix:2 and emacs.nix:17 to '30' and add an `elisp-compile-emacs31` check to nix/checks.nix using `variant = "unstable"` with the same batch-byte-compile recipe. Either way bump init.el:5 Package-Requires when the floor moves.

[verifier correction] Stale version claims: flake.nix:2 ("GNU Emacs 31+ configuration") and emacs.nix:17 ("Emacs 31 release branch (default)") misstate what the default build produces. On the pinned nixpkgs, mainline resolves to pkgs.emacs = Emacs 30.2 (Emacs 31 exists only as the 31.0.90 pretest attr emacs31), so every delivery mode ships 30.2 — consistent with CLAUDE.md ("Emacs 30+", "mainline ... currently the Emacs 30 release") and init.el's Package-Requires ((emacs "30.1")), but contradicting these two headers. The many boundp/fboundp guards for 31-only symbols are deliberate graceful-degradation (init-terminal.el:67 says so explicitly), not dead code. Fix: correct flake.nix:2 and emacs.nix:17 to say Emacs 30 (and emacs.nix:23's "latest release tag" → note it is the emacs-31 release branch, currently the 31.0.90 pretest). Optionally, as an enhancement, add an elisp-compile-emacs31 check in nix/checks.nix using variant = "unstable" so 31 obsoletions hit the warnings-as-errors gate early. Do NOT change the default variant in nix/mk-overlay.nix — mainline = pkgs.emacs is the documented, Hydra-cached design, and "unstable" would ship an unreleased pretest by default.

### 47. jylhisEmacs builds a different source per entry point: flake uses the dev-branch input, standalone overlay.nix falls back to a hardcoded 6-month-old rev

**`nix/mk-overlay.nix:125`** · layer: nix

**What:** overlay.nix:12 is `import ./nix/mk-overlay.nix { }`, so `jylhisEmacsSrc = null` (mk-overlay.nix:2) and jylhisEmacs (125-132) falls back to emacs-jylhis.nix's hardcoded `rev ? "eaf289b4..."` (emacs-jylhis.nix:27-28, no rationale comment). The flake path passes `jylhisEmacsSrc = inputs."jylhis-emacs"` — dev head c39fcf76 (2026-07-04). module.nix:24 uses the raw overlay.nix fallback whenever imported outside the flake, so `services.jotain.emacsBackend = "jylhis"` yields a different Emacs than `nix build .#jylhis-emacs` from the same checkout.

**Why it matters:** Same attribute name, two silently different sources months apart — different crashes per entry point when debugging the fork, and the fallback rev decays indefinitely since nothing ties it to flake.lock's pin.

**Fix:**

Make emacs-jylhis.nix read the pin from flake.lock like emacs.nix does for nixpkgs:
```nix
rev ? (builtins.fromJSON (builtins.readFile ./flake.lock)).nodes."jylhis-emacs".locked.rev,
hash ? (builtins.fromJSON (builtins.readFile ./flake.lock)).nodes."jylhis-emacs".locked.narHash,
```
(with fetchTarball so narHash applies), or at minimum comment that the fallback must track flake.lock and add a `just verify` comparison.

[verifier correction] WHAT: overlay.nix:12 imports nix/mk-overlay.nix with `{ }`, so `jylhisEmacsSrc = null` and jylhisEmacs (mk-overlay.nix:125-132) falls back to emacs-jylhis.nix's hardcoded `rev ? "eaf289b4..."` (emacs-jylhis.nix:27-28, dated 2026-05-08, no comment tying it to flake.lock). The flake passes `jylhisEmacsSrc = inputs."jylhis-emacs"` (flake.nix:89-91), locked to c39fcf76 (2026-07-04) — about two months newer. So a raw `import ./overlay.nix` consumer (the usage documented in overlay.nix's header) or a raw `import ./module.nix` with `emacsBackend = "jylhis"` builds a different Emacs source than `nix build .#jylhis-emacs` from the same checkout. Note the exposure is limited to genuinely non-flake imports: flake.nix:93-111 injects the input-fed overlay into all module exports via `_module.args.jotainOverlay`, and default.nix (flake-compat) serves the flake's overlays.default. WHY: Same attribute name, two silently diverging sources for an (experimental, per CLAUDE.md) fork build; CI (nix/checks.nix:124) only ever builds the flake path, so the fallback rev is untested and decays indefinitely — nothing ties it to flake.lock's pin, violating the repo's stated convention that non-flake consumers read pins from flake.lock (which emacs-jylhis.nix already honors for nixpkgs at lines 8-24). FIX (unchanged, verified correct): default rev/hash in emacs-jylhis.nix from flake.lock's nodes."jylhis-emacs".locked.{rev,narHash}, using fetchTarball so narHash applies (github-type flake inputs record the unpacked-tree NAR hash, so it also works as fetchFromGitHub's `hash`); or at minimum comment the coupling and add a `just verify` comparison. Source-only change; no platform or nix-build vs `just run` impact.

### 48. Scanner parse gaps are latent landmines: bare :disabled unrecognized, alias regex drops '+', comments/strings scanned as real forms — the top-level-forms contract is enforced only by a comment

**`nix/use-package.nix:99`** · layer: nix

**What:** Three fidelity holes, currently unexercised by lisp/ (grep-verified) but silently wrong when used: (1) isDisabled (use-package.nix:99) matches only `:disabled t`; use-package also honors bare `:disabled`, which the scanner treats as enabled and ships anyway. (2) resolveEnsureName's alias class `wordChar = "[A-Za-z0-9_-]"` (line 94) omits `+` while the head regex (line 59) includes it — `:ensure foo+` resolves to `foo`. (3) The head regex matches anywhere, including inside comments and strings; the 'never nested/conditional/commented' contract exists only as a comment (lines 14-18) with no check against a real Elisp read.

**Why it matters:** Each hole converts an innocuous Elisp edit (adding `:disabled`, commenting out a block, quoting an example in a docstring) into a wrong Nix package set with no error.

**Fix:**

(a) line 99: `isDisabled = block: match (".*:disabled([[:space:]]+t)?(" + endSym + ".*)?") block != null;` (b) line 94: wordChar → `"[A-Za-z0-9+_-]"`. (c) Add a flake check asserting scanner fidelity: batch-read lisp/*.el with Emacs's own reader, collect `(use-package NAME)` heads, and diff against `scanDirectory` output — any commented/string/nested occurrence then fails CI.

### 49. Stale planning debris in repo root: plan.md lists shipped work as open, TODO.md lists adopted packages, questions.md is a one-note scratch file

**`plan.md:63`** · layer: docs

**What:** plan.md §5 (63-78) presents the x86_64-darwin 26.05 pin + second nixpkgs input as an open decision — fully implemented (flake.nix:9,56; emacs.nix:52-56). plan.md:84-93 instructs `just bench` (disabled stub, Justfile:136-139) and asserts parity against 'pkgs.emacs31' (line 91) — a different attr than the pkgs.emacs the check compares. plan.md's 'document the devenv-divergence exception in CLAUDE.md' bullet (73-76) was never executed and muddies the real lock desync. TODO.md lists eglot, pdf-tools, and WindMove as 'Investigate' — all long shipped (init-prog.el; init-writing.el:84-87; modules.mdx:64). questions.md is a single answered scratch question. Nothing in CLAUDE.md/AGENTS.md declares the status of journal/, plan.md, TODO.md, questions.md.

**Why it matters:** These sit at repo root where agents read them as current state. plan.md actively contradicts the implemented flake (an agent could re-implement the Darwin pin or trust the pkgs.emacs31 target), and its unexecuted CLAUDE.md-exception note is adjacent to a real, worse lock bug — misdirecting anyone auditing the desync.

**Fix:**

(1) plan.md: move §5 to Done citing flake.nix:9/56 + emacs.nix:52-56, fix line 91 to pkgs.emacs, execute or delete the exception bullet. (2) TODO.md: prune shipped items. (3) Delete questions.md or fold into docs/. (4) Add one line to CLAUDE.md/AGENTS.md declaring journal/, plan.md, TODO.md as 'working notes, may be stale — code wins'.


## Nitpick

### 50. Eager `system` backtick assignment shells out to `nix eval --impure` on every just invocation

**`Justfile:11`** · layer: nix

**What:** `system := \`nix eval --impure --raw --expr 'builtins.currentSystem'\`` (Justfile:11). just evaluates backtick assignments when the justfile loads, so every command — `just fmt`, `just clean`, `just verify`, even `just --list` via the default recipe — pays a nix evaluator startup (hundreds of ms to seconds on a busy store) plus an impure eval, for a value only the build-*/test recipes need.

**Why it matters:** Constant latency tax on all 30+ recipes and an unnecessary dependency on a working nix at parse time for recipes that don't need Nix at all.

**Fix:**

```just
system := arch() + "-" + if os() == "macos" { "darwin" } else { "linux" }
```
(just's arch() returns aarch64/x86_64, matching Nix system names; instant and offline.)

### 51. `just run-built` — the only live launch path — hardwires --debug-init and global debug-on-error into every ordinary session

**`Justfile:234`** · layer: seam

**What:** The run-built recipe launches `./result/bin/emacs --debug-init --eval '(setq debug-on-error t)' --init-directory=...` unconditionally (Justfile:234-236). The historical run/debug split (run plain, debug with these flags — see the disabled originals at Justfile:31-41) collapsed into the debug variant when the recipes were stubbed out.

**Why it matters:** With debug-on-error globally t, any error in a timer, process sentinel, or hook during normal editing pops a *Backtrace* window instead of a message — e.g. the tagref prog-mode hook error and the stale-capf error above become full debugger sessions in what is documented as the plain "launch Emacs with this configuration" path. It also changes behavior under test relative to every deployed mode (HM daemon, NixOS, nix-on-droid), none of which set these flags, so the dev launch is not representative of what ships.

**Fix:**

Justfile only:

```diff
 run-built *ARGS:
     ...
-    ./result/bin/emacs --debug-init \
-        --eval '(setq debug-on-error t)' \
-        --init-directory="{{config_dir}}" {{ARGS}}
+    ./result/bin/emacs --init-directory="{{config_dir}}" {{ARGS}}
+
+# Same, with init debugging enabled.
+[group('build')]
+run-built-debug *ARGS:
+    just run-built --debug-init --eval '(setq debug-on-error t)' {{ARGS}}
```

### 52. Bench harness only times `require` — autoload-driven loads and the network refresh are invisible

**`bench/early-init.el:29`** · layer: elisp

**What:** bench/early-init.el:20-29 advises `require` only. Packages loaded through autoload trampolines (autoload-do-load uses `load`) are untimed at their trigger point: package activation, site-start, and every after-init autoload activation (doom-modeline-mode, global-projection-hook-mode, devenv-env-global-mode, etc.) appear only via transitive requires, misattributed or missing. The package-refresh-contents advice (31-38) can never report: the refresh fires on a 2 s idle timer (init.el:104) while bench/init.el:24,90 kills Emacs at emacs-startup-hook; and the async call returns immediately so any measured span would be dispatch time, not network time.

**Why it matters:** The TIME BUDGET and NETWORK OPERATIONS report sections (bench/init.el:59-69) systematically understate exactly the costs the startup review flags, so optimization decisions made from `just bench` chase the wrong hotspots. (`just bench` is currently a disabled stub anyway.)

**Fix:**

Also advise `load` with a threshold-filtered timing advice pushing `(cons (concat "load:" file) elapsed)` into the results; and instead of kill-emacs at emacs-startup-hook, wait for the refresh window (`(run-with-timer 10 nil ...)`) or time the refresh by advising package--download-one-archive.

[verifier correction] WHAT (unchanged in substance): bench/early-init.el:20-29 advises only `require`, so autoload-triggered loads (the after-init activations doom-modeline-mode, global-projection-hook-mode, devenv-env-global-mode, global-clipetty-mode, global-diff-hl-mode, etc., plus the package-quickstart load and site-start) are untimed at their trigger point and appear only as transitive requires; and the package-refresh-contents advice (lines 31-38) can never report — the refresh sits behind a 2s idle timer whose registering emacs-startup-hook lambda runs after (add-hook prepends) the bench hook that calls kill-emacs, and the call is async anyway. WHY (unchanged): the TIME BUDGET / NETWORK OPERATIONS sections of the report systematically understate these costs; low stakes since `just bench` is currently a disabled stub. FIX (corrected): do NOT rely on advising `load` for the autoload case — `autoload-do-load` is a C primitive that calls Fload C-to-C, and nadvice on primitives is not honored for calls from C (Elisp manual, Advising Functions), so such advice would silently miss exactly the autoload trampolines the finding targets. Instead either (a) intercept loads via a file-name handler for the `load` operation (handlers are consulted inside Fload, so C-initiated loads are seen too), or (b) more simply, time the after-init/emacs-startup hook execution as a block and snapshot `features` before/after to attribute post-init loads, and note the load-advice blind spot in the report header. For the network section: within 7 days of a cached refresh nothing fires at all, so either drop the section, or add an explicit bench mode that calls `(package-refresh-contents)` synchronously (no async flag) under the existing advice so the measured span is real network time; advising package--download-one-archive in async mode still only measures dispatch.

### 53. OpenRouter model list is hand-duplicated verbatim between config/eca/config.json and init-ai.el with no sync check

**`config/eca/config.json:6`** · layer: seam

**What:** config/eca/config.json's providers.openrouter.models block lists exactly the seven model ids (anthropic/claude-opus-4.8, anthropic/claude-sonnet-4.6, openai/gpt-5.5, google/gemini-3.5-flash, deepseek/deepseek-v4-pro, qwen/qwen3.5-35b-a3b, z-ai/glm-4.7) that init-ai.el:134-140 lists for gptel's OpenRouter backend. Two hand-maintained copies of the same provider catalogue, one in JSON installed by module.nix (module.nix:344-349), one in Elisp — and unlike the @doc/package-reference pair, no check keeps them in sync (nix/checks.nix has packages-doc-in-sync for docs drift but nothing for this).

**Why it matters:** The next model bump will land in one file and not the other, and eca and gptel will silently expose different model sets against the same OpenRouter key. The repo already demonstrates (packages-doc-in-sync, checks.nix:139-157) that it considers this class of drift worth a CI gate.

**Fix:**

Nix change: generate config/eca/config.json from a single Nix-level model list, or add a checks.nix derivation that extracts the `:models` list from init-ai.el (same regex-over-elisp approach as nix/use-package.nix) and diffs it against jq output from config.json. Minimal Elisp alternative: a comment in both files cross-referencing the other (`;; keep in sync with config/eca/config.json` / `// keep in sync with lisp/init-ai.el gptel :models`).

### 54. Darwin patches are fetched from the mutable `main` branch of nix-giant/nix-darwin-emacs

**`emacs.nix:336`** · layer: nix

**What:** darwinPatch builds the URL as raw.githubusercontent.com/nix-giant/nix-darwin-emacs/main/overlays/patches-${patchBranch}/${name} (emacs.nix:336-338). Output hashes are pinned (lines 316-330) so it's not impure — but the URL is branch-addressed: any upstream commit to `main` that rewrites/moves/deletes a patch makes the fixed-output fetch fail (hash mismatch or 404) at an arbitrary future date on machines without the store path.

**Why it matters:** The Darwin patch flags will break for new machines/CI whenever upstream touches those files, looking like a hash-mismatch mystery. The file's own header (line 153) anticipates rewrites — pinning a rev makes them a deliberate, diffable bump instead of a time bomb.

**Fix:**

```nix
darwinPatchesRev = "<commit sha of nix-giant/nix-darwin-emacs>";
darwinPatch = name: fetchpatch {
  inherit name;
  url = "https://raw.githubusercontent.com/nix-giant/nix-darwin-emacs/${darwinPatchesRev}/overlays/patches-${patchBranch}/${name}";
  hash = darwinPatchHashes.${patchBranch}.${name};
};
```

[verifier correction] What: darwinPatch (emacs.nix:332-340) fetches Darwin patches from the mutable `main` branch of nix-giant/nix-darwin-emacs. Hashes are pinned (316-330), so builds are reproducible while cached, but any upstream commit that rewrites, moves, or deletes a patch makes the fixed-output fetch fail (hash mismatch, or a 404 with no replacement hash reported) on machines without the store path. Why (corrected): this only affects downstream Darwin consumers who explicitly enable the opt-in patch flags (all default false; nothing in-repo sets them, and CI is x86_64-linux with an isDarwin gate, so CI is never affected). The file's own header (150-153) documents the hash-mismatch-on-rewrite mode as the intended update signal, so this is a known tradeoff rather than a latent mystery — but the 404/moved-file case is not covered by that mitigation, and nixpkgs convention requires immutable fetchpatch URLs. Fix (unchanged, verified safe): pin a nix-giant/nix-darwin-emacs commit sha in the URL (darwinPatchesRev); choosing the current main rev keeps all existing hashes valid, and since the Darwin patch path already runs through overrideAttrs and intentionally busts the cache, the change cannot affect mainline cache parity, nox/droid builds, or `just run`.

### 55. Standalone emacs.nix sets config.allowUnfree = true while the flake's pkgsFor does not — asymmetric eval config across entry points

**`emacs.nix:66`** · layer: nix

**What:** The standalone nix-build path imports nixpkgs with `config.allowUnfree = true` (emacs.nix:66; emacs-jylhis.nix:23 likewise), but flake.nix's pkgsFor/pkgsForLite (57-80) import with no config. Nothing in the closure is unfree today so outputs match, but the entry points evaluate under different nixpkgs policies; an unfree dependency sneaking into an override would make `nix-build emacs.nix` succeed while `nix build .#emacs` fails (or vice versa).

**Why it matters:** Entry-point behavior divergence with no comment explaining why unfree is enabled — a purity wart that will surface as a confusing one-path-only failure.

**Fix:**

Drop `config.allowUnfree = true;` from emacs.nix and emacs-jylhis.nix, or if genuinely needed add the same config to flake.nix's pkgsFor with a comment naming the package requiring it.

### 56. melpa-stable is registered but can never win, and only adds a staleness probe

**`init.el:32`** · layer: elisp

**What:** MELPA-stable is appended to package-archives with no package-archive-priorities (grep: no matches anywhere); package.el picks the highest version across archives, and MELPA's date-based versions always exceed melpa-stable's semvers, so nothing ever installs from it. Its only observable effect: jotain--package-archives-stale-p (init.el:83-95) requires an archives/melpa-stable cache to exist, and the background refresh downloads its index.

**Why it matters:** Dead configuration plus one extra archive index downloaded per refresh cycle for zero benefit.

**Fix:**

Delete line 32, or make it meaningful: `(setopt package-archive-priorities '(("gnu" . 3) ("nongnu" . 2) ("melpa" . 1)))`.

[verifier correction] What: melpa-stable is appended to package-archives (init.el:32) with no package-archive-priorities or pins anywhere; package.el orders candidates by (priority . version) and MELPA's date-based versions always exceed melpa-stable's semvers, so nothing ever auto-installs from it. Its only observable effect is negative: jotain--package-archives-stale-p (init.el:83-95) requires an archives/melpa-stable cache to exist, and the background refresh downloads its index. Why: dead configuration plus one extra archive index download per refresh cycle for zero benefit; init.el's own commentary ("Register MELPA as a fallback archive") and CLAUDE.md never mention melpa-stable as intentional. Fix: delete init.el:32 (and drop the matching mention in docs/configuration/init.mdx:22 and docs/configuration/packages.mdx:18). Do NOT adopt the finding's alternative of setting package-archive-priorities '(("gnu" . 3) ("nongnu" . 2) ("melpa" . 1)): it leaves melpa-stable at priority 0 (still never used), and since priority outranks version it would silently prefer older GNU/NonGNU ELPA releases over MELPA snapshots for every dual-hosted package.

### 57. `(use-package mcp :defer t :after gptel)` is dead config — the :after chain never loads anything

**`lisp/init-ai.el:170`** · layer: elisp

**What:** init-ai.el:170-172: with :defer t, no :bind/:commands/:hook/:config and no :demand, use-package emits no require for mcp at all; `:after gptel` merely gates an empty body. Loading gptel does not load mcp, contradicting the block's own @doc ('Loaded after gptel').

**Why it matters:** No runtime harm (mcp's package autoloads make M-x mcp-connect-server work), but the block documents behavior that doesn't exist, and the misconception could propagate (e.g. assuming gptel-mcp integration is armed after C-c RET).

**Fix:**

Either make it real (`:after gptel :demand t`) or honest (`:defer t ;; loaded on demand by devenv-mcp-setup / M-x mcp-connect-server`).

### 58. Idle GC timer defeats the minibuffer GC pause, and the 16 MiB literal is duplicated

**`lisp/init-core.el:40`** · layer: elisp

**What:** init-core.el:40 `(run-with-idle-timer 5 t #'garbage-collect)` calls garbage-collect explicitly, which ignores gc-cons-threshold — so idling >=5 s while a minibuffer is open runs a full GC anyway, contradicting the minibuffer-setup-hook bump to most-positive-fixnum (45-46) whose stated purpose is pausing GC while the minibuffer is open. The exit hook (48) also hardcodes `(* 16 1024 1024)` a second time.

**Why it matters:** A full GC on a large heap is exactly the perceptible pause the minibuffer hooks try to prevent, landing mid-completion. Occasional and bounded — hence nitpick.

**Fix:**

```elisp
(run-with-idle-timer 5 t (lambda ()
                           (unless (active-minibuffer-window)
                             (garbage-collect))))
```
and replace both 16 MiB literals with the jotain-core-gc-cons-threshold constant from the gc-restore fix.

### 59. Dead capability guards for features at or below the Emacs 30 floor

**`lisp/init-core.el:349`** · layer: elisp

**What:** init-core.el:349 wraps `minibuffer-regexp-mode` in `(when (fboundp ...))` with a comment 'Built-in since Emacs 30; no-op guard keeps older Emacs happy', and init-prog.el:323 guards `eglot-inlay-hints-mode` with fboundp (shipped since Eglot 1.9/Emacs 29). The floor is Emacs 30.1 (init.el:5 Package-Requires).

**Why it matters:** The repo's own elisp-dev digest states: don't guard for versions at or below the Emacs 30 floor — such guards are dead and misleading. They imply 29- support, muddying the 30/31 version-gating discipline the legitimate 31 guards depend on.

**Fix:**

Drop the fboundp guards: call `(minibuffer-regexp-mode 1)` directly (comment: 'Built-in since Emacs 30') and in init-prog.el:323 keep only the add-hook.

### 60. diminish is invisible under doom-modeline and superseded on Emacs 31 by mode-line-collapse-minor-modes

**`lisp/init-core.el:127`** · layer: elisp

**What:** init-core.el:127-128 demand-loads third-party `diminish` solely so ~12 :diminish keywords expand. But the modeline is doom-modeline (init-ui.el:73-81), whose minor-modes segment is off by default (no doom-modeline-minor-modes override anywhere — grep), so diminished lighters are never rendered. Emacs 31 adds the built-in mode-line-collapse-minor-modes covering the vanilla-modeline case (NEWS.31 ~614).

**Why it matters:** A MELPA runtime dependency (installed in mode (a), scanned into the Nix closure in mode (b)) whose entire effect is invisible in this config, and whose remaining use case is now core in the targeted Emacs version. Removing it shrinks both provisioning paths and drops one always-:demand startup load.

**Fix:**

Delete the diminish block (init-core.el:124-128) and every :diminish keyword across the modules. For vanilla-modeline fallback on 31: `(when (boundp 'mode-line-collapse-minor-modes) (setopt mode-line-collapse-minor-modes t))` in init-ui.el.

### 61. init-devenv @doc still claims devenv-reload works through envrc, contradicting the config's own 'envrc intentionally not enabled' design

**`lisp/init-devenv.el:36`** · layer: docs

**What:** The `;;; @doc` block at init-devenv.el:36-37 says '`devenv-reload` refreshes the project environment through envrc (the direnv substrate configured in init-prog)' — but init-prog.el:536-541 states envrc is intentionally NOT enabled, the file's own Commentary (init-devenv.el:15-17) says direnv/envrc is disabled, and devenv.el:1124-1129 implements the refresh natively (devenv-env-global-mode, devenv-env-defer-to-direnv nil) with no envrc involvement.

**Why it matters:** @doc blocks are harvested into the generated package reference (docs/configuration/package-reference.mdx via the packages-doc pipeline), so the published documentation contradicts the code in the same file.

**Fix:**

```elisp
;;; `devenv-reload` re-runs `devenv print-dev-env' and re-applies the
;;; environment buffer-locally via the native loader
;;; (`devenv-env-global-mode'), and offers to reconnect eglot servers.
```
then `just docs-refresh-packages` to regenerate the .mdx (Justfile:290-294).

### 62. Sweep of remaining avoidable eager loads: super-save, pulsar, kkp, corfu/cape/marginalia :demand, keyfreq

**`lisp/init-editing.el:146`** · layer: elisp

**What:** Third-party blocks loading during init without needing to: super-save (init-editing.el:146-154, :config super-save-mode); pulsar (init-ui.el:299-309, :demand t — pulses only on user commands); kkp (init-terminal.el:50-52, :config global-kkp-mode — its own @doc says no-op in GUI frames, yet loaded in GUI-only sessions); corfu (init-completion.el:273-280, :demand t); cape (init-completion.el:298-316, :demand t — its add-hooks reference autoloaded capfs, so the package need not be loaded); marginalia (init-completion.el:106-108, :demand t — defensible but after-init works identically); keyfreq (init-tracking.el:15-23, defensible). Built-in eager enables are individually cheap and reasonable to keep.

**Why it matters:** Individually small (2-15 ms each, estimates), collectively an estimated 30-80 ms of init eliminable with `:hook (after-init . …-mode)` or dropping :demand, with no behavior change — the stragglers in a config that otherwise carefully defers doom-modeline/tab-bar/projection/devenv.

**Fix:**

Apply the same pattern to each: `:hook (after-init . super-save-mode)` / `pulsar-global-mode` / `global-kkp-mode` (or gate kkp on tty/daemonp) / `global-corfu-mode`; cape: delete `:demand t` — the add-hook'd capfs (cape-dabbrev, cape-file, cape-keyword, cape-elisp-symbol) are autoloaded, so the :init hooks keep working and cape loads on first completion.

[verifier correction] Several third-party blocks load eagerly during init while sibling packages defer to after-init: super-save (init-editing.el:146-154), pulsar (init-ui.el:299-309), kkp (init-terminal.el:50-52), corfu (init-completion.el:273-280), cape (init-completion.el:298-316), marginalia (init-completion.el:106-108), keyfreq (init-tracking.el:15-23). Note the `:demand t` in the pulsar/corfu/cape/marginalia blocks is redundant — those blocks contain no deferring keyword, so they are eager with or without it. WHY (corrected): this is primarily a consistency cleanup, not a startup-time win — after-init-hook runs synchronously before the first screen is painted (startup.el 30.2: after-init-hook at line 1580 precedes command-line-1 at 1607 and window-setup-hook at 2924), so `:hook (after-init . …)` conversions change `emacs-init-time` bookkeeping but not perceived startup. Real work elimination exists only for cape (defer until first completion) and kkp in pure-GUI sessions. FIX (corrected): for consistency, `:hook (after-init . super-save-mode / pulsar-global-mode / global-corfu-mode)` are safe (globalized modes apply to existing buffers). For cape, deleting `:demand t` is NOT sufficient — the block has no deferring keyword and use-package-always-defer is unset, so add explicit `:defer t`; the :init add-hook'd capfs (cape-dabbrev/file/keyword/elisp-symbol) are autoloaded, so cape then loads on first completion-at-point. For kkp, `:hook (after-init . global-kkp-mode)` still loads it in GUI; to actually skip GUI-only sessions gate on `(or (daemonp) (not (display-graphic-p)))` — the daemonp arm is required so a GUI-started daemon still gets KKP on later `emacsclient -nw` frames. Keyfreq and marginalia: leave as-is (the finding itself calls them defensible).

### 63. jotain-window-resize-repeat-map re-implements the built-in resize-window-repeat-map; comment claims a gap that doesn't exist

**`lisp/init-keys.el:143`** · layer: elisp

**What:** The comment (init-keys.el:138-142) says these commands lack a repeat map 'out of the box', but window.el has shipped resize-window-repeat-map (with ^, }, { on the same commands) since Emacs 28. `defvar-keymap :repeat t` overwrites each command's repeat-map property, silently replacing the built-in map; the only net addition is `v` → shrink-window.

**Why it matters:** Misleading rationale plus duplicated built-in behavior — one keymap-set on the existing map achieves the same and keeps future upstream additions.

**Fix:**

```elisp
(keymap-set resize-window-repeat-map "v" #'shrink-window)
```
(window.el is preloaded, so no with-eval-after-load needed.)

[verifier correction] What: init-keys.el:138-149 defines jotain-window-resize-repeat-map with a comment claiming these window-resize commands lack a repeat map "out of the box". False: Emacs 28+ ships resize-window-repeat-map, and in Emacs 30 (the version this repo targets and its Nix layer builds) that built-in map already contains all four bindings including v→shrink-window. defvar-keymap :repeat t overwrites each command's repeat-map symbol property (verified empirically on the repo's Emacs 30.2), so the custom map silently replaces a built-in it exactly duplicates — zero net additions. Why: misleading comment plus dead-weight duplication that would also mask future upstream additions to the built-in map. Fix: delete the comment and defvar-keymap block (lines 138-149) entirely — behavior is identical afterward; window.el is preloaded on every build variant (pgtk/nox/macport/droid), so nothing else changes. (The originally proposed keymap-set of "v" is a no-op on Emacs 30 and unnecessary.)

### 64. init-keys commentary sanctions `:general` though general.el is not in the config; which-key label table omits C-c m

**`lisp/init-keys.el:8`** · layer: elisp

**What:** The header convention at init-keys.el:7-8 says per-package bindings may use ':bind, :bind-keymap, or :general', but nothing uses :general and general.el is not installed (grep: only this comment, the name-drop at init-keys.el:85, and GPL boilerplate in devenv.el) — with use-package-always-ensure t, following the guidance would silently pull general.el from MELPA in mode (a) and fall on the scanner in mode (b). Separately, the which-key label table (96-125) documents C-c h/i/k but omits sibling C-c m (consult-man, init-completion.el:125).

**Why it matters:** The comment is the repo's stated keybinding policy; recommending a keyword that only works by dragging in an undeclared third-party dependency contradicts the deliberate 'general.el without the dependency' stance three lines below. The missing label is a small discoverability gap in a table whose purpose is C-c namespace completeness.

**Fix:**

Change the comment to '`:bind' or `:bind-keymap'' and add `"C-c m" "consult-man"` to the label table.

[verifier correction] what: The header convention at init-keys.el:7-8 says per-package bindings may use ':bind, :bind-keymap, or :general', but general.el is not part of this config (grep: only this comment, the deliberate "without the dependency" remark at init-keys.el:85, and GPL boilerplate in devenv.el). `:general` is only a valid use-package keyword when general.el is loaded, so a contributor following the header would hit a hard "Unrecognized keyword: :general" use-package-error at expansion time, failing `just compile`/CI (byte-compile-error-on-warn) — use-package-always-ensure would NOT install general.el, since it only ensures the block's own named package. Separately, the which-key label table (init-keys.el:96-125) documents the sibling consult bindings C-c h/i/k (and C-c M-x) but omits C-c m (consult-man, init-completion.el:125) — the only global C-c binding missing from an otherwise complete table. why: The comment is the repo's stated keybinding policy and contradicts the explicit no-general.el stance 77 lines below; the missing label is a small discoverability gap. fix: Change the comment to "`:bind' or `:bind-keymap'" and add `"C-c m" "consult-man"` to the label table — a comment/string-only change, safe in both nix-build and `just run` modes and on all platform variants.

### 65. setq on the defcustom dockerfile-mode-command violates the repo's setopt rule

**`lisp/init-lang-devops.el:28`** · layer: elisp

**What:** jotain--apply-docker-backend does `(setq dockerfile-mode-command cmd)` on dockerfile-mode's defcustom. The repo's hard rule (CLAUDE.md / elisp-dev skill) is setopt for defcustoms.

**Why it matters:** Consistency only today (no :set callback), but the rule exists precisely so a future upstream :set/:type addition isn't silently bypassed.

**Fix:**

```elisp
    (when (boundp 'dockerfile-mode-command)
      (setopt dockerfile-mode-command cmd))
```

### 66. go-ts-mode-indent-offset is renamed to go-ts-indent-offset in Emacs 31

**`lisp/init-lang-go.el:64`** · layer: elisp

**What:** :custom sets `(go-ts-mode-indent-offset 8)`. NEWS.31's 'FOO-ts-mode-indent-offset renamed to FOO-ts-indent-offset' list includes go-ts-indent-offset (with toml/html/typescript/json/java/csharp/cmake/c). rust-ts-mode-indent-offset (init-lang-rust.el:17) is NOT in the rename list and stays as-is.

**Why it matters:** On 31 the old name is reachable only through the compat alias; when the floor moves this becomes obsolete-name debt, and if the alias is dropped the :custom silently sets a dead variable and Go indentation reverts to default.

**Fix:**

Leave a tracking comment now ('Renamed to go-ts-indent-offset in Emacs 31; flip when the floor moves') and rename atomically then, or belt-and-suspenders today: `:config (when (boundp 'go-ts-indent-offset) (setopt go-ts-indent-offset 8))`.

[verifier correction] WHAT: init-lang-go.el:64 sets `(go-ts-mode-indent-offset 8)` via :custom. Emacs 31 renames this to `go-ts-indent-offset` (NEWS.31 ~4136-4145; go is in the list, rust-ts-mode-indent-offset in init-lang-rust.el:17 is NOT renamed) and keeps a define-obsolete-variable-alias, verified in emacs-31 go-ts-mode.el:65-66, so the setting keeps working on 31. WHY: pure obsolete-name debt for when the repo's floor moves to 31 — no behavioral risk even if the alias were later dropped, because the default is 8 in both Emacs 30 and 31 (the repo's own comment calls it "the default"), so the :custom is documentation of the default value; and since :custom/setopt reference the symbol quoted, the alias never triggers the byte-compile obsolete-variable warning under the warnings-as-errors gate. FIX: add a tracking comment now ("Renamed to go-ts-indent-offset in Emacs 31; rename when the floor moves") and rename atomically then; the belt-and-suspenders `(when (boundp 'go-ts-indent-offset) ...)` guard is harmless but unnecessary given the alias and identical default.

### 67. apheleia loads eagerly at startup; format-on-save only matters at first save

**`lisp/init-prog.el:547`** · layer: elisp

**What:** init-prog.el:547-573: the apheleia block has no deferral keyword, so apheleia (and apheleia-formatters' large alist) loads during init, then `apheleia-global-mode 1` runs in :config. Format-on-save cannot fire before a buffer is saved; the global mode just needs to be active before the first save.

**Why it matters:** Unneeded pre-first-frame load, estimated 10-40 ms (estimate). Trivially deferrable — apheleia-global-mode is autoloaded.

**Fix:**

Change to `:hook (after-init . apheleia-global-mode)`, keep the add-to-list forms and safe-local-variable in :config, drop the explicit `(apheleia-global-mode 1)`.

[verifier correction] init-prog.el:547 — WHAT: the apheleia block does load eagerly during init (no deferral keyword; apheleia-global-mode 1 at line 572), which is unidiomatic next to the deferred neighbors (wgrep :defer t). WHY (corrected): the practical cost is near-nil and the proposed remedy recovers none of it — after-init-hook runs synchronously before command-line files are visited and before the frame is interactive, so `:hook (after-init . apheleia-global-mode)` moves the same load a few steps later in the same blocking sequence; only the emacs-init-time metric improves. FIX (optional, cosmetic): the proposed `:hook (after-init . apheleia-global-mode)` with add-to-list/safe-local-variable forms kept in :config is safe and slightly more idiomatic (and lets :functions be dropped), but should not be sold as a startup-time win; a real win would require deferring the load to first file visit/save, which conflicts with the global format-on-save design for single-digit-ms gains.

### 68. C/C++ missing from the tree-sitter remap table while the rest of the config assumes c-ts modes; the gap-detector spams every C file

**`lisp/init-prog.el:61`** · layer: elisp

**What:** jotain-prog-ts-remaps (init-prog.el:61-76) covers 11 languages but not C/C++, and init-lang-systems.el:36-43 pins headers/sources to classic c++-mode. Yet init-prog.el:330-333 arms eglot inlay hints for `c-ts-mode c++-ts-mode` and init-project.el:592 keys compile-multi on them — modes no buffer can ever reach. Meanwhile the non-ts diagnostic (124-141) derives lang 'c from c-mode, treesit-ready-p succeeds (tree-sitter-c ships in both grammar sets, mk-overlay.nix:48), so every C buffer logs 'opened in c-mode; c-ts-mode ... is available' on every visit. C++ escapes only by accident: the heuristic interns `c++` but the grammar symbol is `cpp` (line 136).

**Why it matters:** Inconsistent state: either cc-mode is deliberate (then the detector nags forever and the c-ts wiring is dead) or an oversight (then C/C++ buffers miss treesit font-lock/indent, treesit-fold, combobulate, and the 30/31 c-ts improvements). Emacs 31 also renames c-ts-mode-indent-offset → c-ts-indent-offset, so the migration only gets more coupled.

**Fix:**

If moving to treesit: add `(c c-ts-mode c-mode)` and `(cpp c++-ts-mode c++-mode)` to the table (major-mode-remap-alist redirects; move c-basic-offset equivalents to c-ts-mode-indent-offset/-style in init-lang-systems.el). If cc-mode is deliberate: say so and add a `jotain-prog-warn-non-ts-exclude` list (containing c-mode/c++-mode) checked in jotain-prog--warn-non-ts-mode so the diagnostic stays trustworthy.

[verifier correction] What: jotain-prog-ts-remaps (init-prog.el:61-76) omits C/C++ while init-lang-systems.el:32-38 pins buffers to classic cc-mode, so in grammar-shipping builds the gap detector (init-prog.el:124-141) logs "opened in c-mode; c-ts-mode ... is available" on every C buffer visit — forever, since the gap is never closed — while C++ escapes only by accident: line 136 interns `c++` but c++-ts-mode's grammar symbol is `cpp`, making the detector silently blind to C++. The repo is also internally inconsistent about intent: nix/mk-overlay.nix:40-44 says the curated grammar set contains only grammars routed by the remap table / :mode entries / combobulate, yet includes tree-sitter-c and tree-sitter-cpp, which none of those route. Note this costs no functionality: eglot inlay hints (init-prog.el:333) and compile-multi (init-project.el:167, not 592) list classic c-mode/c++-mode alongside the ts variants, so C/C++ buffers get both features today; only treesit font-lock/indent and treesit-fold are missed, and only if migration was intended. Why: the diagnostic is noisy for C and structurally unable to flag C++, and the stale mk-overlay comment obscures whether cc-mode is deliberate. Fix (either): (a) commit to treesit — add (c c-ts-mode c-mode) and (cpp c++-ts-mode c++-mode) to the table (treesit-ready-p guard keeps grammar-less builds — bare devenv Emacs, host Emacs via `just run` — safely on cc-mode) and migrate c-basic-offset/c-default-style to c-ts-mode-indent-offset/-style in init-lang-systems.el; or (b) keep cc-mode deliberately — add an exclusion list checked by jotain-prog--warn-non-ts-mode, document the choice, and fix the mk-overlay.nix:40-44 comment (or drop tree-sitter-c/cpp from the curated set). Given the invested cc-mode style config, (b) is the lower-risk fix.

### 69. with-eval-after-load 'project inside projection's :config is a redundant self-require

**`lisp/init-project.el:81`** · layer: elisp

**What:** init-project.el:81-82: inside projection's :config — which only runs once projection is already loaded — `(with-eval-after-load 'project (require 'projection))` re-requires the package that is by definition already in `features`. It also cannot serve its apparent purpose (load projection when project.el loads) because :config hasn't run until projection loads by some other path (the after-init hook at line 78).

**Why it matters:** Dead code that reads as load-order machinery but is a no-op; confuses future reasoning about when projection loads (answer: at after-init via global-projection-hook-mode's autoload).

**Fix:**

Delete lines 81-82. If eager-on-project-load was actually intended it belongs in :init — but the after-init hook already loads it earlier in practice, so plain deletion is right.

### 70. Global C-x C-j binding shadows the Emacs 28+ default dired-jump

**`lisp/init-vc.el:119`** · layer: elisp

**What:** `use-package vc-jj :bind ("C-x C-j" . jotain-switch-jj-status-buffer)` rebinds C-x C-j globally. Since Emacs 28, C-x C-j is dired-jump out of the box (no dired-x load needed), and nothing else in lisp/ rebinds dired-jump (grep: no matches) — despite the config investing heavily in dired/dirvish. The shadowing is undocumented (the which-key table in init-keys.el:95-125 omits it) and internally inconsistent: the sibling git helper is on C-x C-g while the jj ecosystem's other entry points live under C-c j / C-c M-j (init-vc.el:182-183).

**Why it matters:** The universal 'open dired at this file' gesture vanishes with no replacement; muscle memory instead opens a jj status prompt that errors 'Not inside a Jujutsu repository' in plain git repos. Silent loss of a stock binding contradicts the file's 'reviewable at a glance' keymap policy, and C-h k still shows the jj command so users must discover M-x dired-jump.

**Fix:**

Rebind, e.g. `("C-x J" . jotain-switch-jj-status-buffer)` or fold under the jj letter as a small C-c j prefix map (C-c j j majutsu-log, C-c j s status), updating the which-key labels at init-keys.el:106/116. dired-jump's default is restored for free.

[verifier correction] WHAT: `use-package vc-jj :bind ("C-x C-j" . jotain-switch-jj-status-buffer)` (init-vc.el:119) globally shadows dired-jump, which since Emacs 28 is bound to C-x C-j out of the box — verified on the repo's Emacs 30.2: it is dired-jump's only default binding, so after this rebind dired-jump has no key at all, in a config that configures dired/dirvish extensively (init-navigation.el). The binding itself is deliberate and documented (init-vc.el:113 calls it the "jj twin" of C-x C-g, mirroring the git/jj pairing), but the loss of dired-jump is acknowledged nowhere (not in the comment, CLAUDE.md, the which-key table at init-keys.el:95-125, or docs). In a plain git repo the key just messages "Not inside a Jujutsu repository." WHY: a stock gesture silently loses its only binding as an unexamined side effect; C-c d (dirvish) partially covers the gesture but does not place point on the current file. FIX: either document the tradeoff where the binding is defined, or move the jj status jump to a free key (e.g. C-x J — unbound in stock Emacs 30 and in this config) so the dired autoload's ctl-x-map binding is restored for free; update the which-key table/docs either way.

### 71. smerge :bind block re-states smerge-mode's own defaults verbatim

**`lisp/init-vc.el:571`** · layer: elisp

**What:** init-vc.el:571-575 binds, in smerge-mode-map, C-c ^ u/l/n/p to smerge-keep-upper/-keep-lower/-next/-prev — exactly what smerge-mode already provides via smerge-command-prefix (default C-c ^) and smerge-basic-map. The @doc comment presents the stock default as a customization ('Custom prefix C-c ^').

**Why it matters:** Pure duplication that misleads readers about what is custom vs stock, and pins the C-c ^ prefix in Elisp so a user customizing smerge-command-prefix would get the commands on both the old and new prefix.

**Fix:**

Drop the :bind block (keep any :custom/:hook content), optionally moving the key list into the @doc comment: `;; (defaults: C-c ^ u/l/n/p already bound by smerge-mode)`.

[verifier correction] What/why: unchanged from the original finding — init-vc.el:571-575 restates smerge-mode's stock C-c ^ u/l/n/p bindings verbatim and the @doc comment presents the default prefix as a customization; it also pins C-c ^ so a user customizing smerge-command-prefix before load gets the commands on both prefixes. Fix (corrected): since :bind is the block's only keyword and use-package-always-defer is not set, simply dropping :bind leaves a degenerate block that eagerly loads smerge-mode at startup. Instead, delete the entire (use-package smerge-mode ...) form and its @doc comment (smerge-mode activates itself on conflict detection), or if a placeholder is wanted for documentation, keep (use-package smerge-mode :ensure nil :defer t) with a comment noting the defaults: ;; defaults: C-c ^ u/l/n/p already bound by smerge-mode.

### 72. Owner-machine path defaults (~/Documents/notes, ~/Developer, ~/Projects, ~/Documents) baked into a config shipped to all module consumers

**`lisp/init-writing.el:78`** · layer: elisp

**What:** denote-directory `~/Documents/notes/` (init-writing.el:78), magit-repository-directories `(("~/Developer" . 2))` (init-vc.el:168), org-directory `~/Documents` (init-org.el:23), and jotain-projects-directories `~/Projects ~/code ~/src` (init-project.el:24-26) encode the author's layout; lisp/ is installed verbatim for every HM/NixOS/droid consumer (module.nix:338-343). All fail soft, and jotain-projects-directories is at least a defcustom — but the magit/denote/org values are :custom literals with no jotain-level knob, and org-directory=~/Documents means org-capture drops .org files loose into the user's Documents folder.

**Why it matters:** Small costs (surprise file placement, empty pickers), but it's the one place the 'no host variation except env vars' design leaks the author's laptop into everyone's config.

**Fix:**

Lift the four into jotain- defcustoms next to jotain-projects-directories (e.g. jotain-notes-directory, jotain-repositories-root) referenced from the :custom blocks, or guard the magit/denote entries with file-directory-p fallbacks to package defaults. No Nix change.

### 73. HM wrapper ships direnv for an envrc integration the config removed, and omits devenv, which the config's env loader actually shells out to

**`module.nix:85`** · layer: nix

**What:** module.nix:85 includes `direnv # envrc` in runtimeDeps. But init-prog.el:536-541 states envrc is intentionally not enabled; init-devenv.el:56-64 enables `devenv-env-global-mode` at after-init with `devenv-env-defer-to-direnv` nil. devenv.el resolves the `devenv` executable (devenv.el:167) — which is NOT in runtimeDeps.

**Why it matters:** Under the HM daemon (especially launchd on macOS, the exact case the runtimeDeps comment cites), `devenv` is only found via the user's login-shell PATH; otherwise every project buffer shows devenv[!]/no env and the per-project LSP toolchains (jotain-prog--maybe-eglot-ensure, init-prog.el:208-241) never activate. Meanwhile direnv is dead weight no Elisp path invokes.

**Fix:**

```diff
-      direnv # envrc
+      devenv # devenv-env-global-mode (lisp/devenv.el) — native env loader
```
No Elisp change (devenv.el degrades cleanly, init-devenv.el:45-46).

[verifier correction] module.nix:85 ships `direnv # envrc` in runtimeDeps, but the envrc integration was removed (init-prog.el:536-541: "envrc is intentionally not enabled here"; no `use-package envrc` remains) and no Elisp path invokes direnv — it is dead closure weight with a stale comment, matched by stale comments at init-core.el:276 ("rg, fd, git, direnv, coreutils") and module.nix:74-77 ("outside of envrc-managed project buffers"). Meanwhile `devenv`, which the enabled-by-default `devenv-env-global-mode` loader (init-devenv.el:56-64) shells out to, is not in runtimeDeps. In practice devenv is still found in most daemon setups because exec-path-from-shell runs under `(daemonp)` (init-core.el:278-286) and is the documented mechanism for user toolchains; the loader also degrades cleanly (devenv[!] modeline, executable-find gate at devenv.el:1165). The residual gap is launchd/systemd setups whose shells don't export PATH non-interactively (`exec-path-from-shell-arguments nil`). Fix: drop the `direnv # envrc` line and update the two stale comments; if devenv should be guaranteed on the daemon PATH, add it as an opt-in option like `cfg.sonarlint.enable` rather than unconditionally, since `pkgs.devenv` drags its bundled nix into the closure and can version-skew against per-project devenv installs.

### 74. sonarlintLs overlay attr is dead code, and the INFOPATH wrapper comment says 'prepend' while the code appends

**`nix/mk-overlay.nix:134`** · layer: nix

**What:** (1) `sonarlintLs = final.sonarlint-ls;` (mk-overlay.nix:134) has zero consumers — module.nix:90 uses pkgs.sonarlint-ls directly; only CLAUDE.md:115 still advertises the attr (deadnix can't flag unused attrset members). (2) The wrapper comment (mk-overlay.nix:152-153, 'prepend ${jotainInfo}/share/info to $INFOPATH') and CLAUDE.md say prepend, but the code uses `--suffix INFOPATH : ...` (mk-overlay.nix:110), which appends — a user's own INFOPATH entries shadow jotain.info on name collisions.

**Why it matters:** Dead overlay surface invites drift (it already drifted into CLAUDE.md), and the append/prepend mismatch means the documented precedence is not what ships — confusing when someone debugs Info resolution.

**Fix:**

Remove `sonarlintLs = final.sonarlint-ls;`, and either fix the comment to 'append' or match the docs: `--prefix INFOPATH : "${final.jotainInfo}/share/info"` (if prepending, ensure the final value ends with ':' so Emacs appends Info-default-directory-list).

[verifier correction] What: the wrapper comment in nix/mk-overlay.nix:153 (and the matching CLAUDE.md sentence) says the re-wrap 'prepends' ${jotainInfo}/share/info to $INFOPATH, but the code (mk-overlay.nix:110) uses `--suffix INFOPATH : "${final.jotainInfo}/share/info:"`, which appends. Why: one-word doc inaccuracy that could mislead someone debugging Info resolution; the code itself is correct — appending with a trailing ':' guarantees the final INFOPATH ends with the path separator, which is what makes Emacs's info-initialize append Info-default-directory-list so built-in manuals stay visible (verified in info.el of the shipped Emacs 30.2). Fix: change 'prepend' to 'append' in the mk-overlay.nix comment and in CLAUDE.md; do NOT switch to --prefix (a user-set INFOPATH without a trailing colon would then hide all built-in Emacs manuals). Drop the sonarlintLs removal: it is a documented public attr of overlays.default for downstream consumers (CLAUDE.md lists it alongside eca/jotainInfo), not dead code.


## Keep doing this

- devenv.el's trust model is genuinely security-conscious: the native env loader honours devenv 2.1's `devenv allow` trust database via `hook-should-activate` (devenv.el:393-436), so Emacs never auto-evaluates an untrusted devenv.nix — the same contract the shell hook uses, with a clean state machine (allowed/blocked/no-project/unsupported) that degrades correctly on devenv 1.x (devenv.el:404-417).
- test/devenv-test.el is a model in-repo test suite: ~25 batch-safe ERT tests covering command construction, cache TTL/invalidation, JSON and plain-table parsing, eglot contact routing (including functional fallbacks and :language-id keys), trust-state mapping, and buffer-local env application with a global-untouched assertion (test/devenv-test.el:142-153) — all pure, no subprocess, and wired into CI as the elisp-test flake check (nix/checks.nix:319-331).
- The devenv↔eglot async-environment race is handled with real care on both sides of the seam: devenv-env--around-eglot-ensure defers eglot-ensure while the env fetch is in flight and replays it per-buffer when pairs land (devenv.el:1084-1116, 1184-1201), and init-prog's auto-start defers to an idle timer and explicitly routes through the deferral when devenv-env-loading-p (init-prog.el:208-241) — with a comment documenting why the naive per-mode eglot-ensure hooks were removed (init-prog.el:278-285).
- Anti-drift automation for docs: the packages-doc-in-sync check byte-diffs the checked-in Mintlify page against the freshly generated one from the ;;; @doc markers, with an actionable failure message pointing at `just docs-refresh-packages` (nix/checks.nix:139-157) — documentation drift is a CI failure, not a hope.
- The emacs-binaries check asserts store purity properly: it runs the built Emacs under a sandboxed HOME, greps stdout/stderr for /home/ and /Users/ leaks, and fails if the binary created ~/.emacs.d (nix/checks.nix:207-239) — an isolation regression test most Nix Emacs setups never write.
- All four deployment modules are evaluated in CI without their host frameworks: a stub HM module system exercises default, graphical, and jylhis-backend configs, and a stub nix-on-droid module asserts EDITOR wiring and package installation, all on x86_64 (nix/checks.nix:13-118) — module regressions surface at `nix flake check` time.
- module.nix's wrapper architecture is coherent end-to-end: one emacsWrapper pins --init-directory and prepends the runtime-deps PATH (module.nix:98-106), and the daemon unit, EDITOR/VISUAL scripts, desktop entry, shell aliases, and the jotctl cross-platform daemon controller (launchctl vs systemctl, module.nix:140-169) all reuse it, so no launch path can pick up a stray ~/.emacs.d or miss the tool PATH.
- Version-gated Emacs 31 adoption is done the right way throughout: early-init pre-seeds the non-preloaded `native-comp-async-on-battery-power` via defvar with a precise comment explaining why a boundp guard would silently no-op on every version (early-init.el:96-117), and the boundp/fboundp guards in init-help.el:22-23, init-navigation.el:46-48, init-shell.el:43-45, and init-prog.el:313-320 keep the config loadable on 30 while lighting up on 31.
- Security-motivated opt-in for config-executing LSP servers: ESLint and Tailwind servers — which evaluate project-controlled JavaScript — are excluded from `rass` sessions unless jotain-prog-enable-risky-js-lsp is set, with the threat model stated in the defcustom docstring (init-prog.el:169-176, 243-253).
- The AI feedback loop is a real engineering idea, correctly guarded: jotain-screenshot capability-checks display-graphic-p and x-export-frames per build variant (init-ai.el:46-71), is exposed to Claude sessions as an emacs_screenshot MCP tool with an fboundp guard against upstream API renames (init-ai.el:88-97), and gets a headless Xvfb-driven CI-able exercise path in the `just screenshot` recipe (Justfile:243-263).

## Claims refuted during verification

Plausible-looking defects that did **not** survive adversarial re-reading — listed so they aren't re-reported later:

- **Missing epkgs attrs fall through with only a trace warning — a scanned package silently degrades from Nix provisioning to a first-startup MELPA install, and no check catches it** — Re-read nix/use-package.nix:305-335 (mechanism accurately described), nix/mk-overlay.nix, nix/checks.nix, nix/packages-doc.nix, nix/extra-packages.nix, init.el, and lisp/init-terminal.el/init-ai.el, then ran the verification the reviewer could not: (1) Both named candidates resolve — "ghostel" and "eca" are present in the MELPA recipes archive of a locally available nixpkgs 26.11pre snapshot contemporaneous with the pinned rev, and a full nix-instantiate evaluation of all scanned names against e
- **ghostel has zero Nix wiring — the comment's claim that 'the Nix package builds the module from source' is backed by no code** — Re-read lisp/init-terminal.el:24-42, nix/extra-packages.nix, nix/mk-overlay.nix, and nix/use-package.nix — the reviewer's description of the repo-local code is accurate (no ghostel entry anywhere in nix/, and toEmacsPackage does trace-warn-and-drop on a missing attr). But the finding's load-bearing claim — explicitly flagged by the reviewer as an unverified inference — is false, and I verified it against the actual pinned nixpkgs (flake.lock rev, store path /nix/store/gfdrr201ykw9fpajw2r3payhaad
- **exec-path-from-shell-initialize can demote or drop the module.nix wrapper's runtime PATH in daemon mode** — Re-read /home/user/jotain/lisp/init-core.el:273-286, /home/user/jotain/module.nix:74-130, 368-422, and verified exec-path-from-shell's actual invocation semantics against the package source (fetched purcell/exec-path-from-shell master) rather than memory. The finding's mechanism cannot occur as claimed. (1) The claim rests on "exec-path-from-shell replaces PATH with a fresh *login* shell's report; on macOS path_helper and user zprofiles rebuild PATH." But init-core.el:284 sets `exec-path-from-sh
- **auto-dark `:demand t` probes system appearance (osascript/D-Bus) before the first frame** — Re-read lisp/init-ui.el:43-66, init.el's module order (init-ui is 3rd of ~30 requires), and the actual upstream auto-dark.el source (elpa/ is absent in this checkout, so I fetched LionyxML/auto-dark-emacs auto-dark.el and read auto-dark--determine-detection-method, auto-dark--check-and-set-dark-mode, auto-dark-mode). Three failures: (1) The claimed mechanism is wrong for the platforms this repo ships — on GUI macOS (NS mainline and macport builds) auto-dark selects the 'applescript method using 
- **Tree-sitter remap table dlopens up to 11 grammar libraries at module load time; the non-ts diagnostic re-probes per buffer** — Re-read lisp/init-prog.el:49-141, the installed Emacs 30.2 sources (src/treesit.c:657-800, lisp/startup.el:1560-1607), journal/2026-07-07.md, and measured probe cost with the store's emacs-nox. The mechanism claims are accurate — treesit-ready-p does dynlib_open the grammar on every call with no cache, the top-level call at init-prog.el:95 probes all 11 languages at load, and after-init-hook (startup.el:1580) runs before command-line-1 visits files (startup.el:1607). But the finding's consequenc
- **Background package-refresh fires on every fresh Nix deployment where it can never be useful** — Re-read /home/user/jotain/init.el:42-109, /home/user/jotain/bench/init.el, and the actual package.el shipped with the devenv Emacs (emacs-nox-30.2). The finding describes documented, deliberate behavior as a defect: the missing-cache trigger on fresh machines is explicitly stated as intended in three places (init.el:51-54 "or an archive has no cache at all", the defvar docstring at init.el:72-76, and the purpose comment at init.el:97-99 "so the next hand install doesn't pay the network cost"), a
- **File-category completion styles omit `basic`, which orderless documents as required for TRAMP method/host completion** — Re-read lisp/init-completion.el:32-42 (claim's reading of the code is accurate: file/project-file styles are (partial-completion orderless), completion-category-defaults nil), then tested the claim empirically against the pinned Emacs 30.2 build in the nix store (/nix/store/cj3j4n3bv3s9j26f4mxdbp3nzpc0zqpr-emacs-nox-30.2) rather than trusting the orderless README. Batch test with tramp loaded, minibuffer-completing-file-name t, calling completion-all-completions on read-file-name-internal under 

## Coverage gaps acknowledged

- journal/ holds 12 dated working-note .md files (2026-04-08 … 2026-07-20) in the repo root alongside the already-flagged plan.md/TODO.md/questions.md debris; they ship inside the flake src used by every check derivation — not evidenced as harmful, but the stale-debris finding should probably subsume them.
- bench/find-file-bench.el was only read in part (first ~30 lines); its hook-instrumentation approach may share the bench/early-init.el blind spots already found, unverified.
- devenv--call passes a buffer as :stderr with no sentinel on the implicit stderr pipe process (devenv.el:236-245), so the default sentinel likely appends 'Process … finished' noise to captured stderr — would need a live Emacs run to confirm the exact text and whether devenv--first-line can ever surface it as the error message.
- The claim in gap finding 2 that corfu's capf wrapper does not demote errors is inferred from memory of corfu/cape sources (not on disk in this environment); the eglot side of the failure (signal without server) is verified against the shipped 30.2 source.
- Whether use-package :custom-before-load semantics interact safely with em-term's late-loading eshell-destroy-buffer-when-process-dies was empirically checked for the value-preservation half (defcustom does not clobber a prior customize-set-variable — verified in batch Emacs 30.2), but the :set-callback half of the contract for such late options across the config was not audited.

## Open questions

- Exact Emacs version each variant resolves to on the pinned nixpkgs/overlay (is pkgs.emacs really 31.x? what rev is emacs-git/igc?) — resolve with: nix eval --raw .#emacs.version  and  nix eval --raw --expr '(import ./emacs.nix { variant = "git"; }).version' --impure
- Whether the cache-parity invariant actually holds on the current lock (mainline/git/unstable/igc outPaths == pkgs.emacs/emacs-git/emacs-unstable/emacs-igc) — resolve by running the nix-instantiate outPath comparison documented at emacs.nix:238-246 / CLAUDE.md
- Which scanned use-package names FAIL to resolve in emacs-overlay's epkgs and therefore silently fall through to runtime MELPA (ghostel is the prime suspect; also eca, majutsu deps) — resolve with: nix eval --raw --impure --expr 'let f = builtins.getFlake (toString ./.); in f.lib.listPackageNames { dir = ./lisp; }'  and by building packages.default with --print-build-logs and grepping for the use-package.nix:319 trace warning
- Whether a fresh `just run-built` startup performs any MELPA installs (i.e., the actual runtime/Nix parity gap in package terms) — resolve by running: JOTAIN_NO_PACKAGE_REFRESH=1 ./result/bin/emacs --init-directory=. --batch -l early-init.el -l init.el 2>&1 | grep -i 'installing\|contacting' on an offline/sandboxed host
- Real startup time and per-module cost on target hardware (emacs-init-time, GC counts, native-comp queue behavior with the hardcoded 3-job cap) — resolve with: ./result/bin/emacs --init-directory=. -f emacs-init-time (bench harness `just bench` is currently a disabled stub, Justfile:134-139; bench/early-init.el still exists)
- Whether `nix flake check` and `devenv test` currently pass on this lock state — resolve by running: nix flake check --print-build-logs  and  devenv test
- Whether jylhisEmacsPackages builds at all (Justfile:177-181 says the Meson fork crashes byte-compiling bundled packages; it was removed from flake packages but is still reachable via services.jotain.emacsBackend = "jylhis") — resolve with: nix build --impure --expr '(import ./. {}).jylhis-emacs' or evaluating the HM module with that backend
- The concrete divergence caused by the devenv/flake nixpkgs desync (which shell tools differ between ffa10e26 and 3b32825d) — resolve with: nix store diff-closures against both nixpkgs revs for the devenv package list, after confirming the desync with: jq -r '.nodes[.nodes.root.inputs.nixpkgs].locked.rev' flake.lock vs jq -r '.nodes.nixpkgs.locked.rev' devenv.lock
- Whether eca-emacs's runtime server download is actually reachable in mode (a) (which URL/version it fetches when `eca` is absent from PATH) — resolve by reading the eca-emacs package source: grep -rn 'download\|url' elpa/eca-*/ after a source-checkout install, or the upstream editor-code-assistant/eca-emacs repo
- Whether ghostel's `'download` module auto-install verifies any checksum/signature and what host it fetches from — resolve by reading the installed ghostel package source (elpa/ghostel-*/ghostel-module.el or the epkgs source): grep -n 'url\|download' in that file
- Actual MELPA package versions that land in elpa/ in mode (a) (unpinned by design; differs per install day) — resolve on a given machine with: ls elpa/ or M-x package-list-packages after bootstrap
- The full store-path cache-parity eval (CLAUDE.md's snippet, after fixing its lock-node lookup per finding 2) could not run in the review sandbox — GitHub tarball fetches outside repo scope are blocked by the proxy. Run it locally or in CI against the corrected snippet.
