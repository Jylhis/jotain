# emacs

A GNU Emacs 31 configuration (floor 31.1) plus the Nix expressions that build Emacs itself.
Stable. Main page: `just man emacs`; testing is `emacs-testing(7)`, the architecture is in
`docs/architecture/`. The dev shell has tooling only and no `emacs`: launch with
`just run-built`.

## Layout

- `early-init.el`, `init.el`, `lisp/init-*.el`: one module per concern, `require`d from
  `init.el` in order (`docs/architecture/modules.mdx`).
- `lisp/devenv.el`, `repo-review.el`, `magit-land.el`: standalone packages with their own
  namespaces, wired in by `init-devenv.el` and `init-review.el`.
- `emacs.nix`, `overlay.nix`, `default.nix`, `module.nix` (Home Manager),
  `module-system.nix` (NixOS), `flake.nix`: all build through `nix/outputs.nix`.
  `nix/mk-overlay.nix` is the overlay, `nix/checks.nix` the checks.
- `test/`: ERT; every `test/*.el` is loaded by the test check, no registration.
- `docs/`: website and Info manual sources. `etc/`: debug harness, vendored `etc/elisp-doc`,
  `etc/lang-eval` registry. `bench/`: startup benchmarks.
- `journal/`: working notes, possibly stale. The code wins.
- `.claude/knowledge/emacs/`: notes on Emacs internals and Elisp practice.

## Commands

- `just test`: ERT with `emacs -batch` (skips when no `emacs` is on PATH).
- `just nix-check`: builds every `emacs-*` check. One check:
  `nix build -f . local.checks.emacs-<name>` at the repo root.
- `just run-built [ARGS]`: builds the distribution and runs it with
  `--init-directory` on this directory, isolated from `~/.emacs.d`.
- `just cache-parity`: every value must print `true`.
- `just docs-refresh-packages`: regenerate `docs/configuration/package-reference.mdx` from the
  `;;; @doc` blocks in `lisp/`. Needed after editing a block, or `packages-doc-in-sync` fails.
- `just docs-refresh-lang-matrix`: regenerate `docs/reference/language-support.mdx` from
  `etc/lang-eval/my-lang-registry.el`.
- `just lint` is a no-op: this project formats through its own `nix/treefmt.nix`.

## Gotchas

- Cache parity is the first rule of the Nix layer: every default in the `emacs.nix`
  argument list must match nixpkgs' `make-emacs.nix` (and the args emacs-overlay passes), so
  `import ./emacs.nix {}` yields the prebuilt store path. After any edit to the `basePackage`
  block, run `just cache-parity`. Custom `rev`, `useCcache` and `cpuTune` are expected misses.
- With `withPgtk = true`, `basePackage` picks the prebuilt `*-pgtk` sibling. Overriding
  `withPgtk` on the non-pgtk base builds the same content under another name and misses the
  cache. Flag trimming applies only to builds already off parity (noGui).
- Override args are filtered with `lib.intersectAttrs (lib.functionArgs basePackage.override)`
  so an older nixpkgs (24.05+) still evaluates. Keep the filter.
- Two pin sets feed `nix/outputs.nix`: the root nixtamal pins (via the root `nix/inputs.nix`)
  and `flake.lock` (via `nix/locked.nix`, for the flake and off-flake entry points). Both must
  name the same `nixpkgs`, `emacs-overlay` and `treefmt-nix` revisions. Do not edit
  `flake.lock` by hand: run `nix flake lock` here, then align the three pins.
- A `path:` flake inside the tree cannot find the kit helpers (the export vendors them):
  test the flake from an export.
- Hand-pinned upstreams that `just update` does not bump: the packages in
  `nix/extra-packages.nix`, `nix/eca-server.nix` and `nix/likec4-lsp.nix`. Bump rev and hash
  by hand. `etc/elisp-doc` is vendored verbatim and never bumped.
- `emacs-api-doc` is heavy and deploy-path-only: only the docs site and its own check import
  it, never `nix/info-manual.nix` or `nix/mk-overlay.nix`, or editor builds lose cache hits.
- `eca-models-in-sync` compares the OpenRouter models in `config/eca/config.json` with
  gptel's `:models` in `lisp/init-ai.el`. Reconcile both by hand.
- `jylhis-emacs-themes` and the docs-site CSS come from the in-tree design library; the
  `jylhis-design` flake input is the published repository, and `nix/world.nix` picks the
  in-tree copy inside the tree.
- `config-startup` is the only check that evaluates `:config` blocks. `scanner-fidelity`
  checks the regex scanner of `nix/use-package.nix` against Emacs' reader.

## Conventions

- No builtins.el and third-party.el split: a package that enhances a built-in lives in the
  built-in's file (`dirvish` in `init-navigation.el`, `magit` in `init-vc.el`).
- `setopt`, not `setq`, for `defcustom` variables (`setq` skips `:set` and `:type`).
- `use-package-always-ensure` is `t`, so built-ins and Nix-provided packages need
  `:ensure nil`. Archives are never fetched at startup.
- LSP hooks (`eglot-ensure`) and `apheleia` formatters live in `init-prog.el`;
  `init-lang-*.el` holds mode regexes and language tweaks.
- A module has the `lexical-binding: t` cookie, ends with `(provide 'init-<concern>)` and
  needs a `(require ...)` in `init.el`. Global symbols use `my-` (`my--` for internals).
- `custom-file` is `var/custom.el`, write-only and never loaded. Never commit `elpa/`, `var/`,
  `result*` or `*.elc`.

<!-- Assisted-by: Claude Code:claude-sonnet-5-5 -->
