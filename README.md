# Emacs configuration

This is a GNU Emacs 31 configuration (floor: Emacs 31.1) with a Nix build
layer and no framework underneath. Nix builds the editor, with binary-cache
parity as an invariant; modular Elisp (`lisp/init-*.el`, one file per
concern) configures it.

Documentation: **<https://docs.example.org>** has the rendered docs,
the full manual (HTML/Info/man), the Nix module options reference, and a
per-package "why is this here" reference.

## Quick start

```sh
# Build Emacs via Nix and launch it with this config, isolated
# from your ~/.emacs.d:
just run-built
```

Development happens in the [devenv](https://devenv.sh) shell
(`devenv shell`, or prefix commands with `devenv shell --`). It provides
linters, language servers, and the docs toolchain, but not Emacs, which
`just run-built` builds on demand. `just check` runs the full
`nix flake check`; `just` alone lists every recipe. In a bare container
with no Nix, run `scripts/bootstrap-agent-env.sh` first.

## Installing

The flake exposes the full distribution (`packages.<system>.default`,
Emacs + tree-sitter grammars), a bare `emacs`, a terminal-only
`emacs-nox`, and modules for Home Manager (`services.emacsConfig` daemon),
NixOS, and nix-on-droid. See
[Installation](https://docs.example.org/docs/installation).

## Contributing

`AGENTS.md` is the contributor guide: structure, commands, style,
testing, and architecture notes. When it disagrees with the code, that's
a bug.

## License

GPL-3.0-or-later. See [LICENSE](LICENSE).
