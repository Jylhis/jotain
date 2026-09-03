# Jotain

**Jotain** (Finnish for "something") is a GNU Emacs 31 configuration
(floor: Emacs 30.1) with a Nix build layer — no framework underneath.
Nix builds the editor from source with binary-cache parity as an
invariant; modular Elisp (`lisp/init-*.el`, one file per concern)
configures it.

Documentation: **<https://page.jylhis.com/jotain>** — rendered docs, the full
manual (HTML/Info/man), the Nix module options reference, and a
per-package "why is this here" reference.

## Quick start

```sh
# Build Emacs via Nix and launch it with this config, isolated
# from your ~/.emacs.d:
just run-built
```

Development happens in the [devenv](https://devenv.sh) shell
(`devenv shell`, or prefix commands with `devenv shell --`); it provides
linters, language servers, and the docs toolchain — Emacs itself is
built on demand. `just check` runs the full `nix flake check`; `just`
alone lists every recipe. In a bare container with no Nix, run
`scripts/bootstrap-agent-env.sh` first.

## Installing

The flake exposes the full distribution (`packages.<system>.default`,
Emacs + tree-sitter grammars), a bare `emacs`, a terminal-only
`emacs-nox`, and modules for Home Manager (`services.jotain` daemon),
NixOS / nix-darwin, and nix-on-droid. See
[Installation](https://page.jylhis.com/jotain/docs/installation) for the
details.

## Contributing

`AGENTS.md` is the contributor guide (structure, commands, style,
testing); `CLAUDE.md` carries the full architecture notes. Both are kept
accurate — when they disagree with the code, that's a bug.

## License

GPL-3.0-or-later — see [LICENSE](LICENSE).
