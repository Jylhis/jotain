# Jotain

**Jotain** (Finnish for "something") is a GNU Emacs 31 configuration
(floor: Emacs 31.1) with a Nix build layer and no framework underneath.
Nix builds the editor, with binary-cache parity as an invariant; modular
Elisp (`lisp/init-*.el`, one file per concern) configures it.

Documentation: **<https://page.jylhis.com/jotain>** has the rendered docs,
the full manual (HTML/Info/man), the Nix module options reference, and a
per-package "why is this here" reference.

## Quick start

```sh
# Build Emacs via Nix and launch it with this config, isolated
# from your ~/.emacs.d:
just run-built
```

Development happens in the flake's dev shell (`nix develop`, or prefix
commands with `nix develop -c`; `nix-shell` works too). It provides
linters, language servers, and the docs toolchain, but not Emacs, which
`just run-built` builds on demand. `just check` runs the full
`nix flake check`; `just` alone lists every recipe.

## Installing

The flake targets `x86_64-linux` and exposes the full distribution
(`packages.x86_64-linux.default`, Emacs + tree-sitter grammars), a bare
`emacs`, and modules for Home Manager (`services.jotain` daemon) and NixOS
(`services.jotain.users.<name>` installs the programs per user). See
[Installation](https://page.jylhis.com/jotain/docs/installation).

## Contributing

`AGENTS.md` is the contributor guide: structure, commands, style,
testing, and architecture notes. When it disagrees with the code, that's
a bug.

## License

GPL-3.0-or-later. See [LICENSE](LICENSE).
