# nix/nix-provided-packages.nix — the config's Emacs packages that come
# from Nix (nix/extra-packages.nix or the base scope) rather than from the
# `lisp/` use-package scan, and that must therefore be BOTH force-injected
# into the distribution and documented by the API-doc generator.
#
# Single source of truth, consumed by:
#   • nix/mk-overlay.nix   — as `extraEmacsPackages` (force-inject).
#   • nix/emacs-api-doc.nix — as `extraFeatureNames` (document).
#
# This is a curated list, NOT `builtins.attrNames (extra-packages.nix …)`:
#   • it EXCLUDES `ghostel` (terminal-only; not in the GUI doc/inject set),
#   • it INCLUDES `nix-ts-mode`, which is provided by the base scope (a real
#     nixpkgs/overlay package), not by nix/extra-packages.nix — the config
#     declares it `:ensure nil`, so the `lisp/` scan skips it and it must be
#     injected explicitly.
#
# Every name here is looked up as a flat `epkgs.<name>` attribute (the
# dual-source scope contract in nix/use-package.nix), unguarded on purpose:
# a name that ever stops resolving should be a loud build failure, not a
# silently missing feature.
[
  "claude-code-ide"
  "combobulate"
  "eglot-booster"
  "jylhis-emacs-themes"
  "majutsu"
  "nix-ts-mode"
  "qml-ts-mode"
  "tagref"
]
