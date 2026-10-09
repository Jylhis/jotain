# nix/nix-provided-packages.nix: Emacs packages the lisp/ use-package
# scan does not pick up (declared `:ensure nil', or loaded with a plain
# `require'), so they are injected and documented explicitly.
#
# Consumed by:
#   • nix/mk-overlay.nix:        `extraEmacsPackages` (force-inject).
#   • nix/emacs-package-set.nix: `extraFeatureNames` (API-doc features).
#   • nix/checks.nix:            emacs-packages-eval's expected set.
#
# Curated, NOT `builtins.attrNames (extra-packages.nix …)`: it excludes
# extra-packages overrides the scan already covers (`ghostel`) and the
# built-in shims (xref, project, eglot, flymake), and includes
# `nix-ts-mode`, which comes from the base scope.
#
# Each name is an unguarded flat `epkgs.<name>` lookup (the scope
# contract in nix/use-package.nix), so a name that stops resolving fails
# the build loudly.
[
  "claude-code-ide"
  "combobulate"
  "eglot-booster"
  "jylhis-emacs-themes"
  "majutsu"
  "nix-ts-mode"
  "project-nix-store"
  "qml-ts-mode"
  "tagref"
]
