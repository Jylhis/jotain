# Packages the use-package scan does not pick up (`:ensure nil`, or loaded
# with a plain `require`), injected through `extraEmacsPackages`. Each name
# is an unguarded `epkgs.<name>` lookup, so one that stops resolving fails
# the build.
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
