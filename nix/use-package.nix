# nix/use-package.nix — Extract use-package declarations from Emacs Lisp
# files and map them to `emacsPackages` attributes.
#
# This is a focused re-implementation of two things:
#
#   • `nix/lib/dependencies.nix` from the `main` branch of this repo
#     (pure-Nix regex scanner), and
#   • `emacsWithPackagesFromUsePackage` from nix-community/emacs-overlay
#     (public wrapper that composes the scanner with `emacsPackagesFor`).
#
# We intentionally do NOT vendor `fromElisp` like emacs-overlay does. The
# config in lisp/init-*.el never nests `(use-package ...)` forms inside
# `:config` blocks, never uses `:disabled` conditionally, and never uses
# `leaf`, so a regex scan is sufficient and keeps the dependency surface
# small.
#
# Features:
#
#   • Recognises `(use-package NAME ...)` forms.
#   • Skips forms with `:ensure nil` or `:disabled t` (the block-scoped
#     check looks at the slice of text up to the next `(use-package ...)`
#     form or end-of-file).
#   • Honours `:ensure other-name` aliasing — the aliased name is used
#     for the Nix lookup (emacs-overlay compatibility).
#   • Drops known pseudo-packages (`emacs`) and anything whose lookup on
#     `epkgs` fails (with a `builtins.trace` warning, never a hard
#     failure — missing packages fall through to MELPA at runtime).
#
# Public API:
#
#     let up = import ./nix/use-package.nix { inherit lib; };
#     in up.packagesForDirectory { dir = ./lisp; epkgs = …; }
#
# See `packagesForDirectory` below for the full set of helpers.

{ lib }:

let
  inherit (builtins)
    readFile
    split
    match
    elemAt
    length
    isList
    head
    filter
    trace
    ;

  # ── Block-level parser ────────────────────────────────────────────
  #
  # `builtins.split` with a capturing pattern returns an alternating
  # list of [pre-text, [capture1], mid-text, [capture2], post-text, …].
  # We walk that list in pairs: for every capture group (the package
  # name) we look at the *following* string element, which is the text
  # between this `(use-package …)` form and the next one. That slice
  # is where `:ensure …` / `:disabled t` would appear for this form.
  parsePackagesFromContent =
    content:
    let
      parts = split "\\(use-package[[:space:]]+([a-zA-Z0-9+_-]+)" content;

      # getBlock :: int -> string
      # Text immediately following the N-th capture group (may be "").
      getBlock =
        idx:
        let
          next = idx + 1;
        in
        if next < length parts then
          let
            v = elemAt parts next;
          in
          if builtins.isString v then v else ""
        else
          "";

      # Character class used as a word-boundary guard. POSIX ERE (which
      # `builtins.match` speaks) wants `-` at the start or end of a
      # bracket expression, so we keep it last. We spell alphanumerics
      # out literally instead of using `[:alnum:]` because some POSIX
      # regex backends choke on `[[:alnum:]_-]` (ambiguous with the
      # `[ ]` opener). This works everywhere.
      wordChar = "[A-Za-z0-9_-]";
      endSym = "[^A-Za-z0-9_-]";

      # :ensure nil  ->  skip
      # :ensure t    ->  use the use-package head name
      # :ensure foo  ->  use foo instead (emacs-overlay aliasing)
      # :ensure …    ->  absent keyword means "use head name"
      ensureName =
        headName: block:
        let
          nilMatch = match (".*:ensure[[:space:]]+nil(" + endSym + ".*)?") block;
          tMatch = match (".*:ensure[[:space:]]+t(" + endSym + ".*)?") block;
          aliasMatch = match (".*:ensure[[:space:]]+(" + wordChar + "+).*") block;
        in
        if nilMatch != null then
          null
        else if tMatch != null then
          headName
        else if aliasMatch != null then
          head aliasMatch
        else
          headName;

      isDisabled =
        block:
        let
          m = match (".*:disabled[[:space:]]+t(" + endSym + ".*)?") block;
        in
        m != null;

      # Process the capture at index `idx`.
      processCapture =
        idx: item:
        if isList item && item != [ ] then
          let
            headName = head item;
            block = getBlock idx;
          in
          if isDisabled block then null else ensureName headName block
        else
          null;

      results = lib.imap0 processCapture parts;
    in
    filter (x: x != null && x != "") results;

  # ── File / directory helpers ──────────────────────────────────────

  parsePackagesFromFile = file: parsePackagesFromContent (readFile file);

  scanDirectory =
    dir:
    let
      all = lib.filesystem.listFilesRecursive dir;
      elFiles = filter (f: lib.hasSuffix ".el" (toString f)) all;
      names = lib.concatMap parsePackagesFromFile elFiles;
    in
    lib.unique names;

  # ── Name mapping ──────────────────────────────────────────────────
  #
  # Only the `emacs` pseudo-package needs to be excluded in this repo
  # (used for global `(use-package emacs :ensure nil ...)` blocks that
  # set defaults). Every other package name is looked up directly on
  # `epkgs` — this matches emacs-overlay's behaviour. Entries set to
  # `null` are dropped silently; string entries rename.
  defaultNameMap = {
    emacs = null;
  };

  mapName = extraMap: name: (extraMap // defaultNameMap).${name} or name;

  # ── epkgs lookup ─────────────────────────────────────────────────

  toEmacsPackage =
    {
      extraMap ? { },
      warnMissing ? true,
    }:
    epkgs: name:
    let
      mapped = mapName extraMap name;
    in
    if mapped == null then
      null
    else epkgs.${mapped} or (if warnMissing then
      trace "use-package: '${name}' (→ ${mapped}) not found in emacsPackages — falling through" null
    else
      null);

  packagesForDirectory =
    {
      dir,
      epkgs,
      extraMap ? { },
      warnMissing ? true,
    }:
    let
      names = scanDirectory dir;
      resolved = map (toEmacsPackage { inherit extraMap warnMissing; } epkgs) names;
    in
    filter (p: p != null) resolved;

  # ── High-level wrapper (mirrors emacs-overlay's public API) ───────

  emacsWithPackagesFromUsePackage =
    {
      # Directory (or single file path) to scan. A list of paths is
      # also accepted if a config is split across several directories.
      config,

      # Base Emacs derivation. Defaults to whatever `pkgs.emacs` is.
      package,

      # emacsPackagesFor factory, i.e. `pkgs.emacsPackagesFor`.
      emacsPackagesFor,

      # self: super: attrs override, forwarded to `overrideScope`.
      override ? (_self: _super: { }),

      # Extra packages injected at build time (same shape as
      # `withPackages`: a function epkgs -> [pkg]).
      extraEmacsPackages ? (_epkgs: [ ]),

      # Extra name → name (or name → null) overrides for the mapping
      # table. Useful for handling forks or MELPA names that differ
      # from the elisp symbol.
      extraMap ? { },

      # If true, missing packages trace-warn; if false they fail silently.
      warnMissing ? true,
    }:
    let
      scope = (emacsPackagesFor package).overrideScope override;

      pathsToScan = if builtins.isList config then config else [ config ];

      collect =
        epkgs:
        let
          autoPkgs = lib.concatMap (
            p:
            packagesForDirectory {
              dir = p;
              inherit epkgs extraMap warnMissing;
            }
          ) pathsToScan;
          extras = extraEmacsPackages epkgs;
        in
        autoPkgs ++ extras;
    in
    scope.withPackages collect;

  # ── Debug helpers ─────────────────────────────────────────────────

  listPackageNames =
    {
      dir,
      extraMap ? { },
    }:
    let
      names = scanDirectory dir;
      labelled = map (
        n:
        let
          mapped = mapName extraMap n;
        in
        "${n} -> ${if mapped == null then "(excluded)" else mapped}"
      ) names;
    in
    lib.concatStringsSep "\n" labelled;

in
{
  inherit
    parsePackagesFromContent
    parsePackagesFromFile
    scanDirectory
    mapName
    toEmacsPackage
    packagesForDirectory
    emacsWithPackagesFromUsePackage
    listPackageNames
    ;
}
