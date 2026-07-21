# nix/use-package.nix — Extract use-package declarations from Emacs Lisp
# files and map them to `emacsPackages` attributes.
#
# Two parsers are exposed, both built on the same low-level helpers:
#
#   • parsePackagesFromContent — name-only scan used by
#     `emacsWithPackagesFromUsePackage` to populate the build-time
#     package list (re-implements the public part of
#     emacs-overlay's helper).
#   • parsePackagesWithDocFromContent — like the above but also
#     extracts the `;;; @doc` block immediately above each form so
#     `nix/packages-doc.nix` can render the package reference.
#
# We intentionally do NOT vendor `fromElisp` like emacs-overlay does. The
# config in lisp/init-*.el never nests `(use-package ...)` forms inside
# `:config` blocks, never uses `:disabled` conditionally, and never uses
# `leaf`, so a regex scan is sufficient and keeps the dependency surface
# small.
#
# `;;; @doc` convention (multi-line, no need to repeat the marker):
#
#     ;;; @doc First sentence — the marker line.
#     ;;; Continuation lines drop the @doc prefix.
#     ;;;
#     ;;; A blank `;;;` line is a paragraph break.
#     (use-package foo …)
#
# The legacy form where every line is `;;; @doc <text>` still works.
#
# Public API:
#
#     let up = import ./nix/use-package.nix { inherit lib; };
#     in up.packagesForDirectory { dir = ./lisp; epkgs = …; }

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

  # ── Shared low-level helpers ──────────────────────────────────────
  #
  # `builtins.split` with a capturing pattern returns an alternating
  # list of [pre-text, [capture1], mid-text, [capture2], post-text, …].
  # We walk that list in pairs: for every capture group (the package
  # name) we look at the *following* string element (the form's body)
  # for `:ensure`/`:disabled`, and at the *preceding* string element
  # (between the previous form and this one) for the `;;; @doc` block.

  splitOnUsePackage = content: split "\\(use-package[[:space:]]+([a-zA-Z0-9+_-]+)" content;

  # Text following capture at index idx (form body up to the next form).
  getNextText =
    parts: idx:
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

  # Text preceding capture at index idx (between previous form and this).
  getPrevText =
    parts: idx:
    let
      prev = idx - 1;
    in
    if prev >= 0 then
      let
        v = elemAt parts prev;
      in
      if builtins.isString v then v else ""
    else
      "";

  # Character classes used as word-boundary guards. POSIX ERE wants `-`
  # at the start or end of a bracket expression, so we keep it last. We
  # spell alphanumerics out literally instead of using `[:alnum:]`
  # because some POSIX regex backends choke on `[[:alnum:]_-]`
  # (ambiguous with the `[ ]` opener).
  wordChar = "[A-Za-z0-9+_-]";
  endSym = "[^A-Za-z0-9_-]";

  isEnsureNil = block: match (".*:ensure[[:space:]]+nil(" + endSym + ".*)?") block != null;

  # use-package honors both `:disabled t` and bare `:disabled`.
  isDisabled = block: match (".*:disabled([[:space:]]+t)?(" + endSym + ".*)?") block != null;

  # :ensure nil  ->  null  (skip — built-in or Nix-provided)
  # :ensure t    ->  use the use-package head name
  # :ensure foo  ->  use foo instead (emacs-overlay aliasing)
  # :ensure …    ->  absent keyword means "use head name"
  resolveEnsureName =
    headName: block:
    let
      tMatch = match (".*:ensure[[:space:]]+t(" + endSym + ".*)?") block;
      aliasMatch = match (".*:ensure[[:space:]]+(" + wordChar + "+).*") block;
    in
    if isEnsureNil block then
      null
    else if tMatch != null then
      headName
    else if aliasMatch != null then
      head aliasMatch
    else
      headName;

  # ── ;;; @doc extractor ────────────────────────────────────────────

  # Match a triple-semicolon comment line, accepting both ";;;" alone
  # and ";;; <anything>". Excludes ";;;;" (section headers) and
  # ";;;<no space>" (which conventionally is also a section header).
  isTripleSemi = line: match ";;;( .*)?" line != null;

  # Strip the ";;; " (or just ";;;") prefix.
  stripTriplePrefix =
    line:
    let
      m = match ";;; ?(.*)" line;
    in
    if m != null then head m else line;

  # If the input begins with "@doc" (after the ;;; prefix has already
  # been stripped), return the content after the marker; otherwise
  # null. Accepts "@doc <text>", "@doc: <text>", and bare "@doc".
  stripDocMarker =
    triple:
    let
      m = match "@doc(.*)" triple;
    in
    if m == null then
      null
    else
      let
        raw = head m;
        sep = match "[: ][[:space:]]*(.*)" raw;
      in
      if sep != null then head sep else raw;

  # Strip ";;; " and (if present) "@doc[: ]?" from a line.
  stripCommentLine =
    line:
    let
      triple = stripTriplePrefix line;
      docM = stripDocMarker triple;
    in
    if docM != null then docM else triple;

  # Collect the ;;;-comment block immediately above the form, then keep
  # only the slice from the first `;;; @doc` marker downward — that
  # slice is the doc. Anything above the marker (file headers like
  # `;;; Code:`) is discarded.
  collectDocLines =
    lines:
    let
      walkBack =
        acc: idx:
        if idx < 0 then
          acc
        else
          let
            line = elemAt lines idx;
          in
          if isTripleSemi line then walkBack ([ line ] ++ acc) (idx - 1) else acc;

      block = walkBack [ ] (length lines - 1);

      findMarkerIndex =
        blk:
        let
          go =
            i:
            if i >= length blk then
              null
            else if stripDocMarker (stripTriplePrefix (elemAt blk i)) != null then
              i
            else
              go (i + 1);
        in
        go 0;

      markerIdx = findMarkerIndex block;

      docBlock =
        if markerIdx == null then [ ] else lib.lists.sublist markerIdx (length block - markerIdx) block;
    in
    map stripCommentLine docBlock;

  extractDocFromPrev =
    prevText:
    let
      raw = lib.splitString "\n" prevText;
      # builtins.split returns text up to (but not including) the `(`
      # of `(use-package …)`, so the last element is usually an empty
      # string from the trailing newline. Drop it so the marker check
      # actually reaches the last comment line.
      n = length raw;
      lines = if n > 0 && elemAt raw (n - 1) == "" then lib.lists.sublist 0 (n - 1) raw else raw;
    in
    lib.concatStringsSep "\n" (collectDocLines lines);

  # ── Parsers built on the shared helpers ──────────────────────────

  parsePackagesFromContent =
    content:
    let
      parts = splitOnUsePackage content;

      processCapture =
        idx: item:
        if isList item && item != [ ] then
          let
            headName = head item;
            block = getNextText parts idx;
          in
          if isDisabled block then null else resolveEnsureName headName block
        else
          null;
    in
    filter (x: x != null && x != "") (lib.imap0 processCapture parts);

  parsePackagesWithDocFromContent =
    content:
    let
      parts = splitOnUsePackage content;

      processCapture =
        idx: item:
        if isList item && item != [ ] then
          let
            headName = head item;
            block = getNextText parts idx;
            prevBlock = getPrevText parts idx;
          in
          if isDisabled block then
            null
          else
            {
              name = headName;
              doc = extractDocFromPrev prevBlock;
              ensureNil = isEnsureNil block;
            }
        else
          null;
    in
    filter (x: x != null) (lib.imap0 processCapture parts);

  # ── File / directory helpers ──────────────────────────────────────

  parsePackagesFromFile = file: parsePackagesFromContent (readFile file);
  parsePackagesWithDocFromFile = file: parsePackagesWithDocFromContent (readFile file);

  scanDirectory =
    dir:
    let
      all = lib.filesystem.listFilesRecursive dir;
      elFiles = filter (f: lib.hasSuffix ".el" (toString f)) all;
      names = lib.concatMap parsePackagesFromFile elFiles;
    in
    lib.unique names;

  scanDirectoryWithDoc =
    dir:
    let
      all = lib.filesystem.listFilesRecursive dir;
      elFiles = filter (f: lib.hasSuffix ".el" (toString f)) all;
      sortedFiles = lib.sort (a: b: toString a < toString b) elFiles;
    in
    map (f: {
      # `baseNameOf` retains the source path's string context, which
      # later trips `listToAttrs` (attrset keys must be context-free).
      # `unsafeDiscardStringContext` strips it; the basename is just a
      # filename ("init-ai.el"), no longer a reference to a store path.
      file = builtins.unsafeDiscardStringContext (builtins.baseNameOf (toString f));
      entries = parsePackagesWithDocFromFile f;
    }) sortedFiles;

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
    else
      epkgs.${mapped} or (
        if warnMissing then
          trace "use-package: '${name}' (→ ${mapped}) not found in emacsPackages — falling through" null
        else
          null
      );

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
    parsePackagesWithDocFromContent
    parsePackagesWithDocFromFile
    scanDirectoryWithDoc
    ;
}
