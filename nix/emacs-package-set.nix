# nix/emacs-package-set.nix — every Emacs package the configuration
# bundles, resolved from the same sources the distribution build uses:
#
#   • the lisp/ use-package scan (nix/use-package.nix), `:ensure`
#     aliases resolved (e.g. `dired-async` -> epkgs.async),
#   • the Nix-provided extras (nix/nix-provided-packages.nix),
#   • treesit-grammars.with-all-grammars (mk-overlay's extraEmacsPackages).
#
# Consumed by flake.nix (legacyPackages.emacs-packages), nix/checks.nix
# (emacs-packages-eval) and nix/emacs-api-doc.nix. Keep the resolution
# logic here only. Needs the Jotain overlay in `pkgs` (pkgs.jotainEmacs).
{
  pkgs,
  lispDir ? ../lisp,
}:
let
  inherit (pkgs) lib;

  up = import ./use-package.nix { inherit lib; };
  extraPackages = import ./extra-packages.nix { inherit pkgs; };

  # The same scope the distribution is built from (nix/mk-overlay.nix).
  scope = (pkgs.emacsPackagesFor pkgs.jotainEmacs).overrideScope extraPackages;

  extraFeatureNames = import ./nix-provided-packages.nix;

  # Every fetched (`:ensure` non-nil) package from the lisp/ scan.
  scanned = up.scanDirectoryWithDoc lispDir;
  allEntries = lib.concatMap (s: s.entries) scanned;
  docEntries = lib.filter (e: !e.ensureNil) allEntries;
  fetchedNames = lib.unique (map (e: e.name) docEntries);
  featureNames = lib.sort (a: b: a < b) (lib.unique (fetchedNames ++ extraFeatureNames));

  # Map a feature (head/require name) to its resolved `:ensure` name,
  # so aliased declarations like `(use-package dired-async :ensure async)`
  # look up the right epkgs attribute; extras map to themselves.
  enameFor = lib.listToAttrs (map (e: lib.nameValuePair e.name e.ename) docEntries);

  # Pair each feature with its resolved epkg derivation, dropping any
  # name that does not resolve.
  features = lib.filter (f: f.pkg != null) (
    map (
      feature:
      let
        ename = enameFor.${feature} or feature;
      in
      {
        inherit feature;
        pkg = up.toEmacsPackage { warnMissing = false; } scope ename;
      }
    ) featureNames
  );
in
{
  inherit scope features;

  # `nix build .#emacs-packages.<feature>`, keyed by use-package head
  # name: `dired-async` holds the `async` derivation.
  byName = lib.recurseIntoAttrs (
    lib.listToAttrs (map (f: lib.nameValuePair f.feature f.pkg) features)
    // {
      treesit-grammars = scope.treesit-grammars.with-all-grammars;
    }
  );
}
