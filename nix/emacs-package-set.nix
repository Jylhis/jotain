# nix/emacs-package-set.nix — every Emacs package the configuration
# bundles, resolved from the same sources the distribution build uses:
#
#   • the lisp/ use-package scan (nix/use-package.nix), `:ensure`
#     aliases resolved (e.g. `dired-async` -> epkgs.async),
#   • the Nix-provided extras (nix/nix-provided-packages.nix),
#   • treesit-grammars.with-all-grammars (mk-overlay's extraEmacsPackages).
#
# Consumed by flake.nix (legacyPackages.emacs-packages, so each package
# is buildable as `nix build .#emacs-packages.magit` without `nix flake
# check` ever building them), nix/checks.nix (emacs-packages-eval), and
# nix/emacs-api-doc.nix (its per-package feature list). Keep the
# resolution logic in this file only.
#
# Requires the Jotain overlay applied to `pkgs` (pkgs.jotainEmacs); the
# flake's pkgsFor and checks.nix both provide that.
{
  pkgs,
  lispDir ? ../lisp,
}:
let
  inherit (pkgs) lib;

  up = import ./use-package.nix { inherit lib; };
  extraPackages = import ./extra-packages.nix { inherit pkgs; };

  # The epkgs scope for the config's Emacs, with the Nix-provided
  # extras merged in — the same scope `emacsWithPackagesFromUsePackage`
  # builds the distribution from (nix/mk-overlay.nix).
  scope = (pkgs.emacsPackagesFor pkgs.jotainEmacs).overrideScope extraPackages;

  # Packages provided by Nix, not via the lisp/ scan (shared list; see
  # that file's header).
  extraFeatureNames = import ./nix-provided-packages.nix;

  # Every fetched (`:ensure` non-nil) package from the lisp/ scan.
  scanned = up.scanDirectoryWithDoc lispDir;
  allEntries = lib.concatMap (s: s.entries) scanned;
  docEntries = lib.filter (e: !e.ensureNil) allEntries;
  fetchedNames = lib.unique (map (e: e.name) docEntries);
  featureNames = lib.sort (a: b: a < b) (lib.unique (fetchedNames ++ extraFeatureNames));

  # Map a feature (head/require name) to its resolved `:ensure` name,
  # so aliased declarations like `(use-package dired-async :ensure async)`
  # look up the right epkgs attribute. Extras (not in the scan) map to
  # themselves.
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

  # Buildable attrset: `nix build .#emacs-packages.<feature>`. Keyed by
  # the use-package head name (what the config and the package
  # reference / API docs key provenance by); `dired-async` therefore
  # holds the `async` derivation its `:ensure async` resolves to.
  # recurseForDerivations lets flake attrpaths walk into the set.
  byName = lib.recurseIntoAttrs (
    lib.listToAttrs (map (f: lib.nameValuePair f.feature f.pkg) features)
    // {
      treesit-grammars = scope.treesit-grammars.with-all-grammars;
    }
  );
}
