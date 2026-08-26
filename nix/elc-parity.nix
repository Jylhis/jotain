# nix/elc-parity.nix — Diagnostic: byte-diff the monolithic and per-file
# config compiles.
#
# Deliberately NOT a flake check: a byte-for-byte .elc comparison can
# fail for reasons that are the *monolith's* fault rather than the
# split's — the monolith compiles every file in one session, so state
# loaded by earlier files (features, defvars, the gensym counter) can
# leak into later files' bytecode.  In particular a variable defvar'd by
# a previously-loaded module compiles to a *dynamic* binding in the
# monolith but a *lexical* one in a clean per-file session.  A mismatch
# here is therefore a bug report against the sources (a missing
# `defvar`/`declare-function` stub), not build noise — but it must gate
# a human's attention, not the main-branch deploy pipeline
# (deploy.yml's site/Pages jobs `need` the check job).
#
# Run it with `just elc-parity` (needs the distribution Emacs, i.e. a
# network position where nix-community/jylhis cachix are reachable, or
# patience).
{
  pkgs,
  emacs,
  src ? ../.,
}:
let
  monolith = import ./config-compiled.nix { inherit pkgs emacs src; };
  split = import ./config-compiled-split.nix { inherit pkgs emacs src; };
in
pkgs.runCommand "jotain-elc-parity" { } ''
  status=0
  for elc in $(cd ${monolith} && find . -name '*.elc' | sort); do
    if [ ! -e ${split}/$elc ]; then
      echo "MISSING in split: $elc"; status=1
    elif ! cmp -s ${monolith}/$elc ${split}/$elc; then
      echo "DIFFERS: $elc (monolith `stat -Lc%s ${monolith}/$elc`B vs split `stat -Lc%s ${split}/$elc`B)"
      status=1
    fi
  done
  for elc in $(cd ${split} && find . -name '*.elc' | sort); do
    [ -e ${monolith}/$elc ] || { echo "EXTRA in split: $elc"; status=1; }
  done
  if [ $status -ne 0 ]; then
    echo ""
    echo "elc parity failed. A DIFFERS entry usually means session state"
    echo "leaking into the monolith's bytecode — check for a let-bound"
    echo "variable that needs a (defvar foo) stub in the differing file."
    exit 1
  fi
  echo "all .elc byte-identical between monolith and split" | tee $out
''
