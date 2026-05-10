# jylhis/emacs Meson Build Report

Date: 2026-05-09
Repository tested: https://github.com/jylhis/emacs
Revision tested: `eaf289b4f7414744a23912ab7aae0a518d998242` (`dev`)
Platform tested: `x86_64-darwin`
Nix package added in downstream repo: `emacs-jylhis.nix`

## Summary

The Meson branch can be built as a terminal-only Nix package after several
downstream workarounds.  A plain batch startup succeeds:

```sh
nix build --no-link .#jylhis-emacs
/nix/store/1v6axcjlkfri2n0lya4g092dj6ml0422-jylhis-emacs-31.0.50-eaf289b/bin/emacs \
  --batch --eval '(princ emacs-version)'
```

The full downstream package set still fails while byte-compiling MELPA
packages because the resulting Emacs segfaults during an `avy` build:

```sh
nix build --no-link .#jylhis-emacs-packages
```

Final failure:

```text
Segmentation fault: 11
emacs --batch -Q -L "$NIX_BUILD_TOP/package-build" \
  -l "$melpa2nix" -f melpa2nix-build-package avy ...
```

## Issues Found

1. `all-features`/feature defaults are too eager for terminal-only Darwin builds.
   The build required explicitly disabling `xft`, `xpm`, `xinput2`,
   `xdbe`, `toolkit-scroll-bars`, `gpm`, `selinux`, `libotf`, `m17n-flt`,
   and `gsettings`.  Several of these are Linux/X11-specific or optional,
   but Meson treated them as required when dependencies were absent.

2. Generated Unicode Lisp files are built but not installed.
   `charscript.el` and `emoji-zwj.el` are generated under the build tree,
   while `meson/meson_install.py` installs only the source `lisp/` tree.
   Runtime package compilation then fails in `international/characters.el`
   with missing `charscript`.

3. Generated Dired autoloads are not installed.
   The installed tree lacks `dired-loaddefs.el`.  Package builds that pass
   output directories on the Emacs command line hit `dired-goto-subdir`
   before `dired-aux` is loaded.  A minimal downstream shim for
   `dired-goto-subdir` gets past this specific failure, but the proper fix is
   to generate and install the complete loaddefs set.

4. No installed `emacs.pdmp` is produced in the default build.
   Batch startup loads `loadup.el` and a long list of source Lisp files at
   runtime.  This is slow and exposes bootstrap/load-order behavior that
   normal installed Emacs builds avoid.

5. Full package compilation currently hits a native crash.
   After the install/autoload workarounds, building the downstream package set
   reaches MELPA package compilation and then segfaults while building `avy`.
   This looks like a runtime/compiler issue in the Meson-built Emacs rather
   than a Nix packaging error.

## Downstream Workarounds Applied

- Build the branch as a separate Meson derivation instead of reusing the
  existing autotools/nixpkgs `emacs30` wrapper.
- Default to terminal-only mode on Darwin because NS/PGTK are not wired in the
  Meson source list yet.
- Disable optional/platform-specific Meson features explicitly.
- Copy generated `charscript.el` and `emoji-zwj.el` from the build tree into
  the installed `lisp/international/` directory.
- Add a minimal `dired-loaddefs.el` shim containing an autoload for
  `dired-goto-subdir`.

## Suggested Upstream Fixes

- Make `feature=auto` stay optional when dependencies are missing, especially
  for non-Linux and terminal-only builds.
- Install generated Lisp artifacts, not only source-tree Lisp files.
- Generate and install loaddefs files as part of the normal Meson install.
- Add a Meson target that produces and installs `emacs.pdmp`.
- Add CI smoke tests for:
  - `meson setup build -Dtoolkit=none` on Darwin
  - `meson install`
  - installed `emacs --batch --eval '(princ emacs-version)'`
  - installed `emacs --batch -Q` running a small package byte-compilation job
