#!/usr/bin/env bash
# Bring up Jotain on a bare Debian/Ubuntu host using apt — no Nix builds.
#
# The Nix layer (emacs.nix + nix/mk-overlay.nix) is the supported way to
# get an Emacs that matches this config: it pins the Emacs 31 pretest,
# every epkg, and ~275 tree-sitter grammars. This script is the fallback
# for a host where Nix cannot fetch or build — an agent container behind
# an egress proxy, a locked-down CI box, a machine where only the distro
# package manager is available.
#
# What apt can and cannot supply, and why each phase below exists:
#
#   * Emacs itself. `init.el' declares (emacs "30.1"); Ubuntu 24.04 ships
#     29.3 and the newer-Emacs PPAs (ubuntu-elisp, kelleyk) are commonly
#     unreachable from a proxied container. So Emacs is built from the
#     emacs-mirror GitHub mirror of the emacs-30.1 tag, with every build
#     dependency coming from `apt build-dep emacs' (phase 1 + 3).
#
#   * Elisp packages. ELPA/MELPA are frequently blocked while github.com
#     stays reachable, and Ubuntu only carries ~30 of the ~100 packages
#     this config needs as elpa-* debs — too few, and pinned to versions
#     built for Emacs 29. Instead phase 5 builds a *local* MELPA-format
#     archive with MELPA's own package-build, from recipes resolved to
#     GitHub, and phase 6 installs from it via plain package.el. Using
#     the upstream builder is what gets the per-package :files specs,
#     -pkg.el descriptors, autoloads and dependency versions right.
#
#   * tree-sitter. Current grammar masters emit ABI 15; Ubuntu's
#     libtree-sitter 0.20.8 tops out at 14, so grammars build but refuse
#     to load. Phase 2 installs 0.25.10 — new enough for ABI 15, and the
#     last series that still exports `ts_language_version', which Emacs
#     30.1 requires (0.26 removed it). Its SONAME is rewritten to
#     libtree-sitter.so.0 so the Emacs built in phase 3 picks it up.
#
#   * Nerd Fonts. Not in apt at all; fetched from the ryanoasis release
#     in phase 7 so nerd-icons/doom-modeline render glyphs, not tofu.
#
# Everything lands outside the repo (/usr/local) except the Elisp
# packages, which go to the gitignored elpa/ that package.el already
# owns. Re-running is safe: each phase skips work that is already done.
#
# Usage:  sudo scripts/bootstrap-apt-env.sh
# Then:   emacs --init-directory=<repo>          (or use the launcher it writes)
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SRC=/usr/local/src
EMACS_TAG="${EMACS_TAG:-emacs-30.1}"
TS_TAG="${TS_TAG:-v0.25.10}"
EMACS_BIN=/usr/local/bin/emacs
JOBS="$(nproc)"

log() { printf '\n\033[1m== %s\033[0m\n' "$*"; }

require_root() {
    [ "$(id -u)" -eq 0 ] || { echo "must run as root (apt installs)" >&2; exit 1; }
}

# ---------------------------------------------------------------- phase 1
# Emacs build deps come from `apt build-dep emacs', which needs deb-src
# entries; noble's deb822 sources.list has them commented out by default.
# The rest are tools the config shells out to, plus jinx's enchant
# headers (jinx compiles jinx-mod.so on first use) and the fonts
# init-ui.el probes for.
apt_packages() {
    log "phase 1: apt packages"
    if ! grep -q '^Types: deb deb-src' /etc/apt/sources.list.d/ubuntu.sources 2>/dev/null; then
        sed -i 's/^Types: deb$/Types: deb deb-src/' /etc/apt/sources.list.d/ubuntu.sources || true
    fi
    apt-get update -y
    apt-get build-dep -y emacs
    apt-get install -y --no-install-recommends \
        build-essential git curl wget unzip patchelf pkg-config \
        libgccjit-13-dev libenchant-2-dev \
        ripgrep fd-find jq sqlite3 shellcheck pandoc texinfo \
        hunspell hunspell-en-us \
        just direnv zoxide fzf clangd nodejs npm python3-pip pipx tidy \
        fonts-jetbrains-mono fonts-noto-core fonts-noto-color-emoji \
        xvfb x11-utils
    # Debian names the binary fdfind; most tooling looks for `fd'.
    [ -e /usr/local/bin/fd ] || ln -sf "$(command -v fdfind)" /usr/local/bin/fd
}

# ---------------------------------------------------------------- phase 2
# Ubuntu's libtree-sitter is too old for ABI 15 grammars. Build 0.25.10
# and make it the libtree-sitter.so.0 the linker resolves: upstream's
# SONAME is libtree-sitter.so.0.25, which no Emacs binary asks for, so
# patchelf rewrites it. The distro lib is removed rather than shadowed —
# ldconfig otherwise keeps preferring the multiarch copy.
build_tree_sitter() {
    log "phase 2: libtree-sitter $TS_TAG"
    if [ -e /usr/local/lib/libtree-sitter.so.0 ] && \
       objdump -p /usr/local/lib/libtree-sitter.so.0 2>/dev/null | grep -q 'SONAME *libtree-sitter.so.0$'; then
        echo "already installed"; return
    fi
    [ -d "$SRC/tree-sitter/.git" ] || git clone --filter=blob:none https://github.com/tree-sitter/tree-sitter.git "$SRC/tree-sitter"
    git -C "$SRC/tree-sitter" checkout --quiet "$TS_TAG"
    make -C "$SRC/tree-sitter/lib" >/dev/null || make -C "$SRC/tree-sitter" >/dev/null
    make -C "$SRC/tree-sitter" install PREFIX=/usr/local >/dev/null
    local lib candidates=(/usr/local/lib/libtree-sitter.so.0.[0-9]*)
    lib="${candidates[0]}"
    patchelf --set-soname libtree-sitter.so.0 "$lib"
    ln -sf "$(basename "$lib")" /usr/local/lib/libtree-sitter.so.0
    ln -sf libtree-sitter.so.0 /usr/local/lib/libtree-sitter.so
    apt-get remove -y libtree-sitter-dev libtree-sitter0 >/dev/null 2>&1 || true
    ldconfig
}

# ---------------------------------------------------------------- phase 3
# Ubuntu tops out at Emacs 29.3, below init.el's 30.1 floor. Build the
# tagged release from the GitHub mirror (git.savannah and ftp.gnu.org are
# usually blocked where this script is needed). Native compilation is
# left at the default JIT setting; --with-native-compilation=aot would
# roughly triple the build for no runtime benefit here.
build_emacs() {
    log "phase 3: GNU Emacs $EMACS_TAG"
    if [ -x "$EMACS_BIN" ] && "$EMACS_BIN" --version | head -1 | grep -q '30\.\|31\.'; then
        echo "already installed: $("$EMACS_BIN" --version | head -1)"; return
    fi
    [ -d "$SRC/emacs-src/.git" ] || \
        git clone --depth 1 --branch "$EMACS_TAG" https://github.com/emacs-mirror/emacs.git "$SRC/emacs-src"
    cd "$SRC/emacs-src"
    [ -f configure ] || ./autogen.sh
    [ -f config.status ] || ./configure --prefix=/usr/local \
        --with-native-compilation=yes \
        --with-tree-sitter --with-modules \
        --with-x-toolkit=gtk3 --with-gnutls --with-sqlite3 \
        --with-xpm --with-jpeg --with-png --with-gif --with-tiff --with-rsvg \
        --without-xwidgets --without-mailutils
    make -j"$JOBS"
    make install
    cd - >/dev/null
}

# ---------------------------------------------------------------- phase 4
# The Nix distribution ships prebuilt grammars; here they are compiled
# from source into /usr/local/lib, which is on the dynamic loader path,
# so no treesit-extra-load-path wiring is needed. The language set mirrors
# jotain-prog-ts-remaps in lisp/init-prog.el plus the *-ts-mode files.
install_grammars() {
    log "phase 4: tree-sitter grammars"
    "$EMACS_BIN" -Q --batch --eval '
(progn
  (setq treesit-language-source-alist
        (quote ((bash "https://github.com/tree-sitter/tree-sitter-bash")
                (c "https://github.com/tree-sitter/tree-sitter-c")
                (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                (cmake "https://github.com/uyha/tree-sitter-cmake")
                (css "https://github.com/tree-sitter/tree-sitter-css")
                (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
                (go "https://github.com/tree-sitter/tree-sitter-go")
                (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
                (gowork "https://github.com/omertuc/tree-sitter-go-work")
                (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
                (json "https://github.com/tree-sitter/tree-sitter-json")
                (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown/src")
                (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown-inline/src")
                (nix "https://github.com/nix-community/tree-sitter-nix")
                (python "https://github.com/tree-sitter/tree-sitter-python")
                (rust "https://github.com/tree-sitter/tree-sitter-rust")
                (toml "https://github.com/tree-sitter-grammars/tree-sitter-toml")
                (tsx "https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")
                (typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")
                (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
                (zig "https://github.com/maxxnino/tree-sitter-zig"))))
  (dolist (e treesit-language-source-alist)
    (unless (treesit-language-available-p (car e))
      (condition-case err (treesit-install-language-grammar (car e) "/usr/local/lib")
        (error (message "GRAMMAR FAIL %s: %s" (car e) (error-message-string err)))))))' 2>&1 |
        grep -E 'GRAMMAR FAIL' || true
    ldconfig
}

# ---------------------------------------------------------------- phase 5
# Build a local MELPA-format archive. Package names are read out of the
# use-package forms in lisp/, so adding a package to the config and
# re-running this picks it up. Recipes come from the MELPA repo; the
# table below covers what MELPA either lacks (GNU ELPA packages) or
# hosts on a forge that is typically blocked (gitlab/codeberg/sourcehut),
# in which case the emacsmirror GitHub mirror stands in.
build_package_archive() {
    log "phase 5: local package archive"
    local rd="$SRC/recipes-all" pbw="$SRC/pb-working" pba="$SRC/pb-archive"
    [ -d "$SRC/melpa/.git" ] || {
        git clone --depth 1 --filter=blob:none --sparse https://github.com/melpa/melpa.git "$SRC/melpa"
        git -C "$SRC/melpa" sparse-checkout set recipes
    }
    [ -d "$SRC/package-build/.git" ] || \
        git clone --depth 1 https://github.com/melpa/package-build.git "$SRC/package-build"
    mkdir -p "$rd" "$pbw" "$pba"

    # Packages this config needs, read from the config itself.
    python3 - "$REPO_ROOT" "$rd" <<'PY'
import re, glob, os, sys, shutil, subprocess
repo, rd = sys.argv[1], sys.argv[2]
MELPA = '/usr/local/src/melpa/recipes'

# use-package blocks that are neither built-in nor installed from an
# archive by name: `:ensure nil' third-party packages the Nix overlay
# would otherwise supply, and the one `:ensure <other-package>' remap.
NIX_PROVIDED = {'claude-code-ide', 'combobulate', 'magit', 'majutsu',
                'nix-ts-mode', 'tagref'}

names = set()
for f in sorted(glob.glob(os.path.join(repo, 'lisp', '*.el'))):
    s = open(f, encoding='utf-8').read()
    for blk in re.split(r'\n\(use-package\s+', s)[1:]:
        name = re.match(r'([^\s\)\n]+)', blk).group(1)
        body = re.split(r'\n\(', blk)[0]
        m = re.search(r':ensure\s+([a-zA-Z0-9-]+)', body)
        if m:
            v = m.group(1)
            if v == 'nil':
                if name in NIX_PROVIDED:
                    names.add(name)
                continue
            if v != 't':
                names.add(v)        # e.g. (use-package dired-async :ensure async)
                continue
        names.add(name)

# Dependencies package.el resolves from the archive, so they must be in it.
names |= {'compat', 'with-editor', 'magit-section', 'ghub', 'closql', 'llama',
          'cond-let', 'yaml', 'treepy', 'transient', 'async', 'dash', 's', 'f',
          'datetime', 'extmap', 'elisp-refs', 'emacsql', 'go-mode', 'hcl-mode',
          'pcre2el', 'plz', 'request', 'shrink-path', 'tablist', 'web-server',
          'websocket', 'dired-hacks-utils',
          # vc-jj needs project >= 0.11.2; Emacs 30.1 bundles 0.11.1.
          'project'}

# GitHub sources for what MELPA lacks or hosts on a blocked forge.
OVERRIDE = {
  'ansible': 'emacsmirror/ansible',        'cmake-mode': 'emacsmirror/cmake-mode',
  'gitlab-ci-mode': 'emacsmirror/gitlab-ci-mode', 'mixed-pitch': 'emacsmirror/mixed-pitch',
  'zig-ts-mode': 'emacsmirror/zig-ts-mode', 'zoxide': 'emacsmirror/zoxide',
  'breadcrumb': 'joaotavora/breadcrumb',   'csv-mode': 'emacsmirror/csv-mode',
  'dape': 'svaante/dape',                  'denote': 'protesilaos/denote',
  'expreg': 'casouri/expreg',              'indent-bars': 'jdtsmith/indent-bars',
  'pulsar': 'protesilaos/pulsar',          'sql-indent': 'alex-hhh/emacs-sql-indent',
  'treesit-fold': 'emacs-tree-sitter/treesit-fold', 'vc-jj': 'emacsmirror/vc-jj',
  'vundo': 'casouri/vundo',                'compat': 'emacs-compat/compat',
  'claude-code-ide': 'manzaltu/claude-code-ide.el', 'combobulate': 'mickeynp/combobulate',
  'majutsu': '0WD0/majutsu',               'tagref': 'vedang/tagref.el',
  'project': 'emacs-straight/project',     'plz': 'alphapapa/plz.el',
  'magit-section': 'magit/magit',          'dired-hacks-utils': 'Fuco1/dired-hacks',
  'go-mode': 'dominikh/go-mode.el',        'emacsql': 'magit/emacsql',
  'shrink-path': 'emacsmirror/shrink-path',
}
names.discard('dired-async')   # shipped inside the `async' package

written = 0
for n in sorted(names):
    dest = os.path.join(rd, n)
    if n in OVERRIDE:
        open(dest, 'w').write('(%s :fetcher github :repo "%s")\n' % (n, OVERRIDE[n]))
        written += 1
    elif os.path.exists(os.path.join(MELPA, n)):
        shutil.copy(os.path.join(MELPA, n), dest); written += 1
    else:
        open(dest, 'w').write('(%s :fetcher github :repo "emacsmirror/%s")\n' % (n, n))
        written += 1
print("recipes: %d" % written)
PY

    # jylhis-emacs-themes is not on any archive; it lives in the design
    # repo at the revision nix/design-pin.nix pins, so the editor and the
    # website can never disagree on the design system.
    local pin_rev pin_ver
    pin_rev="$(sed -n 's/.*rev = "\([^"]*\)".*/\1/p' "$REPO_ROOT/nix/design-pin.nix" | head -1)"
    pin_ver="$(sed -n 's/.*version = "\([^"]*\)".*/\1/p' "$REPO_ROOT/nix/design-pin.nix" | head -1)"
    if [ ! -d "$SRC/design/.git" ]; then
        git clone --quiet https://github.com/Jylhis/design.git "$SRC/design"
    fi
    git -C "$SRC/design" fetch --quiet origin "$pin_rev" 2>/dev/null || true
    git -C "$SRC/design" checkout --quiet FETCH_HEAD 2>/dev/null || \
        git -C "$SRC/design" checkout --quiet "$pin_rev"

    "$EMACS_BIN" -Q --batch --eval "
(progn
  (add-to-list 'load-path \"$SRC/package-build\")
  (require 'package-build)
  (setq package-build-recipes-dir \"$rd\"
        package-build-working-dir \"$pbw/\"
        package-build-archive-dir \"$pba/\"
        package-build--use-sandbox nil)
  (dolist (name (package-recipe-recipes))
    (condition-case err (package-build-archive name nil t)
      (error (message \"PKG FAIL %s :: %s\" name (error-message-string err)))))
  (package-build-dump-archive-contents))" 2>&1 | grep -E '^PKG FAIL' || true

    # Themes: assembled by hand into a tarball rather than via a recipe,
    # because package-build's git fetcher will not read a local checkout.
    local stage="$SRC/themes-stage/jylhis-emacs-themes-$pin_ver"
    rm -rf "$SRC/themes-stage"; mkdir -p "$stage"
    cp "$SRC/design/platforms/emacs/"*.el "$stage/"
    cat >"$stage/jylhis-emacs-themes-pkg.el" <<PKG
(define-package "jylhis-emacs-themes" "$pin_ver"
  "Jylhis design-system themes for Emacs (Sheet/Field)" '((emacs "27.1")))
PKG
    tar -cf "$SRC/jylhis-emacs-themes.tar" -C "$SRC/themes-stage" "jylhis-emacs-themes-$pin_ver"
}

# ---------------------------------------------------------------- phase 6
# Install from the local archive into the repo's gitignored elpa/. Once
# these are present, `use-package-always-ensure' is satisfied from disk
# and startup never reaches for the network — which matters because
# elpa.gnu.org/melpa.org are exactly what is unreachable here.
install_packages() {
    log "phase 6: install packages into $REPO_ROOT/elpa"
    "$EMACS_BIN" -Q --batch --eval "
(progn
  (require 'package)
  (setq package-user-dir \"$REPO_ROOT/elpa\"
        package-archives '((\"local\" . \"$SRC/pb-archive/\"))
        package-check-signature nil
        native-comp-jit-compilation nil
        byte-compile-warnings nil)
  (package-initialize)
  (package-refresh-contents)
  (dolist (e package-archive-contents)
    (let ((n (car e)))
      (condition-case err
          (unless (package-installed-p n) (package-install n))
        (error (message \"INSTALL FAIL %s :: %s\" n (error-message-string err))))))
  (unless (package-installed-p 'jylhis-emacs-themes)
    (package-install-file \"$SRC/jylhis-emacs-themes.tar\")))" 2>&1 |
        grep -E '^INSTALL FAIL' || true

    # vc-jj's autoloads push onto project-vc-backend-markers-alist at
    # load time, before project.el has defined it — void-variable on
    # every startup. Load project first.
    local al
    al="$(ls "$REPO_ROOT"/elpa/vc-jj-*/vc-jj-autoloads.el 2>/dev/null | head -1)"
    if [ -n "$al" ] && ! grep -q "require 'project" "$al"; then
        python3 - "$al" <<'PY'
import sys
f = sys.argv[1]; s = open(f).read()
old = "(add-to-list 'project-vc-backend-markers-alist '(JJ . \".jj\"))"
new = "(progn (require 'project nil t)\n  (add-to-list 'project-vc-backend-markers-alist '(JJ . \".jj\")))"
if old in s:
    open(f, 'w').write(s.replace(old, new))
PY
        rm -f "${al}c"
    fi
}

# ---------------------------------------------------------------- phase 7
# init-ui.el prefers BlexMono/JetBrainsMono Nerd Font and falls back to
# plain monospace; without a patched font the nerd-icons glyphs in
# dirvish, corfu and doom-modeline render as tofu.
install_fonts() {
    log "phase 7: Nerd Fonts"
    local dir=/usr/local/share/fonts/nerd
    if [ -f "$dir/BlexMonoNerdFont-Regular.ttf" ]; then echo "already installed"; return; fi
    mkdir -p "$dir"
    local base=https://github.com/ryanoasis/nerd-fonts/releases/latest/download
    for f in JetBrainsMono IBMPlexMono; do
        curl -sSL --max-time 300 -o "/tmp/$f.zip" "$base/$f.zip" || continue
    done
    unzip -o -j /tmp/JetBrainsMono.zip 'JetBrainsMonoNerdFont-{Regular,Bold,Italic,BoldItalic}.ttf' -d "$dir" >/dev/null 2>&1 || true
    unzip -o -j /tmp/IBMPlexMono.zip 'BlexMonoNerdFont-{Regular,Bold,Italic,BoldItalic}.ttf' -d "$dir" >/dev/null 2>&1 || true
    rm -f /tmp/JetBrainsMono.zip /tmp/IBMPlexMono.zip
    fc-cache -f >/dev/null
}

# ---------------------------------------------------------------- launcher
# Mirrors `just run-built': point Emacs at this checkout instead of
# ~/.emacs.d. JOTAIN_NO_PACKAGE_REFRESH is set because init.el's idle
# archive warm-up has nothing reachable to refresh from on such a host.
write_launcher() {
    log "launcher: /usr/local/bin/jotain"
    cat >/usr/local/bin/jotain <<LAUNCH
#!/usr/bin/env bash
# Launch Emacs with the Jotain configuration from $REPO_ROOT.
exec env JOTAIN_NO_PACKAGE_REFRESH=1 \\
    /usr/local/bin/emacs --init-directory=$REPO_ROOT "\$@"
LAUNCH
    chmod +x /usr/local/bin/jotain
}

main() {
    require_root
    apt_packages
    build_tree_sitter
    build_emacs
    install_grammars
    build_package_archive
    install_packages
    install_fonts
    write_launcher
    log "done — run: jotain            (GUI/tty Emacs with this config)"
    echo   "        or: jotain -nw       (terminal frame)"
}

main "$@"
