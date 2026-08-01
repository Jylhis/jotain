# Jotain Emacs configuration — task runner.
#
# All recipes assume the devenv shell is active. Enter it with
# `devenv shell`, or prefix any command with `devenv shell --`,
# e.g. `devenv shell -- just check`. direnv users can create their
# own (untracked) .envrc with `eval "$(devenv direnvrc)"` + `use devenv`.

config_dir := justfile_directory()

# Emacs build flavours target the current system by default. Override
# with `just system=x86_64-linux build-nox` etc.
system := arch() + "-" + if os() == "macos" { "darwin" } else { "linux" }

# List available recipes.
default:
    @just --list --justfile "{{justfile()}}"


# ── Run ─────────────────────────────────────────────────────────────
#
# All recipes in this section depend on `emacs` / `emacsclient` being
# on PATH. Emacs is temporarily not installed into the devenv shell
# (see top-of-file note in devenv.nix), so they print a notice and
# exit 1 — a stub must never look like a passing run to a script or
# an agent. Use `just run-built` to build Emacs via Nix and launch it
# with this configuration.

# [DISABLED] Launch Emacs with this config in isolation (--init-directory).
[group('run')]
run *ARGS:
    @echo "just run is disabled — emacs is not in the devenv shell."
    @echo "Try: just run-built  (builds Emacs via Nix, then launches it)"
    @exit 1
# Original:
#   emacs --init-directory={{config_dir}} {{ARGS}}

# [DISABLED] Launch with --debug-init and debug-on-error.
[group('run')]
debug *ARGS:
    @echo "just debug is disabled — emacs is not in the devenv shell."
    @echo "Try: just run-built-debug  (builds Emacs via Nix, --debug-init)"
    @exit 1
# Original:
#   emacs --init-directory={{config_dir}} --debug-init \
#         --eval '(setq debug-on-error t)' {{ARGS}}

# [DISABLED] Launch in the terminal (-nw) — exercises kkp + clipetty.
[group('run')]
tty *ARGS:
    @echo "just tty is disabled — emacs is not in the devenv shell."
    @exit 1
# Original:
#   emacs -nw --init-directory={{config_dir}} {{ARGS}}

# [DISABLED] Run a foreground Emacs daemon with this config.
[group('run')]
daemon *ARGS:
    @echo "just daemon is disabled — emacs is not in the devenv shell."
    @exit 1
# Original:
#   emacs --fg-daemon --init-directory={{config_dir}} {{ARGS}}

# [DISABLED] Connect a graphical emacsclient frame to the running daemon.
[group('run')]
client *ARGS:
    @echo "just client is disabled — emacsclient is not in the devenv shell."
    @exit 1
# Original:
#   emacsclient -c --alternate-editor='emacs --init-directory={{config_dir}}' {{ARGS}}

# [DISABLED] Connect a terminal emacsclient frame to the running daemon.
[group('run')]
client-tty *ARGS:
    @echo "just client-tty is disabled — emacsclient is not in the devenv shell."
    @exit 1
# Original:
#   emacsclient -t --alternate-editor='emacs -nw --init-directory={{config_dir}}' {{ARGS}}

# [DISABLED] Lightweight `emacs -Q -nw` for quick edits.
[group('run')]
quick *ARGS:
    @echo "just quick is disabled — emacs is not in the devenv shell."
    @exit 1
# Original:
#   emacs -Q -nw --eval "(load-theme 'wombat t)" {{ARGS}}


# ── Check / compile ─────────────────────────────────────────────────

# Run all checks: eval, flake, devenv, linting.
[group('check')]
check:
    nix flake check

# Equivalent coverage lives in the `elisp-lint` flake check; `just check`
# runs it via `nix flake check`.
# [DISABLED] Parse every .el file (no compile, no package install).
[group('check')]
check-elisp:
    @echo "just check-elisp is disabled — emacs is not in the devenv shell."
    @echo "Equivalent: just check  (runs the elisp-lint flake check)"
    @exit 1
# Original:
#   emacs -Q --batch --eval '(check-parens for early-init.el init.el lisp/init-*.el)'

# Equivalent coverage lives in the `elisp-compile` flake check.
# [DISABLED] Byte-compile everything; requires packages installed.
[group('check')]
compile:
    @echo "just compile is disabled — emacs is not in the devenv shell."
    @echo "Equivalent: just check  (runs the elisp-compile flake check)"
    @exit 1
# Original:
#   emacs --batch --init-directory={{config_dir}} \
#       --eval '(setq byte-compile-error-on-warn t)' \
#       --eval '(byte-recompile-directory "{{config_dir}}/lisp" 0 t)' \
#       -f batch-byte-compile early-init.el init.el

# [DISABLED] Native-compile every config module ahead of time.
#
# This recipe's premise — that AOT coverage "belongs in the Nix /
# home-manager deploy of the config" — is now implemented, so there is
# nothing interim left to warm in-tree. nix/config-compiled.nix takes a
# `nativeCompile' flag that emits .eln into the store, and module.nix
# exposes it as `services.jotain.nativeCompile.enable'. Read the gate
# comment in nix/config-compiled.nix before turning it on.
[group('check')]
compile-native:
    @echo "just compile-native is disabled — AOT now happens in the Nix build."
    @echo "Enable it with: services.jotain.nativeCompile.enable = true;"
    @echo "See the gate command in nix/config-compiled.nix first."
    @exit 1

# Run the ERT tests under test/ via the flake check (the dev shell has
# no emacs; the check builds one). Direct equivalent once Emacs is back
# in the shell:
#   emacs --batch -L lisp -L test \
#       --eval '(dolist (f (directory-files "test" t "\\.el$")) (load f nil t))' \
#       -l ert -f ert-run-tests-batch-and-exit
[group('check')]
test:
    nix build .#checks.{{system}}.elisp-test --no-link --print-build-logs


# Wrapper init files live in bench/ — kept on disk for when re-enabled.
# [DISABLED] Benchmark startup: launch Emacs, collect metrics, print results.
[group('check')]
bench output="":
    @echo "just bench is disabled — emacs is not in the devenv shell."
    @exit 1
# Original:
#   JOTAIN_BENCH_OUTPUT=... emacs --init-directory={{config_dir}}/bench

# [DISABLED] Benchmark file-open: open representative files and time each hook.
[group('check')]
bench-open output="":
    @echo "just bench-open is disabled — emacs is not in the devenv shell."
    @exit 1
# Original:
#   JOTAIN_BENCH_OPEN_OUTPUT=... emacs --init-directory={{config_dir}}/bench


# ── Build (nix) ─────────────────────────────────────────────────────

# Build the full distribution (Emacs + every grammar) for the current system.
[group('build')]
build:
    nix-build

# Build a bare Emacs (no tree-sitter grammars).
[group('build')]
build-bare:
    nix-build --argstr system {{system}} emacs.nix

# Build the full distribution with only the grammars this config uses (~26 vs ~275; smaller, lighter to build). Opt-in.
[group('build')]
build-lite:
    nix build .#emacs-lite -o result-emacs-lite

# Build a bare Emacs from source, trimmed (no xwidgets/mailutils) for fewer build inputs. Opt-in.
[group('build')]
build-bare-lite:
    nix-build --arg withXwidgets false --arg withMailutils false \
        --argstr system {{system}} emacs.nix

# Build with --with-pgtk for Wayland.
[group('build')]
build-pgtk:
    nix-build --arg withPgtk true --argstr system {{system}} emacs.nix

# Build with --with-x-toolkit=gtk3 (X11 + GTK3).
[group('build')]
build-gtk3:
    nix-build --arg withGTK3 true --argstr system {{system}} emacs.nix

# Build the X11/Lucid escape hatch (unstable/Emacs 31, pgtk off) — the
# pre-switch Linux GUI backend, for when the pgtk default misbehaves.
[group('build')]
build-x11:
    nix-build --arg variant '"unstable"' --arg withPgtk false --argstr system {{system}} emacs.nix

# Build a terminal-only Emacs (--without-x --without-ns).
[group('build')]
build-nox:
    nix-build --arg noGui true --argstr system {{system}} emacs.nix

# Build the macport variant (Darwin only).
[group('build')]
build-macport:
    nix-build --arg variant '"macport"' --argstr system {{system}} emacs.nix

# Build from git master (the revision pinned by emacs-overlay, binary-cached).
[group('build')]
build-git:
    nix-build --arg variant '"git"' --argstr system {{system}} emacs.nix

# Build the IGC (Memory Pool System GC) branch.
[group('build')]
build-igc:
    nix-build --arg variant '"igc"' --argstr system {{system}} emacs.nix

# Build IGC with ccache. On Darwin, nix-community.cachix.org has no
# prebuilt igc, so plain build-igc is already a from-source build there
# (plan.md §3) — ccache makes repeat local rebuilds cheaper. Needs a
# ccache sandbox exception set up first; see the useCcache doc comment
# in emacs.nix. No-op improvement elsewhere: everywhere else, igc is a
# cache hit and this just adds ccache overhead for nothing.
[group('build')]
build-igc-ccache:
    nix-build --arg variant '"igc"' --arg useCcache true --argstr system {{system}} emacs.nix

# Build a bare aarch64-linux nox Emacs (Termux/Android) — kept for
# cache-parity testing of emacs.nix; `run-built` uses build-nox-full.
[group('build')]
build-android:
    nix-build --arg noGui true --argstr system aarch64-linux emacs.nix

# Build the full terminal-only distribution (noGui Emacs + packages +
# grammars) for the current system.
[group('build')]
build-nox-full:
    nix build .#emacs-nox -o result

# Auto-detect platform, build, then launch Emacs with this configuration.
[group('build')]
run-built *ARGS:
    #!/usr/bin/env bash
    set -euo pipefail
    platform="$(uname -s)-$(uname -m)"
    case "$platform" in
        Darwin-arm64)  target=build       ;;
        Darwin-*)      target=build       ;;
        Linux-aarch64) target=build-nox-full ;;
        *)             target=build       ;;
    esac
    echo "Platform: $platform → just $target"
    just "$target"
    echo "Build output: $(readlink result)"
    echo "Launching Emacs from result/bin/emacs..."
    ./result/bin/emacs --init-directory="{{config_dir}}" {{ARGS}}

# Same, with init debugging enabled.
[group('build')]
run-built-debug *ARGS:
    just run-built --debug-init --eval '(setq debug-on-error t)' {{ARGS}}

# Like run-built-debug, but with every debugging facility on (--debug-init,
# debug-on-error, unbounded *Messages*, verbose warnings + native-comp,
# full eglot/jsonrpc traffic) and every message, warning and error backtrace
# mirrored into var/debug/<timestamp>/ (gitignored via var/). stderr is teed
# there live too; stdout stays on the tty so a GUI or -nw Emacs both work.
# Inspect the files afterwards, or M-x jotain-debug-dump-now to flush mid-run.
[group('build')]
[doc('Launch with full debugging on; logs to var/debug/<timestamp>/')]
run-built-debug-log *ARGS:
    #!/usr/bin/env bash
    set -euo pipefail
    ts="$(date +%Y%m%d-%H%M%S)"
    dir="{{config_dir}}/var/debug/$ts"
    mkdir -p "$dir"
    export JOTAIN_DEBUG_DIR="$dir"
    echo "Debug session → $dir"
    # Redirect only fd 2 through tee: stderr (build logs + Emacs native-comp /
    # GTK / --debug-init output) is saved and still shown, without touching
    # stdout, so the interactive Emacs tty is left intact.
    exec 2> >(tee "$dir/stderr.log" >&2)
    just run-built --debug-init --load "{{config_dir}}/etc/debug-init.el" {{ARGS}}

# Headless screenshot: build Emacs, launch under Xvfb with this config,
# capture the frame via jotain-screenshot, write OUT (PNG). Linux only;
# needs xvfb-run from the devenv shell. First run is slow: nix build
# (cache pull) + MELPA package bootstrap — raise the timeout if a cold
# cache needs it.
[group('build')]
[linux]
screenshot out="var/screenshots/headless.png":
    #!/usr/bin/env bash
    set -euo pipefail
    command -v xvfb-run >/dev/null 2>&1 || {
        echo "xvfb-run not on PATH — enter the devenv shell (direnv/devenv shell)"; exit 1; }
    just build
    out="{{out}}"
    case "$out" in /*) ;; *) out="{{config_dir}}/$out" ;; esac
    mkdir -p "$(dirname "$out")"
    JOTAIN_SCREENSHOT_OUT="$out" timeout 600 xvfb-run -a -s '-screen 0 1920x1080x24' \
        ./result/bin/emacs --init-directory="{{config_dir}}" \
        --eval '(run-at-time 3 nil (lambda ()
                  (condition-case err
                      (progn (jotain-screenshot (getenv "JOTAIN_SCREENSHOT_OUT"))
                             (kill-emacs 0))
                    (error (message "jotain-screenshot failed: %S" err)
                           (kill-emacs 2)))))'
    test -s "$out" || { echo "FAIL: no screenshot written"; exit 1; }
    echo "Screenshot → $out"


# Build option reference documentation (HTML for GitHub Pages).
[group('build')]
docs:
    nix build .#docs -o result-docs
    @echo "Docs built → result-docs/index.html"

# Build the bundled Info manual (jotain.info) from docs/*.mdx + options.
# Loaded automatically by init-docs.el when result-info/ exists.
[group('build')]
info:
    nix build .#info -o result-info
    @echo "Info manual → result-info/share/info/jotain.info"
    @echo "Open with 'just run-built' then C-h i d m Jotain RET."

# Build the per-package reference (HTML + texi + Mintlify .mdx).
[group('build')]
build-packages-doc:
    nix build .#packages-doc -o result-packages-doc
    @echo "Packages doc → result-packages-doc/index.html"

# Regenerate docs/configuration/package-reference.mdx from the
# `;;; @doc` markers in lisp/init-*.el. Run after editing any
# `;;; @doc` block; CI's `packages-doc-in-sync` check will fail
# otherwise.
[group('build')]
docs-refresh-packages: build-packages-doc
    cp result-packages-doc/package-reference.mdx \
       docs/configuration/package-reference.mdx
    @echo "Refreshed docs/configuration/package-reference.mdx"

# Build both HTML docs and the Info manual.
[group('build')]
docs-all: docs info

# Build the full jotain.j10s.io site: landing SPA + docs pages +
# manual (HTML/Info) + man pages + GNU Emacs/Elisp manuals + options
# and package references. Deployed from the `site` branch by the
# Cloudflare GitHub integration (see .github/workflows/deploy.yml).
[group('build')]
site:
    nix build .#site -o result-site
    @echo "Site → result-site/public/index.html"

# Build and locally serve the full site.
[group('build')]
serve-site: site
    python3 -m http.server -d result-site/public 8080


# ── Site deployment ─────────────────────────────────────────────────

# Remote and branch the built site is published to. Cloudflare Workers
# Builds watches `site_branch`; override either for a test deploy.
site_remote := "origin"
site_branch := "site"

# Builds `.#site` and force-pushes the output as a single orphan commit
# to `site_branch`. Nothing from the source tree is carried over, so
# the branch is exactly what Cloudflare serves: wrangler.jsonc at the
# root, the site under public/. deploy.yml runs this on push to main.
#
#   just deploy-site                        # build + push to origin/site
#   just site_branch=site-preview deploy-site
#   DRY_RUN=1 just deploy-site              # build + commit, no push
#
# The commit is written with plumbing inside this repo, so the push uses
# whatever credentials the repo already has (a local SSH remote, or the
# token actions/checkout configured in CI).
#
# Build and publish the full site to the `site` deploy branch.
[group('deploy')]
deploy-site:
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{ config_dir }}"

    sha=$(git rev-parse HEAD)
    if [ -n "$(git status --porcelain --untracked-files=no)" ]; then
        echo "warning: tracked files are modified, so the build is not $sha as committed" >&2
    fi

    out=$(nix build --no-link --print-out-paths .#site)

    # Stage the store output as a work tree with its own index, then
    # commit it parentless. -f because the store copy carries no
    # .gitignore and a global excludes file must not eat site files.
    tmp=$(mktemp -d)
    trap 'rm -rf "$tmp"' EXIT
    cp -rL "$out/." "$tmp/dist"
    chmod -R u+w "$tmp/dist"

    export GIT_INDEX_FILE="$tmp/index"
    git --work-tree="$tmp/dist" add -Af "$tmp/dist"
    tree=$(git write-tree)
    unset GIT_INDEX_FILE

    export GIT_AUTHOR_NAME="${GIT_AUTHOR_NAME:-$(git config user.name || echo jotain-site)}"
    export GIT_AUTHOR_EMAIL="${GIT_AUTHOR_EMAIL:-$(git config user.email || echo noreply@jylhis.com)}"
    export GIT_COMMITTER_NAME="$GIT_AUTHOR_NAME"
    export GIT_COMMITTER_EMAIL="$GIT_AUTHOR_EMAIL"
    commit=$(git commit-tree "$tree" -m "site: build from $sha")

    if [ -n "${DRY_RUN:-}" ]; then
        echo "DRY_RUN: built $commit from $sha, not pushing. Branch root:"
        git ls-tree --name-only "$commit" | sed 's/^/  /'
        echo "  ($(git ls-tree -r --name-only "$commit" | wc -l) files total)"
        exit 0
    fi

    git push --force "{{ site_remote }}" "$commit:refs/heads/{{ site_branch }}"
    echo "Deployed $sha → {{ site_remote }}/{{ site_branch }} ($commit)"


# ── Format ──────────────────────────────────────────────────────────

# Format all Nix files.
[group('format')]
fmt:
    nix fmt


# ── Lock synchronization ────────────────────────────────────────────

# Inputs shared between flake.nix and devenv.yaml — both locks must agree on these revs.
shared_inputs := "nixpkgs treefmt-nix emacs-overlay"

# Update flake inputs, then sync devenv.yaml/devenv.lock to the new revs.
[group('pins')]
update:
    #!/usr/bin/env bash
    set -euo pipefail
    nix flake update
    just sync-devenv all
    echo "Done."

# Sync devenv.yaml/devenv.lock to the revs already in flake.lock.
[group('pins')]
sync-devenv scope="shared":
    #!/usr/bin/env bash
    # Deliberately does NOT run `nix flake update`, so it is safe on a
    # Dependabot PR — those bump flake.lock alone, and
    # .github/workflows/sync-devenv.yml runs exactly this recipe to make
    # such a PR self-consistent.
    #
    # scope=shared (default) re-locks only the shared inputs. scope=all
    # re-resolves every devenv input including the unpinned `devenv`
    # module input itself, which is what `just update` has always done —
    # fine with a human reading the diff, but unreviewed drift in an
    # automated commit.
    set -euo pipefail
    case "{{ scope }}" in
        shared | all) ;;
        *)
            echo "ERROR: scope must be 'shared' or 'all', got '{{ scope }}'" >&2
            exit 1
            ;;
    esac
    tmpfile=$(mktemp)
    cp devenv.yaml "$tmpfile"
    for input in {{ shared_inputs }}; do
        node=$(jq -r ".nodes.root.inputs.\"$input\" // empty" flake.lock)
        if [ -z "$node" ]; then
            echo "ERROR: input '$input' missing from flake.lock root inputs" >&2
            exit 1
        fi
        owner=$(jq -r ".nodes.\"$node\".locked.owner" flake.lock)
        repo=$(jq -r ".nodes.\"$node\".locked.repo" flake.lock)
        rev=$(jq -r ".nodes.\"$node\".locked.rev" flake.lock)
        echo "Syncing devenv.yaml: $input -> $rev"
        sed -i.bak "s|url: github:$owner/$repo/[^[:space:]]*|url: github:$owner/$repo/$rev|" "$tmpfile"
        rm -f "$tmpfile.bak"
    done
    mv "$tmpfile" devenv.yaml
    if [ "{{ scope }}" = "all" ]; then
        devenv update
    else
        for input in {{ shared_inputs }}; do
            devenv update "$input"
        done
    fi

# Verify that flake.lock and devenv.lock agree on every shared input's rev.
[group('pins')]
verify:
    #!/usr/bin/env bash
    # Shares one implementation with the `locks-in-sync` flake check.
    set -euo pipefail
    SHARED_INPUTS="{{ shared_inputs }}" bash scripts/verify-locks.sh .

# Re-vendor website/public/ds from the jylhis/design rev pinned in
# nix/design-pin.nix — the same pin the Emacs themes are built from.
# Run after bumping that pin; the `ds-in-sync` flake check fails until
# the committed copy matches.
[group('pins')]
ds-sync:
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{config_dir}}"
    out=$(nix build --no-link --print-out-paths .#ds-assets)
    # Wipe first: this is what removes fonts retired by an upstream type
    # change (v2 dropped all eight Literata/JetBrains Mono slices).
    rm -rf website/public/ds
    mkdir -p website/public/ds
    cp -r "$out/." website/public/ds/
    chmod -R u+w website/public/ds
    echo "Re-vendored website/public/ds from $(nix eval --raw --file nix/design-pin.nix rev)"


# ── Cleanup ─────────────────────────────────────────────────────────

# Remove .elc files, autosaves, and the eln-cache.
[group('clean')]
clean:
    #!/usr/bin/env bash
    set -euo pipefail
    find "{{config_dir}}" -name '*.elc' -type f -delete 2>/dev/null || true
    find "{{config_dir}}" -name '*~'    -type f -delete 2>/dev/null || true
    find "{{config_dir}}" -name '#*#'   -type f -delete 2>/dev/null || true
    find "{{config_dir}}" -name '.#*'   -type f -delete 2>/dev/null || true
    rm -rf "{{config_dir}}/var/eln-cache" 2>/dev/null || true
    rm -rf "{{config_dir}}/eln-cache"     2>/dev/null || true
    rm -f  "{{config_dir}}/result"        2>/dev/null || true
    echo "Cleaned compiled artifacts."

# Nuke installed packages and persistent state — forces a full re-fetch.
[group('clean')]
clean-all: clean
    #!/usr/bin/env bash
    set -euo pipefail
    rm -rf "{{config_dir}}/elpa"      2>/dev/null || true
    rm -rf "{{config_dir}}/var"       2>/dev/null || true
    rm -rf "{{config_dir}}/.dev-home" 2>/dev/null || true
    echo "Nuked elpa/, var/, .dev-home/."
