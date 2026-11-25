# Run recipes inside the dev shell (`nix develop`).

config_dir := justfile_directory()

# List available recipes.
default:
    @just --list --justfile "{{justfile()}}"

# Benchmark startup of the Nix-built Emacs (needs a display; xvfb-run works).
[group('build')]
bench-built output="var/bench/startup.txt":
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{config_dir}}"
    just build
    out="{{output}}"; case "$out" in /*) ;; *) out="{{config_dir}}/$out" ;; esac
    mkdir -p "$(dirname "$out")"
    echo "Benchmarking → $out"
    JOTAIN_BENCH_OUTPUT="$out" \
        ./result/bin/emacs --init-directory="{{config_dir}}/bench"
    echo
    cat "$out"


# Build the full distribution (Emacs + every grammar).
[group('build')]
build:
    nix-build

# Build, then launch Emacs with this configuration.
[group('build')]
run-built *ARGS:
    #!/usr/bin/env bash
    set -euo pipefail
    just build
    echo "Build output: $(readlink result)"
    echo "Launching Emacs from result/bin/emacs..."
    ./result/bin/emacs --init-directory="{{config_dir}}" {{ARGS}}

# Launch the AOT-compiled config (.elc + store .eln); reflects the last build.
[group('build')]
run-built-fast *ARGS:
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{config_dir}}"
    just build
    echo "Emacs:           $(readlink result)"
    store=$(nix build --no-link --print-out-paths .#config-compiled)
    echo "Compiled config: $store"
    home="{{config_dir}}/var/fast-home"
    mkdir -p "$home" "{{config_dir}}/var"
    for f in early-init.el early-init.elc init.el init.elc lisp; do
        ln -sfn "$store/$f" "$home/$f"
    done
    ln -sfn "{{config_dir}}/var" "$home/var"
    ln -sfn "{{config_dir}}/templates" "$home/templates"
    [ -e "{{config_dir}}/elpa" ] && ln -sfn "{{config_dir}}/elpa" "$home/elpa" || true
    export JOTAIN_ELN_PATH="$store/share/emacs/native-lisp"
    exec ./result/bin/emacs --init-directory="$home" {{ARGS}}

# Like run-built, with --debug-init and debug-on-error.
[group('build')]
run-built-debug *ARGS:
    just run-built --debug-init --eval '(setq debug-on-error t)' {{ARGS}}

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
    exec 2> >(tee "$dir/stderr.log" >&2)
    just run-built --debug-init --load "{{config_dir}}/etc/debug-init.el" {{ARGS}}

# Headless screenshot under Xvfb via jotain-screenshot, saved to `out` (PNG).
[group('build')]
[linux]
screenshot out="var/screenshots/headless.png":
    #!/usr/bin/env bash
    set -euo pipefail
    command -v xvfb-run >/dev/null 2>&1 || {
        echo "xvfb-run not on PATH — enter the dev shell (nix develop)"; exit 1; }
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


# Build the site and serve it locally at the root.
[group('build')]
serve-site:
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{config_dir}}"
    nix build .#site -o result-site
    echo "Serving → http://localhost:8080/"
    python3 -m http.server -d "{{config_dir}}/result-site/public" 8080

# Build the site and deploy it to the emacs-docs Worker (needs CLOUDFLARE_API_TOKEN and CLOUDFLARE_ACCOUNT_ID).
[group('build')]
deploy:
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{config_dir}}"
    out=$(nix build .#site --no-link --print-out-paths)
    dir=$(mktemp -d)
    trap 'rm -rf "$dir"' EXIT
    cp -RL "$out/public" "$dir/public"
    chmod -R u+w "$dir/public"
    cd website
    nix shell nixpkgs#wrangler --command wrangler deploy --assets "$dir/public"

# Bump hand-pinned upstreams with nix-update; optional names scope it.
[group('pins')]
update-pins *PINS:
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{config_dir}}"
    branch="claude-code-ide eglot-booster combobulate qml-ts-mode tagref jylhis-emacs-themes"
    tagged="eca"
    script="likec4-lsp"
    wanted="{{PINS}}"
    want() { [ -z "$wanted" ] || [[ " $wanted " == *" $1 "* ]]; }
    nu() { nix run nixpkgs#nix-update -- --flake "$@"; }
    for n in $branch; do
        want "$n" && nu --version=branch "legacyPackages.x86_64-linux.emacs-packages.$n"
    done
    for n in $tagged; do
        want "$n" && nu "$n"
    done
    for n in $script; do
        want "$n" && nu --use-update-script "$n"
    done
    true


# Remove .elc files, autosaves, the eln-cache and result links.
[group('clean')]
clean:
    #!/usr/bin/env bash
    set -euo pipefail
    find "{{config_dir}}" -name '*.elc' -type f -delete 2>/dev/null || true
    find "{{config_dir}}" -name '*~'    -type f -delete 2>/dev/null || true
    find "{{config_dir}}" -name '#*#'   -type f -delete 2>/dev/null || true
    find "{{config_dir}}" -name '.#*'   -type f -delete 2>/dev/null || true
    rm -rf "{{config_dir}}/var/eln-cache" 2>/dev/null || true
    rm -f  "{{config_dir}}"/result "{{config_dir}}"/result-* 2>/dev/null || true
    echo "Cleaned compiled artifacts."

# Run clean, then remove elpa/ and var/ (forces a full re-fetch).
[group('clean')]
clean-all: clean
    #!/usr/bin/env bash
    set -euo pipefail
    rm -rf "{{config_dir}}/elpa"      2>/dev/null || true
    rm -rf "{{config_dir}}/var"       2>/dev/null || true
    echo "Nuked elpa/ and var/."
