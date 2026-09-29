#!/usr/bin/env bash
# Bump the hand-pinned upstream sources in nix/ (the pins flake.lock does
# not manage) to their latest release. See "Hand-pinned upstreams" in
# AGENTS.md for the list of pins, how each is bumped, and the two that
# stay manual by design.
#
# Usage:
#   scripts/update-pins.sh                 # update everything
#   scripts/update-pins.sh combobulate eca # update only the named pins
#   scripts/update-pins.sh --list          # list the known pin names
#   NO_BUILD=1 scripts/update-pins.sh ...  # skip the post-update build
set -euo pipefail

cd "$(dirname "$0")/.."

system="$(nix eval --impure --raw --expr builtins.currentSystem)"
epkgs="legacyPackages.${system}.emacs-packages"
nix_update=(nix run nixpkgs#nix-update --)
build_flag="--build"
[ "${NO_BUILD:-0}" = "1" ] && build_flag=""

# name -> how to update it. "tag" / "branch" go through nix-update; the
# rest name a bespoke function below.
declare -A PINS=(
  [combobulate]="tag"
  [project-nix-store]="tag"
  [claude-code-ide]="branch"
  [eglot-booster]="branch"
  [qml-ts-mode]="branch"
  [tagref]="branch"
  [majutsu]="branch" # tags lag the fixes we want; track HEAD
  [eca]="fn:update_eca"
  [likec4-lsp]="fn:update_likec4"
  [ellsp]="fn:update_ellsp"
  [design]="fn:update_design"
)

log() { printf '\n# %s\n' "$*"; }

# --- nix-update-driven Emacs packages -------------------------------------

update_via_nix_update() {
  local name="$1" mode="$2"
  local args=(--flake)
  [ "$mode" = "branch" ] && args+=(--version=branch)
  [ -n "$build_flag" ] && args+=("$build_flag")
  log "nix-update ($mode): $name"
  "${nix_update[@]}" "${args[@]}" "${epkgs}.${name}"
}

# --- eca: prebuilt server, four-platform sidecar-hash table ---------------

update_eca() {
  local file="nix/eca-server.nix"
  local tag
  tag="$(gh api repos/editor-code-assistant/eca/releases/latest --jq .tag_name)"
  log "eca -> ${tag}"
  # Same platform->asset map as the nix file.
  local -A assets=(
    [x86_64-linux]="eca-native-static-linux-amd64.zip"
    [aarch64-linux]="eca-native-linux-aarch64.zip"
    [aarch64-darwin]="eca-native-macos-aarch64.zip"
    [x86_64-darwin]="eca-native-macos-amd64.zip"
  )
  local base="https://github.com/editor-code-assistant/eca/releases/download/${tag}"
  local sys asset hex
  for sys in "${!assets[@]}"; do
    asset="${assets[$sys]}"
    hex="$(curl -fsSL "${base}/${asset}.sha256" | awk '{print $1}')"
    [ "${#hex}" -eq 64 ] || { echo "eca: bad sha for $asset" >&2; exit 1; }
    perl -0pi -e "s/(asset = \"\Q${asset}\E\";\s*\n\s*sha256 = \")[0-9a-f]{64}(\")/\${1}${hex}\${2}/" "$file"
  done
  perl -pi -e "s/^(  version = \")[^\"]*(\";)/\${1}${tag}\${2}/" "$file"
  [ -n "$build_flag" ] && build_eca
}

build_eca() {
  log "build: eca"
  nix build --no-link --impure --expr '
    let f = builtins.getFlake (toString ./.);
        pkgs = import f.inputs.nixpkgs { system = builtins.currentSystem; };
    in import ./nix/eca-server.nix { inherit pkgs; }'
}

# --- likec4-lsp: vendored npm wrapper, regenerated lockfile ---------------

update_likec4() {
  local dir="nix/likec4-lsp" file="nix/likec4-lsp.nix" ver
  ver="$(curl -fsSL https://registry.npmjs.org/@likec4/lsp/latest | jq -r .version)"
  log "likec4-lsp -> ${ver}"
  perl -pi -e "s/(\"version\": \")[^\"]*(\")/\${1}${ver}\${2}/; s|(\"\@likec4/lsp\": \")[^\"]*(\")|\${1}${ver}\${2}|" "${dir}/package.json"
  ( cd "$dir"
    HOME="$(mktemp -d)" nix shell nixpkgs#nodejs_22 --command \
      npm install --package-lock-only --ignore-scripts >/dev/null )
  local hash
  hash="$(nix run nixpkgs#prefetch-npm-deps -- "${dir}/package-lock.json")"
  perl -pi -e "s/(version = \")[^\"]*(\";)/\${1}${ver}\${2}/ if /version = \"1\./ || /version = \"[0-9]/;
               s|(npmDepsHash = \")[^\"]*(\")|\${1}${hash}\${2}|;
               s|(\@likec4/lsp\@)[0-9][^\`]*|\${1}${ver}|" "$file"
  [ -n "$build_flag" ] && { log "build: likec4-lsp"; nix build --no-link ".#likec4-lsp"; }
}

# --- ellsp: buildNpmPackage from a GitHub tag -----------------------------

update_ellsp() {
  local file="nix/ellsp.nix" tag src srchash npmhash
  tag="$(gh api repos/elisp-lsp/Ellsp/tags --jq '.[].name' | sort -V | tail -1)"
  log "ellsp -> ${tag}"
  srchash="$(nix-prefetch-url --unpack "https://github.com/elisp-lsp/Ellsp/archive/${tag}.tar.gz" \
    | xargs nix hash to-sri --type sha256)"
  src="$(nix-build --no-out-link -E \
    "(import <nixpkgs> {}).fetchFromGitHub { owner=\"elisp-lsp\"; repo=\"Ellsp\"; rev=\"${tag}\"; hash=\"${srchash}\"; }")"
  npmhash="$(nix run nixpkgs#prefetch-npm-deps -- "${src}/proxy/package-lock.json")"
  # version appears once at the top-level `let`; the src hash is the one in
  # the Ellsp fetchFromGitHub block (msgu keeps its own).
  perl -pi -e "s/(version = \")[^\"]*(\";)/\${1}${tag}\${2}/ if \$. < 12;" "$file"
  perl -0pi -e "s/(repo = \"Ellsp\";\s*\n\s*rev = version;\s*\n\s*hash = \")[^\"]*(\")/\${1}${srchash}\${2}/" "$file"
  perl -0pi -e "s/(npmDepsHash = \")[^\"]*(\")/\${1}${npmhash}\${2}/" "$file"
  [ -n "$build_flag" ] && build_ellsp
}

build_ellsp() {
  log "build: ellsp"
  # shellcheck disable=SC2016 # ${...} is Nix interpolation, not shell
  nix build --no-link --impure --expr '
    let f = builtins.getFlake (toString ./.);
        pkgs = import f.inputs.nixpkgs { system = builtins.currentSystem; };
    in pkgs.callPackage ./nix/ellsp.nix { emacs = f.packages.${builtins.currentSystem}.emacs; }'
}

# --- design: multi-consumer pin tracking a rewritten-history branch -------

update_design() {
  local file="nix/design-pin.nix" branch rev date prefix ver sha
  branch="$(gh api repos/Jylhis/design --jq .default_branch)"
  rev="$(gh api "repos/Jylhis/design/commits/${branch}" --jq .sha)"
  date="$(gh api "repos/Jylhis/design/commits/${branch}" --jq '.commit.committer.date' | cut -dT -f1)"
  sha="$(nix-prefetch-url --unpack "https://github.com/Jylhis/design/archive/${rev}.tar.gz")"
  # Keep the semantic-version prefix from the current pin (it comes from the
  # upstream CHANGELOG, which this script cannot read); refresh only the date.
  prefix="$(sed -n 's/.*version = "\([0-9.]*\)-unstable.*/\1/p' "$file")"
  ver="${prefix:-0}-unstable-${date}"
  log "design -> ${rev} (${ver})"
  perl -pi -e "s/(rev = \")[^\"]*(\")/\${1}${rev}\${2}/;
               s/(sha256 = \")[^\"]*(\")/\${1}${sha}\${2}/;
               s/(version = \")[^\"]*(\")/\${1}${ver}\${2}/" "$file"
  log "re-vendoring website/public/ds (just ds-sync)"
  just ds-sync
}

# --- dispatch -------------------------------------------------------------

run_one() {
  local name="$1" how="${PINS[$1]:-}"
  [ -n "$how" ] || { echo "unknown pin: $name (try --list)" >&2; exit 1; }
  case "$how" in
    tag | branch) update_via_nix_update "$name" "$how" ;;
    fn:*) "${how#fn:}" ;;
  esac
}

case "${1:-}" in
  --list) printf '%s\n' "${!PINS[@]}" | sort; exit 0 ;;
esac

targets=("$@")
[ "${#targets[@]}" -eq 0 ] && targets=("${!PINS[@]}")
for t in "${targets[@]}"; do run_one "$t"; done

log "done. Review the diff, then commit (design also touched website/public/ds)."
