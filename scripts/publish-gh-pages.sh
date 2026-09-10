#!/usr/bin/env bash
# Publish a built site tree to the `gh-pages` branch.
#
# The site is served from a GitHub Pages *branch* source: the branch root
# is the production site (page.jylhis.com/jotain/), and each open pull
# request gets a preview under pr-preview/pr-<N>/
# (page.jylhis.com/jotain/pr-preview/pr-N/). A production publish preserves
# every pr-preview/ directory, and a preview publish touches only its own
# subdirectory, so the two never clobber each other.
#
# Usage:
#   publish-gh-pages.sh production      <src-dir>       "<commit-msg>"
#   publish-gh-pages.sh preview   <pr>  <src-dir>       "<commit-msg>"
#   publish-gh-pages.sh remove-preview  <pr>            "<commit-msg>"
#
# <src-dir> is the built site tree (the contents of result-site/public).
# Assumes it runs inside a git checkout whose `origin` remote is pushable
# (in GitHub Actions, actions/checkout wires GITHUB_TOKEN auth for origin).
# Concurrent publishers race on the branch tip; a fetch/reset/re-apply loop
# makes each publish converge without losing the other's paths.
set -euo pipefail

BRANCH="gh-pages"
WT=".gh-pages-worktree"

mode="${1:?mode required: production|preview|remove-preview}"

case "$mode" in
  production)
    SRC="$(cd "${2:?src-dir required}" && pwd)"
    MSG="${3:-deploy site (production)}"
    ;;
  preview)
    PR="${2:?pr number required}"
    SRC="$(cd "${3:?src-dir required}" && pwd)"
    MSG="${4:-deploy site (pr-preview/pr-$PR)}"
    ;;
  remove-preview)
    PR="${2:?pr number required}"
    MSG="${3:-remove pr-preview/pr-$PR}"
    ;;
  *)
    echo "publish-gh-pages: unknown mode '$mode'" >&2
    exit 2
    ;;
esac

git config user.name  "${GIT_AUTHOR_NAME:-github-actions[bot]}"
git config user.email "${GIT_AUTHOR_EMAIL:-github-actions[bot]@users.noreply.github.com}"

# A fresh worktree tracking the current branch tip, created once. The
# retry loop re-syncs it to origin each attempt, so the actual file
# changes are re-applied from $SRC (outside the worktree) every time.
setup_worktree() {
  git worktree remove --force "$WT" 2>/dev/null || true
  rm -rf "$WT"
  git fetch origin "$BRANCH" || true
  if git show-ref --verify --quiet "refs/remotes/origin/$BRANCH"; then
    git worktree add "$WT" "origin/$BRANCH" >/dev/null
    git -C "$WT" checkout -B "$BRANCH" "origin/$BRANCH" >/dev/null
  else
    git worktree add --detach "$WT" >/dev/null
    git -C "$WT" checkout --orphan "$BRANCH" >/dev/null
    git -C "$WT" reset --hard >/dev/null 2>&1 || true
    find "$WT" -mindepth 1 -maxdepth 1 ! -name .git -exec rm -rf {} +
  fi
}

# Re-apply the intended tree state into the worktree from scratch, so the
# step is idempotent across retries.
apply_changes() {
  case "$mode" in
    production)
      # Replace the root with the new site, preserving PR previews.
      find "$WT" -mindepth 1 -maxdepth 1 \
        ! -name .git ! -name pr-preview -exec rm -rf {} +
      cp -rL "$SRC/." "$WT/"
      ;;
    preview)
      rm -rf "$WT/pr-preview/pr-$PR"
      mkdir -p "$WT/pr-preview/pr-$PR"
      cp -rL "$SRC/." "$WT/pr-preview/pr-$PR/"
      ;;
    remove-preview)
      rm -rf "$WT/pr-preview/pr-$PR"
      ;;
  esac
}

setup_worktree

for attempt in 1 2 3 4 5; do
  # Sync to the latest tip before applying, so a concurrent publish's
  # commit is kept rather than overwritten.
  git -C "$WT" fetch origin "$BRANCH" || true
  if git show-ref --verify --quiet "refs/remotes/origin/$BRANCH"; then
    git -C "$WT" reset --hard "origin/$BRANCH" >/dev/null
  fi

  apply_changes

  git -C "$WT" add -A
  if git -C "$WT" diff --cached --quiet; then
    echo "publish-gh-pages: no changes to publish"
    exit 0
  fi
  git -C "$WT" commit -m "$MSG" >/dev/null

  if git -C "$WT" push origin "HEAD:$BRANCH"; then
    echo "publish-gh-pages: published ($mode) on attempt $attempt"
    exit 0
  fi

  echo "publish-gh-pages: push rejected (attempt $attempt), re-syncing…" >&2
  sleep $((attempt * 2))
done

echo "publish-gh-pages: failed to push after retries" >&2
exit 1
