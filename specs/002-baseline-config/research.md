# Research: Jotain Baseline Configuration

## R1: Nix Detection Mechanism

**Decision**: Check `NIX_PROFILES` environment variable.
**Rationale**: Present on all NixOS and nix-managed systems. More reliable than checking for `/nix/store` on PATH (which could be set on non-NixOS systems using nix-portable). Already specified in the constitution.
**Alternatives considered**: Checking `/etc/NIXOS` (NixOS-only, misses standalone Nix), checking `nix` on PATH (doesn't imply packages are Nix-managed).

## R2: Non-Nix Package Archives

**Decision**: Configure GNU ELPA + MELPA when `jotain/nix-managed-p` is nil.
**Rationale**: GNU ELPA is official; MELPA has the widest package coverage. NonGNU ELPA is built-in to Emacs 28+ and auto-configured, so no manual addition needed.
**Alternatives considered**: MELPA Stable (fewer packages, stale versions), NonGNU ELPA only (insufficient coverage).

## R3: Platform Detection Strategy

**Decision**: Use `system-type`, `system-configuration`, and Termux-specific env vars (`TERMUX_VERSION`).
**Rationale**: `system-type` gives OS family, `system-configuration` gives arch (e.g., `aarch64-unknown-linux-gnu`). Termux sets `TERMUX_VERSION` which distinguishes Android from standard Linux aarch64.
**Alternatives considered**: Checking `/system/build.prop` (fragile, not always accessible), checking `ANDROID_ROOT` (less reliable than Termux-specific vars).

## R4: Directory Rename Approach

**Decision**: Rename `elisp/` → `lisp/`, create `modules/`, update all references atomically.
**Rationale**: Constitution mandates `lisp/` + `modules/`. The current `elisp/` is empty (only `.gitkeep` and a stale `.elc`), so the rename has zero content migration cost. Doing it atomically avoids broken intermediate states.
**Alternatives considered**: Gradual migration with symlinks (unnecessary complexity given empty directory), keeping `elisp/` as alias (contradicts constitution).

## R5: private.el Loading Strategy

**Decision**: Load via `after-init-hook` using `load` with `noerror` parameter.
**Rationale**: `after-init-hook` runs after all `init.el` processing, ensuring all modules are available. The `noerror` parameter means missing `private.el` is silently ignored. Wrapping in `condition-case` catches and reports errors without preventing startup.
**Alternatives considered**: `emacs-startup-hook` (runs after frame creation, too late for some settings), `eval-after-load` (wrong semantics).
