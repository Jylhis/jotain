# Research: Jotain Baseline

**Branch**: `001-baseline` | **Date**: 2026-02-22

## Overview

This research document validates the existing architecture against
the baseline specification and constitution. Since this is a baseline
(no new code), research focuses on confirming current decisions and
identifying any gaps between the spec and reality.

## Decision: Emacs 30+ PGTK as Base

**Decision**: Use `emacs30-pgtk` from the emacs-overlay.

**Rationale**: PGTK provides native Wayland support and is the
forward-looking GTK backend. Emacs 30 includes built-in use-package,
tree-sitter integration, and improved native compilation — all
features Jotain depends on.

**Alternatives considered**:
- `emacs30-gtk3`: Legacy X11/GTK3 backend. Rejected: no native
  Wayland support.
- `emacs29`: Stable but lacks built-in use-package and some
  tree-sitter improvements. Rejected: requires backports.
- `emacs-git`: Bleeding edge. Rejected: breaks reproducibility
  guarantees; incompatible with constitution principle III.

## Decision: Eglot over lsp-mode

**Decision**: Use Eglot as the sole LSP client.

**Rationale**: Eglot is built into Emacs 29+, has minimal
configuration surface, and aligns with principle VII (Simplicity).
It uses standard Emacs facilities (Flymake, xref, completion-at-point)
rather than reimplementing them.

**Alternatives considered**:
- `lsp-mode`: Feature-rich but heavy. Rejected: violates principle
  VII; introduces large dependency tree; parallel UI systems
  (lsp-ui) conflict with Vertico/Corfu stack.
- No LSP: Rejected: modern development workflows depend on
  language intelligence (FR-008).

## Decision: Vertico/Corfu/Consult Completion Stack

**Decision**: Use Vertico (minibuffer), Corfu (in-buffer), Consult
(search), Orderless (matching), Embark (actions) as the unified
completion ecosystem.

**Rationale**: These packages are designed to work together, use
standard Emacs APIs (`completing-read`, `completion-at-point`), and
are lightweight. They compose rather than replace Emacs infrastructure.

**Alternatives considered**:
- Helm: Monolithic, replaces Emacs completion entirely. Rejected:
  violates principle VII; heavy; non-composable.
- Ivy/Counsel/Swiper: Middle ground but tightly coupled. Rejected:
  Vertico ecosystem is more modular and better maintained.
- Built-in `fido-mode`/`icomplete`: Insufficient for power users.
  Rejected: lacks Consult's search capabilities and Embark's actions.

## Decision: Nix-Only Package Management

**Decision**: All Emacs packages installed via Nix; `package-archives`
set to nil; no runtime installation.

**Rationale**: Core value proposition (constitution principle I).
Ensures reproducibility, eliminates network dependencies during
startup, and prevents version drift between machines.

**Alternatives considered**:
- Hybrid (Nix + MELPA for unstable packages): Rejected: breaks
  reproducibility; creates two package management systems.
- `straight.el`: Rejected: Git-based; not Nix-native; introduces
  build-time network dependency.
- `elpaca`: Rejected: same issues as straight.el.

## Decision: Auto-Dependency Extraction

**Decision**: `nix/lib/dependencies.nix` scans `.el` files with regex
to extract `use-package` declarations and map them to nixpkgs.

**Rationale**: Eliminates manual package list maintenance. Adding a
new package with `:ensure t` in an Elisp module automatically makes
it available in the Nix build (if the name maps correctly).

**Alternatives considered**:
- Manual package list: Rejected: error-prone; requires updating two
  places (Elisp + Nix) for every package change.
- Emacs evaluation at build time: Rejected: requires running Emacs
  during Nix build; breaks sandbox; impure.
- `use-package` keyword `:nix`: Rejected: non-standard; requires
  patching use-package.

## Decision: Platform Detection via platform.el

**Decision**: Centralised platform detection in `platform.el` with
constants and macros.

**Rationale**: Single source of truth for platform checks. Macros
(`platform-when`, `platform-unless`, `platform-cond`) provide
zero-overhead conditionals evaluated at load time.

**Alternatives considered**:
- Inline `system-type` checks: Rejected: scattered, inconsistent,
  violates principle V (Platform Portability).
- Separate init files per platform: Rejected: code duplication;
  harder to maintain shared logic.

## Decision: Two-Theme System with Auto-Detection

**Decision**: Exactly two sanctioned themes (doom-nord-light, nord)
with auto-dark for OS theme detection and `C-c t` toggle.

**Rationale**: Opinionated curation. Users get a polished light/dark
pair without choice paralysis. Theme blending prevention (disabling
all themes before loading new one) ensures visual consistency.

**Alternatives considered**:
- User-selectable themes: Rejected: violates opinionated distribution
  model; theme compatibility testing burden.
- Single theme only: Rejected: light/dark switching is a baseline
  user expectation.
- Modus themes: Considered; Nord aesthetic was preferred for visual
  consistency across the distribution.

## Gaps Identified

1. **Windows support**: `platform-windows-p` exists in `platform.el`
   but is not listed as a supported platform in the spec or tested.
   Status: Correctly out of scope per spec (Linux, macOS, Android).

2. **1Password availability**: `auth-source-1password` configured
   but no test verifies behavior when `op` CLI is absent. The spec
   (FR-019) requires graceful failure. Status: Test coverage gap —
   should be addressed in task planning.

3. **Standalone startup time**: SC-011 requires < 3s but no
   automated benchmark exists. Status: Manual verification only;
   consider adding a timed startup test.
