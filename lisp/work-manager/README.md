# work-manager

A comprehensive Emacs package for managing git worktrees with JIRA integration.

## Features

- **Create worktrees** with automatic branch naming
- **JIRA integration** for issue-based workflow
- **Worktree registry** for tracking and management
- **Interactive commands** with completion
- **CLI-ready** design for shell integration

## Installation

This package is included in the jotain Emacs configuration and installed via Nix.

### With Nix

```nix
{
  environment.systemPackages = [
    (pkgs.callPackage ./lisp/work-manager {
      inherit (pkgs.emacsPackages) trivialBuild magit org-jira;
    })
  ];
}
```

### Manual Installation

1. Clone or copy the `work-manager` directory to your Emacs load path
2. Install dependencies: magit, org-jira
3. Add to your configuration:

```elisp
(use-package work-manager
  :commands (work-manager-new
             work-manager-new-from-jira
             work-manager-load
             work-manager-remove
             work-manager-list))
```

## Usage

### Interactive Commands

- `M-x work-manager-new` - Create new worktree manually
- `M-x work-manager-new-from-jira` - Create worktree from JIRA issue
- `M-x work-manager-load` - Switch to existing worktree
- `M-x work-manager-remove` - Delete worktree
- `M-x work-manager-list` - Display all managed worktrees
- `M-x work-manager-sync-registry` - Clean up stale entries

### Branch Naming Scheme

Branches follow the pattern: `<type>/<jira-key>-<description>`

Examples:
- `feature/PROJ-123-add-user-login`
- `bugfix/BUG-456-fix-crash-on-startup`
- `hotfix/SEC-789-security-patch`

### Configuration

```elisp
;; Base directory for worktrees (default: ~/worktrees)
(setq work-manager-base-directory "~/dev/worktrees")

;; Branch types (default: feature, bugfix, hotfix, refactor, docs, test, chore)
(setq work-manager-branch-types '("feature" "bugfix" "hotfix"))

;; Default branch type (default: feature)
(setq work-manager-default-branch-type "feature")

;; Enable JIRA integration (default: t)
(setq work-manager-jira-enabled t)

;; Auto-fetch JIRA issues (default: t)
(setq work-manager-auto-fetch-jira t)
```

### JIRA Integration

The package integrates with `org-jira` to provide seamless issue-based workflow:

1. Configure org-jira with your JIRA credentials
2. Run `M-x work-manager-new-from-jira`
3. Select an issue from your JIRA backlog
4. Choose a branch type
5. Worktree is created with automatic branch naming

### CLI Usage

The package provides CLI wrapper functions for shell integration:

```bash
# Create worktree
emacs --batch -l work-manager.el \
  --eval '(work-manager-cli-new "feature/add-login" "PROJ-123")'

# List worktrees
emacs --batch -l work-manager.el \
  --eval '(work-manager-cli-list)'

# Remove worktree
emacs --batch -l work-manager.el \
  --eval '(work-manager-cli-remove "/path/to/worktree" t)'
```

## Registry

Worktrees are tracked in a JSON registry file (default: `~/.emacs.d/worktree-registry.json`).

The registry stores:
- Worktree path
- Branch name
- JIRA issue key and summary
- Creation timestamp
- Git repository root

## Dependencies

- Emacs 29.1+
- magit 3.0.0+
- org-jira 4.0.0+

## License

GPL-3.0-or-later

## Author

Jylhis
