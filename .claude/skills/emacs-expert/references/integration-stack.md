# Integration Stack Reference

Extended patterns for the Vertico/Corfu/Consult/Orderless ecosystem used in jotain.

## Completion Architecture

All completion flows through this stack:
- **Vertico** — vertical minibuffer completion UI
- **Consult** — enhanced search and navigation commands
- **Corfu** — in-buffer completion popup (replaces company)
- **Orderless** — flexible space-separated completion style
- **Marginalia** — rich annotations in minibuffer

### Vertico Patterns

```elisp
;; Vertico is configured in completion.el
;; Key settings:
(setq vertico-cycle t)          ; Wrap around at ends
(setq vertico-resize t)         ; Dynamic height

;; Directory navigation in find-file stays in minibuffer
;; Use DEL to go up, RET to enter
```

### Consult Commands

Prefer Consult over built-ins for these operations:

| Built-in | Consult equivalent | Use in jotain |
|----------|-------------------|---------------|
| `switch-to-buffer` | `consult-buffer` | Yes |
| `find-file` | `consult-find` | For project files |
| `grep` | `consult-grep` / `consult-ripgrep` | Yes |
| `goto-line` | `consult-goto-line` | Yes |
| `imenu` | `consult-imenu` | Yes |
| `yank-pop` | `consult-yank-pop` | Yes |

### Corfu Integration

Corfu provides in-buffer popup completion. Key integration points:

```elisp
;; Corfu works with Eglot automatically via capf
;; Add additional completion sources via cape:
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)

;; Corfu in terminal (tui) mode:
(use-package corfu-terminal
  :ensure
  :unless (display-graphic-p)
  :config (corfu-terminal-mode 1))
```

### Orderless Configuration

Orderless allows space-separated tokens that can match in any order:

```elisp
;; Components can be:
;; - literal:  "foo bar" matches both "foo" and "bar" anywhere
;; - regexp:   "fo+" matches regexp pattern
;; - initialism: "fb" matches "foo-bar"
;; - flex:     (with cape-flex) fuzzy matching

(setq completion-styles '(orderless basic))
(setq completion-category-overrides
      '((file (styles basic partial-completion))))
```

## Eglot LSP Configuration

Eglot (built-in) is the LSP client. Configured in `programming.el`.

```elisp
;; Add server for a new language
(add-to-list 'eglot-server-programs
             '(new-mode . ("language-server" "--stdio")))

;; Per-project server configuration via .dir-locals.el:
((nil . ((eglot-server-programs
          . ((python-mode . ("pyright-langserver" "--stdio")))))))

;; Disable specific LSP capabilities
(setq-local eglot-ignored-server-capabilities
            '(:documentHighlightProvider))
```

## Theme System Details

From `ui.el`:

```elisp
;; Internal mappings (do not change without testing):
;; jotain-theme-light → doom-nord-light
;; jotain-theme-dark  → nord

;; The disable-all-themes advice prevents theme blending:
(advice-add 'load-theme :before #'jotain-ui--disable-all-themes)

;; Daemon mode: themes applied after frame creation
;; Hook: server-after-make-frame-hook
;; Toggle: C-c t → jotain-ui-toggle-theme
```

## Org-mode Configuration

From `writing.el`:

```elisp
;; Key org settings to be aware of:
(setq org-directory "~/org")
(setq org-hide-emphasis-markers t)

;; Capture templates follow this pattern:
(setq org-capture-templates
      '(("t" "Task" entry (file+headline "~/org/tasks.org" "Tasks")
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)))
```
