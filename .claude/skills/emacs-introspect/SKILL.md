---
name: emacs-introspect
description: Live Emacs introspection using evalElisp MCP tool or emacsclient. Query running Emacs state without restarting. Triggers on: "what packages are loaded", "keybinding conflict", "check keybinding", "which function", "inspect variable", "loaded features", "emacs state", "introspect", "query emacs", "live emacs".
argument-hint: "[symbol, keybinding, or question]"
allowed-tools: Bash, mcp__emacs__evalElisp
---

# Emacs Introspection

Query the running Emacs instance for live state information. $ARGUMENTS

Use `mcp__emacs__evalElisp` when available (MCP server running). Fall back to `emacsclient --eval` via Bash when not.

## When to Use Live Introspection vs Static Analysis

| Situation | Use |
|-----------|-----|
| "Is package X loaded right now?" | Live (features variable) |
| "What does keybinding C-c t do?" | Live (key-binding lookup) |
| "Is a function defined?" | Live (fboundp) |
| "What is the current value of variable X?" | Live (symbol-value) |
| "What does this use-package form do?" | Static (read the file) |
| "Where is this package configured?" | Static (grep elisp/) |

## Checking Loaded Packages

```elisp
;; Is a specific feature loaded?
(featurep 'vertico)

;; All loaded features (returns a list)
(cl-remove-if-not
 (lambda (f) (string-prefix-p "jotain" (symbol-name f)))
 features)

;; Did use-package load this package?
(bound-and-true-p vertico-mode)

;; Check use-package load status (requires use-package-compute-statistics t)
(assoc 'package-name use-package-statistics)
```

## Keybinding Inspection

```elisp
;; What command does a key run? (current mode context)
(key-binding (kbd "C-c t"))

;; Where is a command bound?
(where-is-internal 'jotain-ui-toggle-theme)

;; Check global binding specifically
(global-key-binding (kbd "C-c t"))

;; Check for conflicts: all keys bound to a prefix
(let ((map (current-global-map)))
  (cl-loop for key in (accessible-keymaps map)
           when (string-prefix-p "C-c" (key-description (car key)))
           collect (key-description (car key))))
```

## Function Introspection

```elisp
;; Does function exist?
(fboundp 'jotain-ui-toggle-theme)

;; Where is it defined? (returns file + line)
(find-function-noselect 'jotain-ui-toggle-theme)

;; Get docstring
(documentation 'jotain-ui-toggle-theme)

;; Get argument list
(help-function-arglist 'jotain-ui-toggle-theme t)

;; Function type
(functionp 'jotain-ui-toggle-theme)
(macrop 'when-let)
(subrp (symbol-function 'car))
```

## Variable Inspection

```elisp
;; Current value
(symbol-value 'jotain-theme-light)

;; Is it buffer-local?
(local-variable-p 'major-mode)

;; Documented value and docstring
(documentation-property 'jotain-theme-light 'variable-documentation)

;; Defcustom type info
(get 'jotain-theme-light 'custom-type)
```

## Reading Messages and Errors

```elisp
;; Get recent messages (last 20 lines of *Messages*)
(with-current-buffer "*Messages*"
  (let ((end (point-max))
        (start (save-excursion
                 (goto-char (point-max))
                 (forward-line -20)
                 (point))))
    (buffer-substring-no-properties start end)))

;; Check for recent errors in *Warnings*
(when (get-buffer "*Warnings*")
  (with-current-buffer "*Warnings*"
    (buffer-string)))
```

## Mode and Hook Inspection

```elisp
;; Current major mode
major-mode

;; Active minor modes
(cl-remove-if-not
 (lambda (m) (and (boundp m) (symbol-value m)))
 minor-mode-list)

;; What functions are on a hook?
(mapcar #'symbol-name after-init-hook)

;; Is a function on a specific hook?
(memq 'my-function after-init-hook)
```

## Defensive Evaluation Patterns

Wrap introspection calls to avoid breaking the session:

```elisp
;; Safe evaluation that returns nil on error
(ignore-errors
  (symbol-value 'potentially-unbound-var))

;; Conditional existence check before evaluation
(when (fboundp 'optional-function)
  (optional-function))

;; Error-reporting wrapper
(condition-case err
  (some-introspection-call)
  (error (format "Error: %s" (error-message-string err))))
```

## emacsclient Fallback (when MCP unavailable)

```bash
# Evaluate elisp and print result
emacsclient --eval '(featurep (quote vertico))'

# Check keybinding
emacsclient --eval '(key-description (where-is-internal (quote jotain-ui-toggle-theme) nil t))'

# Get variable value
emacsclient --eval '(format "%s" jotain-theme-light)'
```

## See Also

- `/debug-elisp` — for diagnosing errors and performance issues
- `/check-config` — for static validation of configuration files
