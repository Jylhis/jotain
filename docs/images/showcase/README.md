# Showcase captures

Seven scenes (`code`, `completion`, `popup`, `files`, `vc`, `org`, `overview`)
under both Jylhis themes — `jylhis-sheet` (light) and `jylhis-field` (dark).
Produced by `etc/showcase.el`; regenerate with `just showcase`.

## How these were captured — read before trusting them

These were **not** captured from the Jotain distribution. The session that made
them could not build it: the egress proxy returns 403 for the `emacs-overlay`
flake input, that overlay is in no binary cache, and every package archive
(MELPA, GNU ELPA, NonGNU ELPA) is blocked as well. `nix build .#default` cannot
run, and neither can a source checkout that bootstraps `elpa/`.

What they *were* captured from:

- **Emacs 30.2** — nixpkgs' own `pkgs.emacs` at the `flake.lock` revision,
  built straight from the pinned nixpkgs source in the store. No overlay
  needed, and cache-parity means this is the same store path the `mainline`
  variant resolves to.
- **The real Jylhis themes**, `trivialBuild`-ed from `github.com/Jylhis/design`
  at exactly the `nix/design-pin.nix` revision (`452789c`), native-compiled
  against that Emacs. The colours are genuine.
- **The real `lisp/init-*.el` modules**, loaded unmodified.
- **26 third-party packages from the Ubuntu archive** — the only reachable
  source. These are older than what the distribution ships: vertico 1.5,
  corfu 0.36, consult 0.35, compat 29.1, magit 3.3.0.

### What is missing, and what that means

`compat` could not be fetched from GNU ELPA and is in no binary cache, and it
is a dependency of essentially every modern Emacs package — so the nixpkgs
`emacsPackages` route failed wholesale. The Ubuntu archive filled part of the
gap but not these:

| Absent | Effect on the images |
|---|---|
| `doom-modeline` | Modeline is the **vanilla** one, not Jotain's |
| `nerd-icons` + the `nerd-icons-*` family | **No icons anywhere** — dired, completion, corfu margins |
| `dirvish` | `files` scene falls back to plain dired |
| `magit` | `vc` scene falls back to built-in `vc-dir` + a diff buffer |
| `org-modern`, `org-appear` | `org` scene is plain Org with theme faces only |
| `indent-bars` | No indent guides |
| `breadcrumb` | No header-line breadcrumb |
| `pulsar`, `mixed-pitch`, `diredfl`, `hl-todo` | Absent |

Also absent: the ~275 tree-sitter grammars (each a separate blocked GitHub
fetch), so every buffer uses a non-treesit major mode. The scenes were chosen
around that — Elisp, Org, dired and diff need no grammar.

### What the images *do* legitimately show

The theme itself — background, foreground, syntax colours, the bronze accent —
plus `tab-bar` with two tabs, `display-line-numbers`, `hl-line`, `show-paren`,
`rainbow-delimiters`, `diff-hl`, `flymake`, `eldoc`, the `devenv` modeline
segment, and the vertico + marginalia + orderless minibuffer stack. The
`completion` scene is the strongest of the set.

### Known defect

The `popup` scene does not show the corfu popup. Two causes were found; the
first is fixed, the second is not:

1. `jotain-screenshot` uses `x-export-frames`, which exports a single frame's
   own contents — a corfu popup is a **child frame** and never appears in it.
   Fixed by routing child-frame scenes through `import -window root`.
2. corfu displays from `post-command-hook`, which does not run when
   `completion-at-point` is invoked from a timer rather than the command loop.
   Not fixed. Driving it via `execute-kbd-macro` is the likely fix.

### Consequence for the UI review

Findings 1, 3 and 4 of `docs/reviews/2026-07-ui-review.md` are visual-verdict
calls, and these captures **do not** settle any of them: finding 1 is macOS
titlebar behaviour, and findings 3 and 4 need `doom-modeline` and the full
fringe stack, none of which are present here. Regenerate with `just showcase`
on a real distribution build to settle them.
