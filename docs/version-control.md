# Version control

Magit and built-in

## General workflow

- Inspect current state
- Git log (follow renames and moves)
- git blame (follow renames and moves)
- STage specific files
- Stage specific lines (hunks)
- Commit
- list/jump worktrees?

## Day to day functionality
magit-status (`C-x g s`) = vc-dir (`C-x v d`)

vc-diff `=` or `C-x v =`

To commit files mark with `m` and `v`


commit single file with `C-x v v`

log for current file `C-x v l` `vc-print-log`
log for whole repo `C-x v L` `vc-print-root-log`
log for region `C-x v h` `vc-region-history`

## What is missing

- Commit specific lines


## summary

C-x v d          ; open status (like magit-status)
  m              ; mark files to commit
  =              ; inspect diff
  v              ; start commit
  C-c C-c        ; finalize

C-x v L          ; browse repo history

## Sources

- https://www.gnu.org/software/emacs/manual/html_node/emacs/Version-Control.html
- https://docs.magit.vc/magit/
