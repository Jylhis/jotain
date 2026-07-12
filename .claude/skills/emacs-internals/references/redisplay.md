# The redisplay engine

Sources: GNU Elisp Reference Manual (Emacs 30.2) Display chapter and the
design commentary at the top of `src/xdisp.c`.

## Architecture

- Redisplay is decoupled from mutation: editing code never draws; the
  command loop (and `sit-for`/`redisplay`) trigger it.
  `redisplay_internal` in `xdisp.c` is the single entry point.
- Three steps: (1) decide which frames need consideration (often just the
  selected window); (2) per window, `redisplay_window` computes a **desired
  glyph matrix** describing how the window should look, reusing the
  **current glyph matrix** (what's on the glass) where possible; (3)
  `update_window`/`update_frame` diff desired vs. current and draw only the
  difference, then desired rows become current.
- **Glyph matrices** are 2-D arrays of `struct glyph`; one glyph row per
  screen line (left margin / text area / right margin arrays). Desired
  matrices are sparse — only rows needing update are enabled.
  `display_line` produces one row from the iterator.
- On text terminals, windows sub-allocate from a **frame matrix** (rows are
  slices of frame rows), enabling frame-global scroll optimizations.

## The iterator (`struct it`)

- All complexity — overlays, text properties, display tables, invisibility,
  compositions, bidi — hides behind the iterator: `init_iterator`, then
  `get_next_display_element` / `set_iterator_to_next`.
- Infrequently-changing state (face, invisibility, display specs) is only
  re-examined at **stop positions** (`compute_stop_pos`: next text-property
  change, overlay change, or composition boundary). At a stop,
  `handle_stop` runs handlers in order: `fontified` → `face` → `display` →
  `invisible` → `composition`, plus overlay changes.
- Display *simulation* without drawing uses the same machinery:
  `move_it_to`, `move_it_by_lines` back `vertical-motion`,
  `posn-at-point`, window-start recentering. Anything that simulates
  layout can therefore trigger fontification and pay full layout cost —
  this is why `line-move-visual`/`vertical-motion` on long lines is
  expensive.
- Bidi reordering (`bidi.c`) happens inside `set_iterator_to_next`; R2L
  rows get glyphs prepended and are drawn left-to-right by back-ends.
  Disabling bidi in pure-ASCII buffers (as this repo's `early-init.el`
  does via `bidi-paragraph-direction 'left-to-right` and
  `bidi-inhibit-bpa`) skips reordering and the bracket-pairing algorithm.

## Optimization ladder (tried in order per window)

1. `try_cursor_movement` — nothing changed, point moved within the
   displayed text: just move the cursor.
2. `try_window_reusing_current_matrix` — text unchanged, window start
   changed (scrolling): shift/reuse current-matrix rows.
3. `try_window_id` — bounded insert/delete: regenerate only the changed
   region, reuse the rest.
4. `try_window` — regenerate the whole window matrix (still no font
   re-dimensioning).
5. Even a full regeneration may draw nothing: `update_frame` diffs desired
   vs. current before touching the glass.

### What defeats the cheap paths

- Buffer changes spanning the window; font changes (force matrix
  re-dimensioning); `prevent_redisplay_optimizations_p` on the buffer;
  non-zero `windows_or_buffers_changed`; cursor inside scroll margins.
- Conditional `(when …)` display specs that depend on point: redisplay
  re-evaluates them only where it believes text changed — a documented
  anti-pattern.
- Change tracking: `beg_unchanged`/`end_unchanged` per buffer bound the
  region redisplay must reexamine; `force-window-update` and
  `set-buffer-redisplay` widen it deliberately.

## Fontification / jit-lock interplay

- `fontification-functions` are called *by redisplay itself* (the
  `fontified` property handler, first in the handler table) just before
  text is displayed; with font-lock the list is `(jit-lock-function)`.
- Contract: given POS, fontify from POS (recommended chunk ~400–600 chars),
  set `fontified` non-nil over what you covered. Text never displayed is
  never fontified — fontification is lazy and display-driven.
- Under long-line optimizations (Emacs 29+), fontification functions run
  narrowed around POS (`long-line-optimizations-in-fontification-functions`).

## Forcing and observing redisplay

- `(redisplay)` attempts immediate redisplay; `(redisplay t)` prevents
  preemption by pending input; return t only means "attempted".
  `(sit-for 0)` ≡ `(redisplay)`.
- `force-window-update OBJECT` marks window(s) for the *next* redisplay.
  `redraw-frame` / `redraw-display` clear and redraw.
- Hooks: `pre-redisplay-function` / `pre-redisplay-functions` (per-window,
  buffer current). `post-command-hook` runs *before* the command loop's
  redisplay, not after.
- Redisplay is preempted by pending input — a busy loop that never reads
  input starves display updates; use `(redisplay t)` or `sit-for` inside.
- Mode lines: `force-mode-line-update`, and `:eval` forms in
  `mode-line-format` run during redisplay — keep them cheap and
  side-effect-free; they can run asynchronously relative to command
  execution (mouse-highlight/expose paths).

## Echo area essentials

- `message` logs to `*Messages*` (respecting `message-log-max`) and
  displays in the echo area; `inhibit-message` suppresses display but
  keeps logging. Always `(message "%s" str)` for literal strings.
  `set-message-functions` pipeline (`set-minibuffer-message`,
  `inhibit-message`, `set-multi-message`) routes messages.

## Overlays vs text properties for display (cost model)

- Overlay `priority`: higher wins per attribute; face attributes merge;
  all overlays outrank text properties. Overlay `before-string`/
  `after-string`/`display` are interpreted only by redisplay.
- A *replacing* `display` spec makes redisplay skip covered text entirely
  (its `invisible` and other properties are then ignored).
- Invisible text: `buffer-invisibility-spec` + `invisible` property;
  changing the spec is far cheaper than rewriting properties;
  `(ATOM . t)` spec entries show an ellipsis.

## Debugging redisplay

- `M-x trace-redisplay` (in builds with `--enable-checking=glyphs`),
  `dump-glyph-matrix`. Practically: bisect with `redisplay-skip-fontification-on-input`, `jit-lock-defer-time`,
  and check `(elp-instrument-package "font-lock")` or the `profiler` for
  fontification hotspots; suspect mode-line `:eval`s and per-window hooks
  first for "constant CPU while idle".
