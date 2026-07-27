# Buffer internals: gap buffer, markers, overlays, text properties

Source: GNU Elisp Reference Manual (Emacs 30.2), E.9.1 Buffer Internals plus
the Buffer Gap / Text Properties / Overlays chapters.

## Two C structures (`src/buffer.h`)

- `struct buffer_text` holds the text itself; `struct buffer` holds
  everything else and points at a `buffer_text`. **Indirect buffers** are
  two or more `struct buffer`s sharing one `buffer_text` (each keeps its own
  point/narrowing via `pt_marker`/`begv_marker`/`zv_marker`).

## The gap buffer

- Buffer text is one linear C `char` array with a movable **gap** in the
  middle (`beg` = base address; `gpt`/`gpt_byte` = gap position; `z`/`z_byte`
  = end; `gap_size`).
- Insertion/deletion happens at the gap; editing at a new position first
  *moves the gap* there (a `memmove` proportional to the distance).
  Consequences:
  - Sequential edits at one location are cheap; alternating edits at distant
    locations are O(buffer size) each.
  - Char position vs byte position are distinct in multibyte buffers;
    primitives track both (`position-bytes`, `byte-to-position`).
- Modification ticks: `modiff` (every text modification — exposed as
  `buffer-modified-tick`), `chars_modiff` (character changes only,
  `buffer-chars-modified-tick`), `overlay_modiff`, `save_modiff` (value at
  last save). Redisplay uses `beg_unchanged`/`end_unchanged` to bound the
  region it must reexamine.

## Markers

- Each buffer's markers form a **linked chain** (the `markers` field is one
  marker whose chain links the rest). Every insertion/deletion walks the
  chain to adjust marker positions — thousands of markers in one buffer make
  editing linearly slower. Delete markers you create in loops
  (`set-marker` to nil) or use `save-excursion` sparingly in hot paths.
- Markers store both char and byte positions and have an insertion type
  (`marker-insertion-type`) controlling which side of an insertion at the
  marker position they end up on.
- Point (`pt`), the mark, and window starts are markers or marker-like
  fields; narrowing bounds are `begv`/`zv`.

## Text properties

- Stored in an **interval tree** per `buffer_text` (`intervals` field,
  `struct interval`; also used for string properties). Lookup and
  next-change scanning are O(log n).
- Text properties are *part of the text*: copied with it (`buffer-substring`
  vs `buffer-substring-no-properties`), and property changes count as buffer
  modifications (but not as character changes — see `chars_modiff`).

## Overlays

- Since the **Emacs 29 "noverlay" rewrite** (`src/itree.c`), each buffer's
  overlays live in an **interval tree** too (`overlays` field in
  `struct buffer`). Lookup is O(log n + k); the historical pathology where
  thousands of overlays made redisplay and editing crawl (the old
  `overlay_center` two-list scheme) is gone, though overlays are still
  heavier than text properties.
- Overlays are buffer-local, not part of the text (never copied with it),
  and support `priority`, `window`, and before/after-string properties that
  only redisplay interprets.
- Rule of thumb: text properties for *content-attached* data (fontification,
  syntax), overlays for *annotation/UI* (highlights, inline hints) — and
  prefer text properties in very hot paths.

## Buffer-local variables

- Built-in per-buffer variables (`DEFVAR_PER_BUFFER`) live as slots in
  `struct buffer` with a `local_flags` bitmap marking which are localized
  (e.g. `mode_line_format`, `tab_width`, `truncate_lines`,
  `enable-multibyte-characters`…). All other buffer-local bindings live in
  the `local_var_alist`.
- `inhibit_buffer_hooks`: buffers created for internal/temp use (e.g.
  `with-temp-buffer`, generate-new-buffer with INHIBIT-BUFFER-HOOKS) skip
  `kill-buffer-hook`/`buffer-list-update-hook` — that's why temp buffers are
  cheap and why global buffer hooks don't see them.
- The `next` chain links **all** buffers including killed ones (GC uses it);
  a "killed" buffer object survives until unreferenced.

## Practical performance notes

- Long lines historically defeated redisplay/scanning caches; Emacs 29+
  mitigates with `long-line-threshold` and forced narrowing
  (`long-line-optimizations-p`).
- `cache_long_line_scans` / `cache-long-scans` enables newline caching for
  buffers where scanning is hot.
- Counting positions: prefer `line-number-at-pos` sparingly in hot loops —
  it scans; cache or use `count-lines` over bounded regions.
