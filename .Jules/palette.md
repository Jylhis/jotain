## 2024-03-27 - Automatic Compilation Scrolling
**Learning:** Emacs compilation output requires manual scrolling by default, which interrupts workflow and provides a poor developer UX when identifying errors.
**Action:** Configure the built-in `compile` package with `(compilation-scroll-output 'first-error)` to automatically scroll the compilation buffer until the first error is reached, keeping the focus on actionable feedback without manual intervention.
