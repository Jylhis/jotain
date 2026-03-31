# Palette's Journal

## UX and Accessibility Learnings

## 2026-03-31 - [Automatically scroll compilation output to first error]
**Learning:** [Users find it tedious to manually scroll through compilation logs to find errors when a build fails. By default, Emacs does not scroll the compilation buffer, leaving the user at the top of the output.]
**Action:** [Configure the built-in `compile` package in `elisp/programming.el` to set `compilation-scroll-output` to `'first-error`. This will automatically scroll the build output and stop at the first error, making it much easier to identify and fix issues immediately, greatly improving developer UX.]
