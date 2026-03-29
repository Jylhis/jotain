## 2024-03-29 - Compilation buffer auto-scrolling
**Learning:** By default, Emacs compilation buffers don't auto-scroll, which means users have to manually switch buffers and scroll to see build progress or errors, disrupting their workflow.
**Action:** Configure `compilation-scroll-output` to `'first-error` to automatically scroll until the first error occurs.
