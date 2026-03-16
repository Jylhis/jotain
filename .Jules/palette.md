## 2024-05-19 - Auto-scroll Compilation Output to First Error
**Learning:** Users running long builds or tests in Emacs often lose track of the process state because the compilation buffer doesn't scroll by default. This forces them to manually jump to the compilation window to check progress or find errors, disrupting their workflow.
**Action:** Set `compilation-scroll-output` to `'first-error`. This automatically scrolls the compilation buffer to follow output but stops at the first error, giving users immediate feedback on build progress without missing the point of failure.
