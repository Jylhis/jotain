## 2024-05-19 - Uniquify missing
**Learning:** Emacs' default buffer name collision strategy adds `<2>`, `<3>`, which is hard to distinguish for users with many files of the same name.
**Action:** Always enable `uniquify` and set it to `forward` to prefix buffer names with their directory path (e.g., `src/index.ts` instead of `index.ts<2>`), improving buffer switching UX significantly.
