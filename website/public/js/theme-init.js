/*
 * theme-init.js — set the Print/Negative mode before first paint.
 * Loaded synchronously in <head> by every page (hand-written and
 * generated alike) so there is no flash of the wrong theme.
 * Jylhis 3.0.0 selects mode with data-mode="dark" on <html>;
 * data-theme is retired.
 */
try {
  var t = localStorage.getItem('jotain-theme');
  document.documentElement.dataset.mode =
    (t ? t === 'dark' : matchMedia('(prefers-color-scheme: dark)').matches) ? 'dark' : '';
} catch (e) { /* no storage — fall back to light */ }
