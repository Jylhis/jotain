/*
 * theme-init.js — set the Paper/Roast theme before first paint.
 * Loaded synchronously in <head> by every page (hand-written and
 * generated alike) so there is no flash of the wrong theme.
 */
try {
  var t = localStorage.getItem('jotain-theme');
  document.documentElement.dataset.theme =
    (t ? t === 'dark' : matchMedia('(prefers-color-scheme: dark)').matches) ? 'dark' : '';
} catch (e) { /* no storage — fall back to light */ }
