/*
 * theme-init.js — set the Print/Negative mode before first paint.
 * Loaded synchronously in <head> by every page so there is no flash
 * of the wrong theme. Dark mode is data-mode="dark" on <html>.
 */
try {
  var t = localStorage.getItem('jotain-theme');
  document.documentElement.dataset.mode =
    (t ? t === 'dark' : matchMedia('(prefers-color-scheme: dark)').matches) ? 'dark' : '';
} catch (e) { /* no storage — fall back to light */ }
