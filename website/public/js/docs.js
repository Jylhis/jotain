/*
 * docs.js — minimal chrome behavior for generated pages:
 * just the theme toggle. Buffer switching lives on the landing SPA.
 * Jylhis 3.0.0 selects mode with data-mode="dark" on <html>.
 */
(function () {
  'use strict';
  var btn = document.getElementById('theme-btn');
  if (!btn) return;
  function render() {
    btn.textContent = document.documentElement.dataset.mode === 'dark' ? '☀' : '☾';
  }
  btn.addEventListener('click', function () {
    var dark = document.documentElement.dataset.mode !== 'dark';
    document.documentElement.dataset.mode = dark ? 'dark' : '';
    try { localStorage.setItem('jotain-theme', dark ? 'dark' : 'light'); } catch (e) { /* private mode */ }
    render();
  });
  render();
})();
