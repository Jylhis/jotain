/*
 * docs.js — minimal chrome behavior for generated pages:
 * just the theme toggle. Buffer switching lives on the landing SPA.
 */
(function () {
  'use strict';
  var btn = document.getElementById('theme-btn');
  if (!btn) return;
  function render() {
    btn.textContent = document.documentElement.dataset.theme === 'dark' ? '☀' : '☾';
  }
  btn.addEventListener('click', function () {
    var dark = document.documentElement.dataset.theme !== 'dark';
    document.documentElement.dataset.theme = dark ? 'dark' : '';
    try { localStorage.setItem('jotain-theme', dark ? 'dark' : 'light'); } catch (e) { /* private mode */ }
    render();
  });
  render();
})();
