/*
 * docs.js: theme toggle for generated pages. Buffer switching
 * lives on the landing SPA (app.js).
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
