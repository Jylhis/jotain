/*
 * app.js — jotain.j10s.io
 * Emacs-frame chrome: buffer switching (tabs / C-x b / n / p / hash),
 * I-search over a section index, modeline position, echo area, theme toggle.
 * No dependencies.
 */
(function () {
  'use strict';

  var ORDER = ['readme', 'man', 'keys'];
  var META = {
    readme: { name: 'README.org', mode: '(Org)' },
    man: { name: '*Man JOTAIN(7)*', mode: '(Man)' },
    keys: { name: 'keybindings.org', mode: '(Org)' }
  };

  var INDEX = [
    { buf: 'readme', label: 'README.org', id: 'sec-intro', title: 'Something, from scratch' },
    { buf: 'readme', label: 'README.org', id: 'sec-ships', title: 'What ships — emacs.nix, init-*.el, module.nix' },
    { buf: 'readme', label: 'README.org', id: 'sec-qs', title: 'Quickstart — devenv shell, just run' },
    { buf: 'readme', label: 'README.org', id: 'sec-manual', title: 'Read the manual' },
    { buf: 'man', label: '*Man JOTAIN(7)*', id: 'man-name', title: 'NAME — jotain' },
    { buf: 'man', label: '*Man JOTAIN(7)*', id: 'man-synopsis', title: 'SYNOPSIS — just run | debug | tty | daemon' },
    { buf: 'man', label: '*Man JOTAIN(7)*', id: 'man-sections', title: 'MANUAL SECTIONS — introduction, installation, …' },
    { buf: 'man', label: '*Man JOTAIN(7)*', id: 'man-seealso', title: 'SEE ALSO — emacs(1), nix(1), just(1)' },
    { buf: 'keys', label: 'keybindings', id: 'keys-cc', title: 'C-c — user namespace (org, magit, consult, gptel)' },
    { buf: 'keys', label: 'keybindings', id: 'keys-cx', title: 'C-x — control namespace (dape, magit-status, vundo)' },
    { buf: 'keys', label: 'keybindings', id: 'keys-global', title: 'Other globals — M-o, embark C-. / C-;, expreg' },
    { buf: 'keys', label: 'keybindings', id: 'keys-embark', title: 'Embark habits — search → edit with wgrep' },
    { buf: 'keys', label: 'keybindings', id: 'keys-disabled', title: 'Disabled — C-z, C-x C-z' }
  ];

  var DEFAULT_ECHO = 'C-s search · C-x b / n / p switch buffer · click tabs · ☾ theme';

  var el = {
    buffer: document.getElementById('buffer'),
    tabs: Array.prototype.slice.call(document.querySelectorAll('.tab')),
    results: document.getElementById('results'),
    isearch: document.getElementById('isearch'),
    input: document.getElementById('search-input'),
    matchCount: document.getElementById('match-count'),
    echo: document.getElementById('echo'),
    modeBuffer: document.getElementById('mode-buffer'),
    modeMajor: document.getElementById('mode-major'),
    modePos: document.getElementById('mode-pos'),
    themeBtn: document.getElementById('theme-btn'),
    searchBtn: document.getElementById('search-btn')
  };

  var state = { buffer: 'readme', searching: false, sel: 0, prefix: null };
  var echoTimer = null;

  /* ── theme ──────────────────────────────────────────── */
  function isDark() { return document.documentElement.dataset.theme === 'dark'; }
  function renderTheme() { el.themeBtn.textContent = isDark() ? '☀' : '☾'; }
  function toggleTheme() {
    var dark = !isDark();
    document.documentElement.dataset.theme = dark ? 'dark' : '';
    try { localStorage.setItem('jotain-theme', dark ? 'dark' : 'light'); } catch (e) { /* private mode */ }
    renderTheme();
    msg('Theme: ' + (dark ? 'jylhis-roast (dark)' : 'jylhis-paper (light)'), 2000);
  }

  /* ── echo area ──────────────────────────────────────── */
  function msg(text, ttl) {
    clearTimeout(echoTimer);
    el.echo.textContent = text;
    el.echo.classList.add('active');
    echoTimer = setTimeout(function () {
      el.echo.textContent = DEFAULT_ECHO;
      el.echo.classList.remove('active');
      state.prefix = null;
    }, ttl || 3000);
  }

  /* ── buffers ────────────────────────────────────────── */
  function renderBuffer() {
    ORDER.forEach(function (b) {
      document.getElementById('buf-' + b).hidden = b !== state.buffer;
    });
    el.tabs.forEach(function (t) {
      if (t.dataset.buffer === state.buffer) t.setAttribute('aria-current', 'page');
      else t.removeAttribute('aria-current');
    });
    el.modeBuffer.textContent = META[state.buffer].name;
    el.modeMajor.textContent = META[state.buffer].mode;
    onScroll();
  }

  function switchBuffer(buf, quiet) {
    state.buffer = buf;
    if (location.hash !== '#' + buf) history.replaceState(null, '', '#' + buf);
    renderBuffer();
    el.buffer.scrollTop = 0;
    if (!quiet) msg('Switched to buffer ' + META[buf].name, 2000);
  }

  function cycleBuffer(dir) {
    var i = (ORDER.indexOf(state.buffer) + (dir || 1) + ORDER.length) % ORDER.length;
    switchBuffer(ORDER[i]);
  }

  function jumpTo(buf, id) {
    state.buffer = buf;
    history.replaceState(null, '', '#' + id);
    closeSearch();
    renderBuffer();
    requestAnimationFrame(function () {
      var target = document.getElementById(id);
      if (target) el.buffer.scrollTop = Math.max(0, target.offsetTop - 24);
      onScroll();
    });
  }

  /* ── modeline position ──────────────────────────────── */
  function onScroll() {
    var c = el.buffer;
    var max = c.scrollHeight - c.clientHeight;
    var line = Math.round(c.scrollTop / 26) + 1;
    var p;
    if (max <= 0 || c.scrollTop <= 2) p = 'Top';
    else if (c.scrollTop >= max - 2) p = 'Bot';
    else p = Math.round((c.scrollTop / max) * 100) + '%';
    el.modePos.textContent = p + ' L' + line;
  }

  /* ── search ─────────────────────────────────────────── */
  function matches() {
    var q = el.input.value.trim().toLowerCase();
    if (!q) return [];
    return INDEX.filter(function (s) {
      return (s.title + ' ' + s.label).toLowerCase().indexOf(q) !== -1;
    });
  }

  function renderSearch() {
    var found = matches();
    var shown = found.slice(0, 8);
    el.results.hidden = !(state.searching && shown.length > 0);
    el.results.textContent = '';
    shown.forEach(function (r, i) {
      var row = document.createElement('div');
      row.className = 'result' + (i === state.sel ? ' selected' : '');
      var title = document.createElement('span');
      title.className = 'result-title';
      title.textContent = r.title;
      var buf = document.createElement('span');
      buf.className = 'result-buf';
      buf.textContent = r.label;
      row.appendChild(title);
      row.appendChild(buf);
      row.addEventListener('click', function () { jumpTo(r.buf, r.id); });
      el.results.appendChild(row);
    });
    el.matchCount.textContent = found.length + (found.length === 1 ? ' match' : ' matches') + ' · RET · ESC';
  }

  function openSearch() {
    state.searching = true;
    state.sel = 0;
    el.input.value = '';
    el.isearch.hidden = false;
    el.echo.hidden = true;
    renderSearch();
    el.input.focus();
  }

  function closeSearch() {
    state.searching = false;
    state.sel = 0;
    el.input.value = '';
    el.isearch.hidden = true;
    el.echo.hidden = false;
    el.results.hidden = true;
  }

  function onQueryKey(e) {
    var found = matches();
    var n = Math.min(found.length, 8);
    if (e.key === 'Enter' && n) {
      e.preventDefault();
      var r = found[Math.min(state.sel, n - 1)];
      jumpTo(r.buf, r.id);
    } else if (e.key === 'ArrowDown' || (e.ctrlKey && e.key === 'n')) {
      e.preventDefault();
      state.sel = (state.sel + 1) % Math.max(n, 1);
      renderSearch();
    } else if (e.key === 'ArrowUp' || (e.ctrlKey && e.key === 'p')) {
      e.preventDefault();
      state.sel = (state.sel - 1 + Math.max(n, 1)) % Math.max(n, 1);
      renderSearch();
    } else if (e.key === 'Escape') {
      closeSearch();
    }
  }

  /* ── global keys ────────────────────────────────────── */
  function onGlobalKey(e) {
    var inInput = document.activeElement === el.input;
    if (e.ctrlKey && !e.altKey && !e.metaKey && e.key === 's') { e.preventDefault(); openSearch(); return; }
    if (e.key === 'Escape') { closeSearch(); state.prefix = null; return; }
    if (inInput || state.searching) return;
    if (e.ctrlKey && !e.altKey && !e.metaKey && e.key === 'x') {
      e.preventDefault();
      state.prefix = 'C-x';
      msg('C-x-   b: switch buffer · 1: delete-other-windows · g: magit-status', 4000);
      return;
    }
    if (e.ctrlKey || e.altKey || e.metaKey) return;
    if (state.prefix === 'C-x') {
      if (e.key === 'b') { e.preventDefault(); cycleBuffer(); }
      else msg('C-x ' + e.key + ' is undefined (on this site)');
      state.prefix = null;
      return;
    }
    if (e.key === 'n') cycleBuffer(1);
    if (e.key === 'p') cycleBuffer(-1);
  }

  /* ── hash routing ───────────────────────────────────── */
  function navigateHash(quiet) {
    var h = location.hash.replace(/^#/, '');
    if (!h) { switchBuffer('readme', true); return; }
    if (ORDER.indexOf(h) !== -1) { switchBuffer(h, quiet); return; }
    for (var i = 0; i < INDEX.length; i++) {
      if (INDEX[i].id === h) { jumpTo(INDEX[i].buf, h); return; }
    }
  }

  /* ── wiring ─────────────────────────────────────────── */
  el.tabs.forEach(function (t) {
    t.addEventListener('click', function () { switchBuffer(t.dataset.buffer); });
  });
  el.themeBtn.addEventListener('click', toggleTheme);
  el.searchBtn.addEventListener('click', openSearch);
  el.input.addEventListener('input', function () { state.sel = 0; renderSearch(); });
  el.input.addEventListener('keydown', onQueryKey);
  el.buffer.addEventListener('scroll', onScroll);
  window.addEventListener('keydown', onGlobalKey);
  window.addEventListener('hashchange', function () { navigateHash(false); });

  document.querySelectorAll('a[href^="#"]').forEach(function (a) {
    a.addEventListener('click', function (e) {
      e.preventDefault();
      var h = a.getAttribute('href').replace(/^#/, '');
      history.replaceState(null, '', '#' + h);
      navigateHash(false);
    });
  });

  renderTheme();
  navigateHash(true);
})();
