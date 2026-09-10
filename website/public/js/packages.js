/*
 * packages.js — live search for the /packages/ page.
 *
 * Fetches the generated /help/api/search-index.json (packages + every
 * function, variable and face the bundled third-party packages define) and
 * filters it client-side, vertico-style. With no query the server-rendered
 * package list stays visible; typing swaps in a ranked result list linking
 * into the generated per-package and per-symbol reference pages.
 *
 * No dependencies. Progressive: if the fetch fails, the static package
 * list rendered by nix/site.nix remains usable.
 */
(function () {
  'use strict';

  var base = (window.JOTAIN_API_BASE || '').replace(/\/$/, '');
  var input = document.getElementById('pkg-q');
  var count = document.getElementById('pkg-count');
  var results = document.getElementById('pkg-results');
  var list = document.getElementById('pkg-list');
  if (!input || !results) return;

  var KIND = { fun: 'function', var: 'variable', face: 'face' };
  var LIMIT = 40;

  var entries = [];   // flattened, searchable
  var sel = 0;
  var ready = false;

  function url(href) { return base + '/' + href; }

  function build(data) {
    var pkgs = (data && data.packages) || [];
    var syms = (data && data.symbols) || [];
    pkgs.forEach(function (p) {
      entries.push({
        kind: 'pkg',
        name: p.name,
        href: p.href,
        meta: (p.count || 0) + (p.count === 1 ? ' symbol' : ' symbols'),
        hay: p.name.toLowerCase()
      });
    });
    syms.forEach(function (s) {
      entries.push({
        kind: s.kind,
        name: s.name,
        href: s.href,
        pkg: s.package,
        meta: (KIND[s.kind] || s.kind) + (s.package ? ' · ' + s.package : ''),
        summary: s.summary || '',
        hay: (s.name + ' ' + (s.package || '')).toLowerCase()
      });
    });
    ready = true;
    input.disabled = false;
    input.placeholder = 'package, function, variable, face…';
  }

  /* Rank: package matches first, then prefix matches, then substring;
     shorter names win ties so exact-ish hits float up. */
  function search(q) {
    var out = [];
    for (var i = 0; i < entries.length; i++) {
      var e = entries[i];
      var at = e.hay.indexOf(q);
      if (at === -1) continue;
      var score = at;
      if (e.kind === 'pkg') score -= 1000;
      if (e.name.toLowerCase().indexOf(q) === 0) score -= 100;
      out.push({ e: e, score: score });
    }
    out.sort(function (a, b) {
      return (a.score - b.score) || (a.e.name.length - b.e.name.length)
        || (a.e.name < b.e.name ? -1 : 1);
    });
    return out.map(function (r) { return r.e; });
  }

  function render() {
    var q = input.value.trim().toLowerCase();
    if (!q) {
      results.hidden = true;
      results.textContent = '';
      if (list) list.hidden = false;
      count.textContent = ready ? entries.filter(function (e) { return e.kind === 'pkg'; }).length + ' packages' : '';
      return;
    }
    if (list) list.hidden = true;
    var found = search(q);
    var shown = found.slice(0, LIMIT);
    if (sel >= shown.length) sel = 0;
    results.hidden = false;
    results.textContent = '';
    shown.forEach(function (e, i) {
      var row = document.createElement('a');
      row.className = 'result' + (i === sel ? ' selected' : '');
      row.href = url(e.href);
      var title = document.createElement('span');
      title.className = 'result-title';
      var code = document.createElement('code');
      code.textContent = e.name;
      title.appendChild(code);
      if (e.summary) {
        var sum = document.createElement('span');
        sum.className = 'result-summary';
        sum.textContent = ' ' + e.summary;
        title.appendChild(sum);
      }
      var meta = document.createElement('span');
      meta.className = 'result-buf';
      meta.textContent = e.meta;
      row.appendChild(title);
      row.appendChild(meta);
      results.appendChild(row);
    });
    var n = found.length;
    count.textContent = n + (n === 1 ? ' match' : ' matches')
      + (n > LIMIT ? ' (showing ' + LIMIT + ')' : '');
  }

  input.addEventListener('input', function () { sel = 0; render(); });
  input.addEventListener('keydown', function (e) {
    var rows = results.querySelectorAll('.result');
    var n = rows.length;
    if (e.key === 'Enter') {
      if (n) { e.preventDefault(); rows[Math.min(sel, n - 1)].click(); }
    } else if (e.key === 'ArrowDown' || (e.ctrlKey && e.key === 'n')) {
      e.preventDefault(); sel = n ? (sel + 1) % n : 0; render();
    } else if (e.key === 'ArrowUp' || (e.ctrlKey && e.key === 'p')) {
      e.preventDefault(); sel = n ? (sel - 1 + n) % n : 0; render();
    } else if (e.key === 'Escape') {
      input.value = ''; sel = 0; render(); input.blur();
    }
  });

  // Focus the search box on "/" or "C-s", Emacs-style.
  window.addEventListener('keydown', function (e) {
    if (document.activeElement === input) return;
    if (e.key === '/' || (e.ctrlKey && !e.altKey && !e.metaKey && e.key === 's')) {
      e.preventDefault(); input.focus();
    }
  });

  input.disabled = true;
  input.placeholder = 'loading index…';
  fetch(url('search-index.json'))
    .then(function (r) { if (!r.ok) throw new Error(r.status); return r.json(); })
    .then(function (data) { build(data); render(); })
    .catch(function () {
      input.placeholder = 'search unavailable — browse the list below';
      count.textContent = '';
    });
})();
