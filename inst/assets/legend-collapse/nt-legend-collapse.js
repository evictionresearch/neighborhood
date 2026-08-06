/* nt-legend-collapse v1.0.2 — collapsible mapgl legends (neighborhood pkg).
 *
 * Adds a chevron to every mapgl legend title; clicking the title (or the
 * chevron) folds the legend down to its title line so the map underneath
 * stays visible. Legends whose map pane is small (multi-pane grids, stacked
 * mobile layouts) start collapsed; a legend the reader toggles by hand keeps
 * their choice. Set window.__NT_LEGEND_NO_AUTO = true before load (print
 * snapshotting does) to keep every legend expanded.
 *
 * Canonical copy: inst/assets/legend-collapse/ in the neighborhood package.
 * Until regenerated widgets carry it natively, built report widgets get the
 * same file injected by hprm_sanmateo/code/o15_legend_collapse.R.
 */
(function () {
  if (window.__ntLegendCollapse) return;
  window.__ntLegendCollapse = true;
  var MIN_W = 480, MIN_H = 340;   // pane sizes below this start collapsed

  function setState(lg, btn, collapsed, manual) {
    lg.classList.toggle('nt-legend-collapsed', collapsed);
    btn.setAttribute('aria-expanded', String(!collapsed));
    btn.setAttribute('aria-label', collapsed ? 'Expand legend' : 'Collapse legend');
    btn.textContent = collapsed ? '▸' : '▾';
    if (manual) lg.setAttribute('data-nt-collapse', 'manual');
  }

  function decorate(lg) {
    if (lg.hasAttribute('data-nt-collapse')) return;
    // direct child only: afford_map nests per-tier legends inside an outer
    // .mapboxgl-legend wrapper — decorate the inner, title-owning ones and
    // leave the wrapper alone (collapsing it would hide every tier at once).
    // Older mapgl builds emit the title as a bare <h2>, newer ones carry
    // .mapgl-legend-title; stamp whichever we find so the CSS has one hook.
    var title = lg.querySelector(':scope > .mapgl-legend-title, :scope > h2');
    if (!title) return;
    if (title.querySelector('.nt-legend-toggle')) return;
    title.classList.add('nt-legend-title');
    lg.setAttribute('data-nt-collapse', 'auto');

    var btn = document.createElement('button');
    btn.className = 'nt-legend-toggle';
    btn.type = 'button';
    title.appendChild(btn);

    function flip(e) {
      e.stopPropagation();
      setState(lg, btn, !lg.classList.contains('nt-legend-collapsed'), true);
    }
    btn.addEventListener('click', flip);
    title.addEventListener('click', flip);

    function auto() {
      if (window.__NT_LEGEND_NO_AUTO) return setState(lg, btn, false, false);
      if (lg.getAttribute('data-nt-collapse') === 'manual') return;
      var host = lg.closest('.maplibregl-map') || lg.parentElement;
      var r = host.getBoundingClientRect();
      if (!r.width && !r.height) return;   // pane not laid out yet
      setState(lg, btn, r.width < MIN_W || r.height < MIN_H, false);
    }
    auto();
    window.addEventListener('resize', auto);
  }

  function scan() {
    document.querySelectorAll('.mapboxgl-legend').forEach(decorate);
  }
  if (document.readyState === 'loading')
    document.addEventListener('DOMContentLoaded', scan);
  scan();
  // legends are added by the widget binding after load — keep watching
  new MutationObserver(scan)
    .observe(document.documentElement, { childList: true, subtree: true });
})();
