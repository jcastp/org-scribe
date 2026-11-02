// The Health tab: the project health report (F8 H) as a page, every finding
// acting on something.  The data is GET /api/health, the same plist the
// Emacs report is drawn from (org-scribe--health-data), fetched when the tab
// is shown and again when the project's revision changes.  It is not part of
// the snapshot: it costs a scan of both files.
//
// What a finding does:
//   a scene missing a field   -> the detail panel, with that field's picker open
//   a link to a deleted entity -> Show in Emacs, or Remove the link (remove-link)
//   an orphaned entity        -> its form in the Entities tab
//   a plot point no scene serves -> its row in the storyline
//   story-time problems       -> the detail panel, on the scene's Timeline
//   gate, word target, plan   -> read only; "Full report in Emacs" (show-health)

let loaded = null;     // { revision, data } or { revision, error }
let fetching = null;   // the revision being fetched
let fetchedAt = 0;     // when the last fetch began
let pause = null;      // the timer that fetches after the pause below

// The report costs Emacs a scan of both files (about 2 s in a long book) and
// every keystroke in Emacs moves the revision: while the writer types, the
// old report stays and the new one is fetched at most this often.  The page's
// own changes (removing a link) refetch at once.
const MIN_GAP_MS = 8000;

function el(tag, className, text) {
  const node = document.createElement(tag);
  if (className) node.className = className;
  if (text !== undefined && text !== null) node.textContent = text;
  return node;
}

function button(text, className, onClick, title) {
  const node = el('button', className, text);
  node.type = 'button';
  if (title) node.title = title;
  node.addEventListener('click', onClick);
  return node;
}

const number = (value, ctx) => Number(value).toLocaleString(ctx.snapshot.language);

/** A titled card; COUNT, when given, is a badge (0 reads as all clear). */
function card(title, count, className = '') {
  const node = el('section', `hl-card ${className}`.trim());
  const head = el('h2', 'hl-title', title);
  if (count !== undefined) {
    const badge = el('span', count ? 'hl-count' : 'hl-count clear', String(count));
    head.append(' ', badge);
  }
  node.append(head);
  return node;
}

function hint(text) { return el('p', 'hl-hint', text); }

/** A scene as a button: opens the detail panel (on FIELD when given). */
function sceneButton(scene, ctx, field) {
  const label = scene.text ? `${scene.title} (${scene.text})` : scene.title;
  if (!scene.id) return el('span', 'hl-chip static', label);
  return button(label, 'hl-chip', () => ctx.openDetail(scene.id, field));
}

/** A labelled group of scene buttons, or nothing when SCENES is empty. */
function sceneGroup(label, scenes, ctx, field) {
  if (!scenes.length) return null;
  const node = el('div', 'hl-group');
  node.append(el('h3', null, label));
  const list = el('div', 'hl-chips');
  for (const scene of scenes) list.append(sceneButton(scene, ctx, field));
  node.append(list);
  return node;
}

function append(parent, ...children) {
  for (const child of children) if (child) parent.append(child);
}

// ---- read only -----------------------------------------------------------

function overview(data, ctx) {
  const { overview: o } = data;
  const node = card(ctx.t('companion-ui-health-overview'));
  const list = el('dl', 'hl-facts');
  const fact = (label, value) => { list.append(el('dt', null, label), el('dd', null, value)); };
  fact(ctx.t('companion-ui-health-scenes'), number(o.scenes, ctx));
  fact(ctx.t('companion-ui-health-words'), number(o.words, ctx));
  if (o.wordTarget !== null) fact(ctx.t('companion-ui-health-word-target'), number(o.wordTarget, ctx));
  if (o.progress !== null) {
    if (o.objective !== o.wordTarget) fact(ctx.t('companion-ui-health-objective'), number(o.objective, ctx));
    const bar = el('dd', 'hl-progress');
    const meter = el('span', 'hl-meter');
    const fill = el('span', 'hl-meter-fill');
    fill.style.width = `${Math.min(100, o.progress)}%`;
    meter.append(fill);
    bar.append(meter, el('span', null, `${o.progress.toLocaleString(ctx.snapshot.language, { maximumFractionDigits: 1 })} %`));
    list.append(el('dt', null, ctx.t('companion-ui-health-progress')), bar);
  } else {
    fact(ctx.t('companion-ui-health-objective'), ctx.t('companion-ui-health-not-set'));
  }
  fact(ctx.t('companion-ui-health-open-scenes'), number(data.open, ctx));
  node.append(list);
  if (o.drift) node.append(el('p', 'hl-warn', o.drift));
  if (o.spelling) node.append(el('p', 'hl-warn', `${ctx.t('companion-ui-health-spelling')}: ${o.spelling}`));
  return node;
}

function plan(data, ctx) {
  if (!data.plan) return null;
  const node = card(ctx.t('companion-ui-health-plan'));
  const list = el('ul', 'hl-lines');
  for (const line of data.plan.lines) list.append(el('li', null, line));
  node.append(list);
  return node;
}

function gate(data, ctx) {
  const g = data.gate;
  if (!g) return null;
  const node = card(ctx.t('companion-ui-health-gate', g.ticked, g.total), undefined, g.crossed ? 'crossed' : '');
  const list = el('ul', 'hl-checks');
  for (const item of g.items) {
    const row = el('li', item.checked ? 'on' : null);
    row.append(el('span', 'hl-box', item.checked ? '☑' : '☐'), el('span', null, item.label));
    list.append(row);
  }
  node.append(list);
  if (g.points) node.append(el('p', null, ctx.t('companion-ui-health-gate-points', g.points[0], g.points[1])));
  node.append(el('p', null, ctx.t('companion-ui-health-gate-scenes', g.scenes)));
  if (g.notes.length) {
    node.append(el('p', 'hl-warn', `${ctx.t('companion-ui-health-gate-worth')} ${g.notes.join('; ')}.`));
  }
  if (g.crossed) node.append(el('p', 'hl-strong', ctx.t('companion-ui-health-gate-crossed')));
  return node;
}

function table(headers, rows, numeric) {
  const node = el('table', 'hl-table');
  const head = el('tr');
  headers.forEach((text, index) => head.append(el('th', numeric[index] ? 'num' : null, text)));
  node.append(head);
  for (const row of rows) {
    const tr = el('tr', row.className || null);
    row.cells.forEach((text, index) => tr.append(el('td', numeric[index] ? 'num' : null, text)));
    node.append(tr);
  }
  return node;
}

function numbers(data, ctx) {
  const node = card(ctx.t('companion-ui-health-numbers'), undefined, 'wide');
  const grid = el('div', 'hl-tables');
  const status = el('div');
  status.append(el('h3', null, ctx.t('companion-ui-health-status')),
    table([ctx.t('companion-ui-health-col-state'), ctx.t('companion-ui-health-col-scenes')],
      data.status.map((cell) => ({
        cells: [cell.state === '(none)' ? ctx.t('companion-ui-no-todo') : cell.state, number(cell.count, ctx)],
      })), [false, true]));
  const words = data.overview.words;
  const share = el('div');
  share.append(el('h3', null, ctx.t('companion-ui-health-pov-share')),
    table([ctx.t('companion-ui-health-col-pov'), ctx.t('companion-ui-health-col-scenes'),
      ctx.t('companion-ui-health-col-words'), '%'],
    data.povShare.map((row) => ({
      cells: [row.name, number(row.scenes, ctx), number(row.words, ctx),
        words ? (100 * row.words / words).toLocaleString(ctx.snapshot.language, { maximumFractionDigits: 1 }) : '0'],
    })), [false, true, true, true]));
  grid.append(status, share);
  if (data.chapters) {
    const chapters = el('div');
    chapters.append(el('h3', null, ctx.t('companion-ui-health-chapters')),
      table([ctx.t('companion-ui-health-col-chapter'), ctx.t('companion-ui-health-col-words')],
        data.chapters.rows.map((row) => ({
          className: row.outlier ? 'outlier' : null,
          cells: [row.title, `${number(row.words, ctx)}${row.outlier ? ' *' : ''}`],
        })), [false, true]),
      hint(data.chapters.summary));
    if (data.chapters.rows.some((row) => row.outlier)) chapters.append(hint(ctx.t('companion-ui-health-outlier')));
    grid.append(chapters);
  }
  node.append(grid);
  return node;
}

// ---- findings that act ---------------------------------------------------

/** The name the page gives a scene field (the project language's). */
function fieldName(key, ctx) {
  const field = (ctx.snapshot.sceneFields || []).find((candidate) => candidate.key === key);
  return field ? field.name : key;
}

function missing(data, ctx) {
  const total = data.missing.reduce((sum, entry) => sum + entry.scenes.length, 0);
  const node = card(ctx.t('companion-ui-health-missing'), total);
  if (!total) { node.append(hint(ctx.t('companion-ui-health-missing-none'))); return node; }
  node.append(hint(ctx.t('companion-ui-health-missing-hint')));
  for (const entry of data.missing) {
    append(node, sceneGroup(ctx.t('companion-ui-health-missing-field', fieldName(entry.key, ctx), entry.scenes.length),
      entry.scenes, ctx, entry.key));
  }
  return node;
}

function broken(data, ctx) {
  const node = card(ctx.t('companion-ui-health-broken'), data.broken.length);
  if (!data.broken.length) { node.append(hint(ctx.t('companion-ui-health-broken-none'))); return node; }
  node.append(hint(ctx.t('companion-ui-health-broken-hint')));
  const list = el('ul', 'hl-rows');
  for (const link of data.broken) {
    const row = el('li');
    const what = el('span', 'hl-what');
    what.append(el('strong', null, link.title), ' · ',
      el('span', null, `${link.key ? fieldName(link.key, ctx) : link.property}: ${link.name}`));
    const actions = el('span', 'hl-actions');
    if (link.id) {
      actions.append(button(ctx.t('companion-ui-health-show-in-emacs'), 'hl-small', () => ctx.goto(link.id)));
      if (link.key) {
        actions.append(button(ctx.t('companion-ui-health-remove-link'), 'hl-small', async () => {
          const answer = await ctx.edit('remove-link', { id: link.id, key: link.key, target: link.target });
          if (answer.ok) ctx.notice(ctx.t('companion-ui-health-link-removed', link.name));
        }));
      }
    }
    row.append(what, actions);
    list.append(row);
  }
  node.append(list);
  return node;
}

const ORPHAN_KINDS = [
  ['character', 'companion-ui-group-characters'],
  ['location', 'companion-ui-group-locations'],
  ['object', 'companion-ui-group-objects'],
  ['plot', 'companion-ui-group-plots'],
];

function orphans(data, ctx) {
  const total = ORPHAN_KINDS.reduce((sum, [kind]) => sum + data.orphans[kind].length, 0);
  const node = card(ctx.t('companion-ui-health-orphans'), total);
  if (!total) node.append(hint(ctx.t('companion-ui-health-orphans-none')));
  else node.append(hint(ctx.t('companion-ui-health-orphans-hint')));
  for (const [kind, title] of ORPHAN_KINDS) {
    const entities = data.orphans[kind];
    if (!entities.length) continue;
    const group = el('div', 'hl-group');
    group.append(el('h3', null, `${ctx.t(title)} (${entities.length})`));
    const chips = el('div', 'hl-chips');
    for (const entity of entities) {
      chips.append(button(entity.name, 'hl-chip', () => ctx.openEntity(entity.id)));
    }
    group.append(chips);
    node.append(group);
  }
  return node;
}

function plotPoints(data, ctx) {
  const points = data.orphans.plotPoint;
  if (!points) return null;     // the project defines no plot points
  const node = card(ctx.t('companion-ui-health-points'), points.length);
  if (!points.length) { node.append(hint(ctx.t('companion-ui-health-points-none'))); return node; }
  node.append(hint(ctx.t('companion-ui-health-points-hint')));
  const chips = el('div', 'hl-chips');
  for (const point of points) {
    chips.append(button(point.name, 'hl-chip', () => ctx.showInStoryline(point.id)));
  }
  node.append(chips);
  return node;
}

function storyTime(data, ctx) {
  const s = data.storyTime;
  if (!s.total) return null;
  const problems = s.missing.length + s.unreadable.length + s.noAnchor.length + s.incomplete.length;
  const node = card(ctx.t('companion-ui-health-time'), problems);
  node.append(hint(ctx.t('companion-ui-health-time-summary', s.total - problems, s.total)));
  if (s.missing.length === s.total) {
    node.append(hint(ctx.t('companion-ui-health-time-none')));
  } else {
    append(node, sceneGroup(ctx.t('companion-ui-health-time-missing', s.missing.length), s.missing, ctx, 'timeline'));
  }
  append(node,
    sceneGroup(ctx.t('companion-ui-health-time-unreadable', s.unreadable.length), s.unreadable, ctx, 'timeline'),
    sceneGroup(ctx.t('companion-ui-health-time-incomplete', s.incomplete.length), s.incomplete, ctx, 'timeline'),
    sceneGroup(ctx.t('companion-ui-health-time-no-anchor', s.noAnchor.length), s.noAnchor, ctx, 'timeline'));
  if (s.jumps.length) {
    const group = el('div', 'hl-group');
    group.append(el('h3', null, ctx.t('companion-ui-health-time-jumps', s.jumps.length)));
    const chips = el('div', 'hl-chips');
    for (const jump of s.jumps) {
      chips.append(sceneButton({ id: jump.id, title: jump.title, text: jump.distance }, ctx, 'timeline'));
    }
    group.append(chips);
    node.append(group);
  }
  return node;
}

function duplicates(data, ctx) {
  if (!data.duplicates.length) return null;
  const node = card(ctx.t('companion-ui-health-duplicates'), data.duplicates.length);
  node.append(hint(ctx.t('companion-ui-health-duplicates-hint')));
  const list = el('ul', 'hl-lines');
  for (const dup of data.duplicates) {
    list.append(el('li', null, ctx.t('companion-ui-health-duplicate-row', dup.kind, dup.name, dup.count)));
  }
  node.append(list);
  return node;
}

// ---- the tab -------------------------------------------------------------

function draw(container, data, ctx) {
  const page = el('div', 'health');
  const bar = el('div', 'hl-bar');
  bar.append(hint(ctx.t('companion-ui-health-intro')),
    button(ctx.t('companion-ui-health-open-report'), 'hl-report',
      () => ctx.edit('show-health', {}), ctx.t('companion-ui-health-open-report-hint')));
  const findings = el('div', 'hl-grid');
  append(findings, missing(data, ctx), broken(data, ctx), orphans(data, ctx), plotPoints(data, ctx),
    storyTime(data, ctx), duplicates(data, ctx));
  const status = el('div', 'hl-grid');
  append(status, overview(data, ctx), gate(data, ctx), plan(data, ctx));
  page.append(bar, findings, status, numbers(data, ctx));
  container.replaceChildren(page);
}

export function render(container, ctx) {
  const { revision } = ctx.snapshot;
  if (loaded && loaded.revision === revision) {
    if (loaded.error) container.replaceChildren(el('p', 'empty', ctx.t('companion-ui-health-failed')));
    else draw(container, loaded.data, ctx);
    return;
  }
  // a new revision keeps the old report on screen until the new one is in
  if (loaded && loaded.data) draw(container, loaded.data, ctx);
  else container.replaceChildren(el('p', 'empty', ctx.t('companion-ui-health-loading')));
  if (fetching === revision) return;
  const wait = fetchedAt + MIN_GAP_MS - Date.now();
  if (loaded && loaded.data && wait > 0 && !ctx.recentEdit()) {
    clearTimeout(pause);
    pause = setTimeout(() => { if (container.isConnected && ctx.isShown('health')) ctx.rerender(); }, wait);
    return;
  }
  clearTimeout(pause);
  fetchedAt = Date.now();
  fetching = revision;
  ctx.fetchHealth()
    .then((answer) => { loaded = { revision, data: answer.health }; })
    .catch(() => { loaded = { revision, error: true }; })
    .finally(() => {
      fetching = null;
      // still the tab shown, for the same revision: draw it
      if (container.isConnected && ctx.isShown('health')) ctx.rerender();
    });
}

/** Forget the report (another project). */
export function useProject() { loaded = null; fetching = null; fetchedAt = 0; clearTimeout(pause); }
