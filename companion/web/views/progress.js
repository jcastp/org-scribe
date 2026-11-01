// The Progress tab: the manuscript's words against its objectives (the book,
// then each act and chapter, from the snapshot) and, when the project has a
// writing plan, where it stands: today's goal, ahead or behind, the streak,
// the pace, and a burndown drawn as inline SVG (no chart library).
//
// The data is the app's (GET /api/progress, one fetch per revision, shared
// with the header chip): the plan's figures are the planner's own, so the
// page and F8 W cannot disagree.  Reading only.  All text goes in with
// textContent.

import { el } from '../edit.js';

const SVG = 'http://www.w3.org/2000/svg';

function svg(tag, attrs = {}, text) {
  const node = document.createElementNS(SVG, tag);
  for (const [key, value] of Object.entries(attrs)) node.setAttribute(key, String(value));
  if (text !== undefined) node.textContent = text;
  return node;
}

const number = (value, ctx) => Math.round(value).toLocaleString(ctx.snapshot.language);

/** A date (YYYY-MM-DD) in the page's language, day and month. */
function date(text, ctx, withYear = false) {
  if (!text) return '';
  const [year, month, day] = text.split('-').map(Number);
  const options = withYear ? { day: 'numeric', month: 'short', year: 'numeric' } : { day: 'numeric', month: 'short' };
  return new Date(year, month - 1, day).toLocaleDateString(ctx.snapshot.language, options);
}

function card(title, className = '') {
  const node = el('section', `hl-card pg-card ${className}`.trim());
  node.append(el('h2', 'hl-title', title));
  return node;
}

/**
 * A labelled bar: WORDS of OBJECTIVE (no objective: the words alone).
 * FOLD, when given, is {open, toggle}: a twisty before the label.  OPEN, when
 * given, is a function the label becomes a button for (a scene's details).
 */
function bar(label, words, objective, ctx, className = '', fold = null, open = null) {
  const row = el('div', `pg-bar ${className}`.trim());
  const name = el('span', 'pg-bar-label');
  if (fold) {
    const twisty = el('button', 'twisty', fold.open ? '▾' : '▸');
    twisty.type = 'button';
    twisty.setAttribute('aria-expanded', String(fold.open));
    twisty.setAttribute('aria-label', ctx.t('companion-ui-collapse'));
    twisty.addEventListener('click', fold.toggle);
    name.append(twisty);
  } else if (className.includes('child') || className.includes('grandchild')) {
    name.append(el('span', 'twisty-gap'));
  }
  if (open) {
    const link = el('button', 'pg-bar-link', label);
    link.type = 'button';
    link.addEventListener('click', open);
    name.append(link);
  } else {
    name.append(el('span', 'pg-bar-text', label));
  }
  name.title = label;
  row.append(name);
  const meter = el('span', 'hl-meter pg-meter');
  const fill = el('span', 'hl-meter-fill');
  const percent = objective ? Math.min(100, (100 * words) / objective) : 0;
  fill.style.width = `${percent}%`;
  if (objective && words >= objective) fill.classList.add('done');
  meter.append(fill);
  const figure = objective
    ? ctx.t('companion-ui-progress-of', number(words, ctx), number(objective, ctx))
    : ctx.t('companion-ui-progress-words', number(words, ctx));
  row.append(meter, el('span', 'pg-bar-figure', figure));
  return row;
}

// ---- the manuscript -------------------------------------------------------

function manuscript(data, ctx) {
  const { words, objective, wordTarget } = data.manuscript;
  const node = card(ctx.t('companion-ui-progress-manuscript'));
  const big = el('p', 'pg-big');
  big.append(el('strong', null, number(words, ctx)),
    el('span', null, objective ? ` / ${number(objective, ctx)}` : ''));
  node.append(big);
  if (objective) {
    node.append(bar(ctx.t('companion-ui-progress-objective'), words, objective, ctx));
  } else {
    node.append(el('p', 'hl-hint', ctx.t('companion-ui-progress-no-objective')));
  }
  if (wordTarget && wordTarget !== objective) {
    node.append(el('p', 'hl-warn', ctx.t('companion-ui-progress-target-differs', number(wordTarget, ctx))));
  }
  return node;
}

// Which groups the user has opened or closed, per project.  Only the toggled
// ones are kept, so a group added later takes its level's default (acts open,
// chapters closed).  View state: nothing is written to the manuscript.
const folds = new Map();
let foldsRoot = null;
const foldKey = (root) => `org-scribe.progress:${root}`;

function loadFolds(root) {
  folds.clear();
  foldsRoot = root;
  try {
    const raw = JSON.parse(localStorage.getItem(foldKey(root)) || '{}') || {};
    for (const [key, value] of Object.entries(raw)) if (typeof value === 'boolean') folds.set(key, value);
  } catch (error) { /* private mode or a damaged entry: start afresh */ }
}

function saveFolds() {
  try { localStorage.setItem(foldKey(foldsRoot), JSON.stringify(Object.fromEntries(folds))); } catch (error) { /* in memory only */ }
}

const isOpen = (key, fallback) => (folds.has(key) ? folds.get(key) : fallback);

/** Open or close every group in KEYS. */
function setAll(keys, open, ctx) {
  for (const key of keys) folds.set(key, open);
  saveFolds();
  ctx.rerender();
}

function parts(ctx) {
  const node = card(ctx.t('companion-ui-progress-parts'), 'wide');
  const shortStory = ctx.snapshot.project.type === 'short-story';
  const untitled = ctx.t('companion-ui-untitled');
  const groups = [];                               // every key that can fold
  const fold = (key, defaultOpen) => {
    groups.push(key);
    const open = isOpen(key, defaultOpen);
    return { open, toggle: () => { folds.set(key, !open); saveFolds(); ctx.rerender(); } };
  };
  const sceneBar = (scene, className) => bar(scene.title || untitled, scene.words, scene.objective, ctx, className, null,
    scene.id ? () => ctx.openDetail(scene.id) : null);
  const body = el('div', 'pg-parts');
  ctx.snapshot.acts.forEach((act, actIndex) => {
    if (act.implicit && !act.chapters.some((chapter) => chapter.scenes.length)) return;
    const group = el('div', 'pg-group');
    const actKey = act.id || `act#${actIndex}`;
    const actFold = act.chapters.length ? fold(actKey, true) : null;
    group.append(bar(act.title || untitled, act.words, act.objective, ctx, 'act', actFold));
    if (!actFold || actFold.open) {
      act.chapters.forEach((chapter, chapterIndex) => {
        if (shortStory || chapter.passthrough) {
          for (const scene of chapter.scenes) group.append(sceneBar(scene, 'child'));
          return;
        }
        const chapterKey = chapter.id || `${actKey}/chapter#${chapterIndex}`;
        const chapterFold = chapter.scenes.length ? fold(chapterKey, false) : null;
        group.append(bar(chapter.title || untitled, chapter.words, chapter.objective, ctx, 'child', chapterFold));
        if (chapterFold && chapterFold.open) {
          for (const scene of chapter.scenes) group.append(sceneBar(scene, 'grandchild'));
        }
      });
    }
    body.append(group);
  });
  // "all" must reach groups inside a closed act too, which were not drawn
  const everything = [];
  ctx.snapshot.acts.forEach((act, actIndex) => {
    const actKey = act.id || `act#${actIndex}`;
    if (act.chapters.length) everything.push(actKey);
    if (shortStory) return;
    act.chapters.forEach((chapter, chapterIndex) => {
      if (!chapter.passthrough && chapter.scenes.length) everything.push(chapter.id || `${actKey}/chapter#${chapterIndex}`);
    });
  });
  if (everything.length) {
    const tools = el('div', 'pg-tools');
    const open = el('button', 'pg-tool', ctx.t('companion-ui-progress-expand-all'));
    const close = el('button', 'pg-tool', ctx.t('companion-ui-progress-collapse-all'));
    for (const button of [open, close]) button.type = 'button';
    open.addEventListener('click', () => setAll(everything, true, ctx));
    close.addEventListener('click', () => setAll(everything, false, ctx));
    tools.append(open, close);
    node.append(tools);
  }
  node.append(body);
  return node;
}

// ---- the plan -------------------------------------------------------------

function standing(plan, ctx) {
  if (plan.status === 'ahead') return ctx.t('companion-ui-progress-ahead', number(plan.difference, ctx));
  if (plan.status === 'behind') return ctx.t('companion-ui-progress-behind', number(-plan.difference, ctx));
  return ctx.t('companion-ui-progress-on-track');
}

const TRENDS = {
  accelerating: 'companion-ui-progress-trend-up',
  slowing: 'companion-ui-progress-trend-down',
  steady: 'companion-ui-progress-trend-steady',
};

function planCard(plan, ctx) {
  const node = card(ctx.t('companion-ui-progress-plan', plan.title));
  if (plan.phase === 'before') node.append(el('p', 'hl-hint', ctx.t('companion-ui-progress-starts', date(plan.start, ctx, true))));
  if (plan.phase === 'after') node.append(el('p', 'hl-hint', ctx.t('companion-ui-progress-ended', date(plan.end, ctx, true))));
  if (plan.phase === 'during') {
    if (plan.todayTarget === 0) {
      node.append(el('p', 'pg-today', ctx.t('companion-ui-progress-rest-day', number(plan.todayActual, ctx))));
    } else if (plan.todayTarget !== null) {
      node.append(bar(ctx.t('companion-ui-progress-today'), plan.todayActual, plan.todayTarget, ctx, 'today'));
    }
  }
  node.append(bar(ctx.t('companion-ui-progress-plan-total'), plan.current, plan.total, ctx));
  const facts = el('dl', 'hl-facts');
  const fact = (label, value) => facts.append(el('dt', null, label), el('dd', null, value));
  const status = el('dd', `pg-status ${plan.status || ''}`, standing(plan, ctx));
  facts.append(el('dt', null, ctx.t('companion-ui-progress-standing')), status);
  if (plan.daysAhead && Math.abs(plan.daysAhead) >= 0.5) {
    fact('', ctx.t(plan.daysAhead > 0 ? 'companion-ui-progress-days-ahead' : 'companion-ui-progress-days-behind',
      number(Math.abs(plan.daysAhead), ctx)));
  }
  fact(ctx.t('companion-ui-progress-streak'), ctx.t('companion-ui-progress-streak-value', plan.streak.current, plan.streak.longest));
  fact(ctx.t('companion-ui-progress-pace'), ctx.t('companion-ui-progress-pace-value',
    number(plan.velocity.average, ctx), number(plan.velocity.recent, ctx)));
  if (TRENDS[plan.velocity.trend]) fact(ctx.t('companion-ui-progress-trend'), ctx.t(TRENDS[plan.velocity.trend]));
  if (plan.velocity.eta) fact(ctx.t('companion-ui-progress-eta'), date(plan.velocity.eta, ctx, true));
  if (plan.velocity.projected && plan.velocity.projected !== plan.velocity.eta) {
    fact(ctx.t('companion-ui-progress-projected'), date(plan.velocity.projected, ctx, true));
  }
  if (plan.end) fact(ctx.t('companion-ui-progress-end'), date(plan.end, ctx, true));
  node.append(facts);
  node.append(el('p', 'hl-hint', ctx.t('companion-ui-progress-plan-hint')));
  return node;
}

/** The burndown: words left, planned (dashed) against written (solid). */
function burndown(plan, ctx) {
  const days = plan.days || [];
  if (days.length < 2) return null;
  const node = card(ctx.t('companion-ui-progress-burndown'), 'wide');
  const W = 800;
  const H = 260;
  const pad = { left: 64, right: 16, top: 14, bottom: 30 };
  const start = days[0].planned - days[0].goal;      // where the plan starts from
  const remaining = (value) => Math.max(0, plan.total - value);
  const top = Math.max(remaining(start), ...days.map((day) => (day.actual === null ? 0 : remaining(day.actual))), 1);
  const x = (index) => pad.left + ((W - pad.left - pad.right) * index) / days.length;
  const y = (value) => pad.top + (H - pad.top - pad.bottom) * (1 - value / top);
  const chart = svg('svg', { viewBox: `0 0 ${W} ${H}`, class: 'pg-burndown', role: 'img' });
  chart.append(svg('title', {}, ctx.t('companion-ui-progress-burndown-label',
    number(remaining(plan.current), ctx), date(plan.end, ctx, true))));
  // axes and the two value marks
  chart.append(svg('line', { x1: pad.left, y1: y(0), x2: W - pad.right, y2: y(0), class: 'pg-axis' }));
  chart.append(svg('line', { x1: pad.left, y1: pad.top, x2: pad.left, y2: y(0), class: 'pg-axis' }));
  chart.append(svg('text', { x: pad.left - 6, y: y(top) + 4, class: 'pg-tick end' }, number(top, ctx)));
  chart.append(svg('text', { x: pad.left - 6, y: y(0) + 4, class: 'pg-tick end' }, '0'));
  chart.append(svg('text', { x: pad.left, y: H - 8, class: 'pg-tick' }, date(days[0].date, ctx)));
  chart.append(svg('text', { x: W - pad.right, y: H - 8, class: 'pg-tick end' }, date(days[days.length - 1].date, ctx)));
  // planned: from the start point through each day's end
  const planned = [`${x(0)},${y(remaining(start))}`]
    .concat(days.map((day, index) => `${x(index + 1)},${y(remaining(day.planned))}`));
  chart.append(svg('polyline', { points: planned.join(' '), class: 'pg-planned' }));
  // written: up to today
  const written = [`${x(0)},${y(remaining(start))}`];
  days.forEach((day, index) => { if (day.actual !== null) written.push(`${x(index + 1)},${y(remaining(day.actual))}`); });
  if (written.length > 1) chart.append(svg('polyline', { points: written.join(' '), class: 'pg-written' }));
  // today
  const today = days.findIndex((day) => day.date === plan.today);
  if (today >= 0) {
    const tx = x(today + 1);
    chart.append(svg('line', { x1: tx, y1: pad.top, x2: tx, y2: y(0), class: 'pg-today-line' }));
    chart.append(svg('text', { x: tx + 4, y: y(0) - 6, class: 'pg-tick' }, ctx.t('companion-ui-progress-today-mark')));
  }
  node.append(chart);
  const legend = el('div', 'pg-legend');
  legend.append(el('span', 'pg-key planned', ctx.t('companion-ui-progress-legend-planned')),
    el('span', 'pg-key written', ctx.t('companion-ui-progress-legend-written')));
  node.append(legend);
  return node;
}

function noPlan(state, ctx) {
  const node = card(ctx.t('companion-ui-progress-no-plan-title'));
  node.append(el('p', 'hl-hint', ctx.t(state === 'off' ? 'companion-ui-progress-plan-off' : 'companion-ui-progress-plan-none')));
  return node;
}

export function render(container, ctx) {
  const data = ctx.progress();
  if (!data || !data.ok) {
    container.replaceChildren(el('p', 'empty', ctx.t(data ? 'companion-ui-progress-failed' : 'companion-ui-health-loading')));
    return;
  }
  if (foldsRoot !== ctx.snapshot.project.root) loadFolds(ctx.snapshot.project.root);
  const page = el('div', 'progress-tab');
  const top = el('div', 'hl-grid');
  top.append(manuscript(data, ctx));
  if (data.plan.state === 'active') top.append(planCard(data.plan, ctx));
  else top.append(noPlan(data.plan.state, ctx));
  page.append(top);
  if (data.plan.state === 'active') {
    const chart = burndown(data.plan, ctx);
    if (chart) page.append(chart);
  }
  page.append(parts(ctx));
  container.replaceChildren(page);
}
