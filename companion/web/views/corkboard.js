// Corkboard: acts as sections, chapters as rows, scenes as cards.
// Read-only in this phase.  All text is put in with textContent.

import { matches, isActive } from '../filters.js';

function el(tag, className, text) {
  const node = document.createElement(tag);
  if (className) node.className = className;
  if (text !== undefined) node.textContent = text;
  return node;
}

function sumWords(scenes) {
  return scenes.reduce((total, scene) => total + scene.words, 0);
}

function linkNames(scene, field) {
  const value = scene.fields[field];
  if (!value) return [];
  const names = value.links ? value.links.map((link) => link.name) : [];
  return names.concat(value.unlinked || []);
}

function progress(words, objective, ctx) {
  const wrap = el('div', 'progress');
  const text = objective
    ? ctx.t('companion-ui-words-of', words.toLocaleString(), objective.toLocaleString())
    : ctx.t('companion-ui-words', words.toLocaleString());
  wrap.append(el('span', 'progress-text', text));
  if (objective) {
    const bar = el('div', 'bar');
    const fill = el('div', 'bar-fill');
    fill.style.width = `${Math.min(100, Math.round((words / objective) * 100))}%`;
    bar.append(fill);
    wrap.append(bar);
  }
  return wrap;
}

function todoChip(heading, ctx) {
  if (heading.todo === null) return null;
  const chip = el('span', 'todo-chip', heading.todo);
  const color = ctx.snapshot.todo.colors[heading.todo];
  if (color) chip.style.setProperty('--todo', color);
  if (ctx.snapshot.todo.done.includes(heading.todo)) chip.classList.add('done');
  return chip;
}

function card(scene, ctx) {
  const node = el('article', 'card');
  node.tabIndex = 0;
  if (scene.id) node.dataset.id = scene.id;
  const color = scene.todo !== null && ctx.snapshot.todo.colors[scene.todo];
  if (color) node.style.setProperty('--todo', color);
  if (scene.todo !== null && ctx.snapshot.todo.done.includes(scene.todo)) node.classList.add('done');
  if (isActive(ctx.filters) && !matches(scene, ctx.filters)) node.classList.add('dimmed');
  if (scene.readOnly) node.classList.add('readonly');
  if (scene.id === ctx.focusId) node.classList.add('focus');

  const head = el('header', 'card-head');
  head.append(el('h4', 'card-title', scene.title || ctx.t('companion-ui-untitled')));
  if (scene.id) {
    const open = el('button', 'open', '↗');
    open.type = 'button';
    const label = ctx.t('companion-ui-open');
    open.title = label;
    open.setAttribute('aria-label', label);
    open.addEventListener('click', (event) => { event.stopPropagation(); ctx.goto(scene.id); });
    head.append(open);
  } else {
    head.append(el('span', 'noid', '⚠'));
    head.title = ctx.t('companion-ui-no-id');
  }
  node.append(head);

  const summary = scene.fields.summary;
  node.append(summary
    ? el('p', 'summary', summary.raw)
    : el('p', 'summary empty', ctx.t('companion-ui-no-summary')));

  const meta = el('div', 'meta');
  const pov = linkNames(scene, 'pov');
  if (pov.length) meta.append(el('span', 'pov', pov.join(', ')));
  const points = linkNames(scene, 'plot-point');
  for (const name of points) {
    const badge = el('span', 'badge', name);
    badge.title = ctx.t('companion-ui-plot-point');
    meta.append(badge);
  }
  if (meta.childNodes.length) node.append(meta);

  node.append(progress(scene.words, scene.objective, ctx));

  node.addEventListener('dblclick', () => { if (scene.id) ctx.goto(scene.id); });
  node.addEventListener('keydown', (event) => {
    if (event.key === 'Enter' && scene.id) ctx.goto(scene.id);
  });
  return node;
}

function chapterRow(chapter, ctx) {
  const row = el('div', 'chapter');
  if (chapter.id) row.dataset.id = chapter.id;
  const head = el('div', 'chapter-head');
  head.append(el('h3', 'chapter-title', chapter.title || ctx.t('companion-ui-untitled')));
  const chip = todoChip(chapter, ctx);
  if (chip) head.append(chip);
  head.append(progress(sumWords(chapter.scenes), chapter.objective, ctx));
  row.append(head);
  const cards = el('div', 'cards');
  for (const scene of chapter.scenes) cards.append(card(scene, ctx));
  row.append(cards);
  return row;
}

export function render(container, ctx) {
  container.replaceChildren();
  const acts = ctx.snapshot.acts;
  const total = acts.reduce((n, act) => n + act.chapters.reduce((m, c) => m + c.scenes.length, 0), 0);
  if (total === 0) {
    container.append(el('p', 'empty', ctx.t('companion-ui-empty')));
    return;
  }
  for (const act of acts) {
    const section = el('section', 'act');
    if (act.id) section.dataset.id = act.id;
    const head = el('div', 'act-head');
    head.append(el('h2', 'act-title', act.title || ''));
    const chip = todoChip(act, ctx);
    if (chip) head.append(chip);
    const words = sumWords(act.chapters.flatMap((chapter) => chapter.scenes));
    head.append(progress(words, act.objective, ctx));
    section.append(head);
    for (const chapter of act.chapters) section.append(chapterRow(chapter, ctx));
    container.append(section);
  }
}
