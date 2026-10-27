// Storyline: scenes across (chapter separators on top), entities down,
// grouped as characters, plot threads, locations and plot points.  A filled
// cell is the PoV, a dot is "present", an empty cell is "absent".  Clicking a
// cell toggles the link in the scene; Shift+click on a character sets or
// clears the PoV; clicking a row header opens the entity in Emacs.
// Entities with a negative Weight are left out, with a "not shown" line under
// the group and a switch to show them.  All text is put in with textContent.

import { matches, isActive } from '../filters.js';
import { el } from '../edit.js';

const GROUPS = [
  { kind: 'character', field: 'characters', title: 'companion-ui-group-characters' },
  { kind: 'plot', field: 'plot', title: 'companion-ui-group-plots' },
  { kind: 'location', field: 'location', title: 'companion-ui-group-locations' },
  { kind: 'plot-point', field: 'plot-point', title: 'companion-ui-group-plot-points' },
];

let showHidden = false;

function ids(scene, field) {
  const value = scene.fields[field];
  return value && value.links ? value.links.map((link) => link.id) : [];
}

function editable(scene) {
  return Boolean(scene.id) && !scene.readOnly;
}

/** Cells of a plot thread's row that lie in a gap longer than the threshold. */
function gapCells(scenes, entityId, threshold) {
  const flagged = new Set();
  const universe = scenes.filter((scene) => scene.fields.plot);
  let run = [];
  let seen = false;
  for (const scene of universe) {
    if (ids(scene, 'plot').includes(entityId)) {
      if (seen && run.length > threshold) for (const id of run) flagged.add(id);
      run = [];
      seen = true;
    } else {
      run.push(scene);
    }
  }
  return new Set([...flagged].map((scene) => scenes.indexOf(scene)));
}

function toggleLink(scene, group, entityId, ctx) {
  const value = scene.fields[group.field] || {};
  const current = ids(scene, group.field);
  const next = current.includes(entityId)
    ? current.filter((id) => id !== entityId)
    : [...current, entityId];
  ctx.edit('set-field', {
    id: scene.id, key: group.field, links: next, unlinked: value.unlinked || [],
  });
}

function togglePov(scene, entityId, ctx) {
  const isPov = ids(scene, 'pov').includes(entityId);
  ctx.edit('set-field', { id: scene.id, key: 'pov', links: isPov ? [] : [entityId] });
}

function byWeight(a, b) {
  // hidden entities (negative weight) go last, not first
  return (Boolean(a.hidden) - Boolean(b.hidden)) || (a.weight - b.weight) || a.name.localeCompare(b.name);
}

function cell(scene, group, entity, flags, ctx) {
  const td = el('td', 'sl-cell');
  const present = ids(scene, group.field).includes(entity.id);
  const pov = group.kind === 'character' && ids(scene, 'pov').includes(entity.id);
  if (isActive(ctx.filters) && !matches(scene, ctx.filters)) td.classList.add('dimmed');
  if (flags.gap) td.classList.add('gap');
  const button = el('button', 'sl-toggle');
  button.type = 'button';
  button.setAttribute('aria-pressed', String(present));
  button.setAttribute('aria-label', `${entity.name} / ${scene.title}`);
  button.title = `${entity.name} / ${scene.title}`;
  if (pov) { button.classList.add('pov'); button.textContent = present ? '●' : '○'; button.title += ` (${ctx.t('companion-ui-pov')})`; }
  else if (present) { button.classList.add('present'); button.textContent = '•'; }
  if (!editable(scene)) button.disabled = true;
  button.addEventListener('click', (event) => {
    if (event.shiftKey && group.kind === 'character') togglePov(scene, entity.id, ctx);
    else toggleLink(scene, group, entity.id, ctx);
  });
  td.append(button);
  return td;
}

function rowHeader(entity, group, ctx, info) {
  const th = el('th', 'sl-name');
  th.scope = 'row';
  const open = el('button', 'sl-open', entity.name);
  open.type = 'button';
  open.title = ctx.t('companion-ui-open');
  open.addEventListener('click', () => ctx.goto(entity.id));
  th.append(open);
  if (entity.hidden) th.classList.add('hidden-entity');
  if (info.gap > ctx.snapshot.storyline.gapThreshold) {
    const warning = el('span', 'sl-warning', '⚠');
    warning.title = ctx.t('companion-ui-gap-warning', info.gap);
    th.append(warning);
  }
  if (info.orphan) {
    const badge = el('span', 'sl-orphan', ctx.t('companion-ui-no-scene'));
    th.append(badge);
  }
  return th;
}

function groupRows(group, scenes, ctx, columns) {
  const { storyline } = ctx.snapshot;
  const all = (ctx.snapshot.entities[group.kind] || []).filter((entity) => entity.id);
  // the plot points keep the method's order; weights and names would give 1, 10, 11…
  const order = group.kind === 'plot-point' ? (list) => list : (list) => list.sort(byWeight);
  const shown = order(all.filter((entity) => showHidden || !entity.hidden));
  const left = order(all.filter((entity) => !showHidden && entity.hidden));
  const orphans = new Set(storyline.orphans[group.kind] || []);
  const rows = [];

  const head = el('tr', 'sl-group');
  const title = el('th', '', ctx.t(group.title));
  title.colSpan = columns + 1;
  head.append(title);
  rows.push(head);

  for (const entity of shown) {
    const row = el('tr', 'sl-row');
    row.dataset.id = entity.id;
    const gaps = group.kind === 'plot'
      ? gapCells(scenes, entity.id, storyline.gapThreshold) : new Set();
    row.append(rowHeader(entity, group, ctx, {
      gap: group.kind === 'plot' ? storyline.plotGaps[entity.id] || 0 : 0,
      orphan: orphans.has(entity.id),
    }));
    scenes.forEach((scene, index) => row.append(cell(scene, group, entity, { gap: gaps.has(index) }, ctx)));
    rows.push(row);
  }
  if (left.length) {
    const note = el('tr', 'sl-hidden-note');
    const text = el('td', '', ctx.t('companion-ui-not-shown', left.map((entity) => entity.name).join(', ')));
    text.colSpan = columns + 1;
    note.append(text);
    rows.push(note);
  }
  return rows;
}

function header(acts, scenes, ctx) {
  const chapters = el('tr', 'sl-chapters');
  chapters.append(el('th', 'sl-corner'));
  const numbers = el('tr', 'sl-scenes');
  numbers.append(el('th', 'sl-corner'));
  let number = 0;
  for (const act of acts) {
    for (const chapter of act.chapters) {
      if (!chapter.scenes.length) continue;
      const th = el('th', 'sl-chapter', chapter.title);
      th.colSpan = chapter.scenes.length;
      th.title = chapter.title;
      chapters.append(th);
      chapter.scenes.forEach((scene, index) => {
        number += 1;
        const cellNode = el('th', 'sl-scene');
        if (index === 0) cellNode.classList.add('chapter-start');
        if (scene.id) cellNode.dataset.id = scene.id;
        if (scene.id === ctx.focusId) cellNode.classList.add('focus');
        const link = el('button', 'sl-open', String(number));
        link.type = 'button';
        link.title = `${scene.title} — ${ctx.t('companion-ui-open')}`;
        link.addEventListener('click', () => { if (scene.id) ctx.goto(scene.id); });
        cellNode.append(link);
        numbers.append(cellNode);
      });
    }
  }
  const head = el('thead');
  head.append(chapters, numbers);
  return head;
}

export function render(container, ctx) {
  container.replaceChildren();
  const acts = ctx.snapshot.acts;
  const scenes = acts.flatMap((act) => act.chapters.flatMap((chapter) => chapter.scenes));
  if (!scenes.length) {
    container.append(el('p', 'empty', ctx.t('companion-ui-empty')));
    return;
  }
  const tools = el('div', 'table-tools');
  const label = el('label', 'switch');
  const box = el('input');
  box.type = 'checkbox';
  box.checked = showHidden;
  box.addEventListener('change', () => { showHidden = box.checked; ctx.rerender(); });
  label.append(box, el('span', '', ctx.t('companion-ui-show-hidden')));
  tools.append(label, el('span', 'legend', ctx.t('companion-ui-storyline-legend')));
  container.append(tools);

  const table = el('table', 'storyline');
  table.append(header(acts, scenes, ctx));
  const body = el('tbody');
  for (const group of GROUPS) {
    for (const row of groupRows(group, scenes, ctx, scenes.length)) body.append(row);
  }
  table.append(body);
  const scroller = el('div', 'sl-scroll');
  scroller.append(table);
  container.append(scroller);
}
