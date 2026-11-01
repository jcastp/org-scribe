// Storyline: scenes across (chapter separators on top), entities down,
// grouped as characters, plot threads, locations and plot points.  A line
// is "present" (consecutive scenes join into one), a bullseye on it is the
// PoV, an empty cell is "absent".  Clicking a
// cell toggles the link in the scene; Shift+click on a character sets or
// clears the PoV; clicking a row header opens the entity in Emacs.
// Entities with a negative Weight are left out, with a "not shown" line under
// the group and a switch to show them.  Plot threads and the main characters
// (Weight up to org-scribe-companion-character-gap-weight) are checked for
// gaps; a plot thread's row header shows its STATUS, and an open thread no
// scene of the last act has gets a badge.  The scene numbers and the row
// headers stay in view while the board scrolls.  All text is put in with
// textContent.

import { matches, isActive } from '../filters.js';
import { el } from '../edit.js';
import { visibleGroups, ids, orderEntities } from './matrix.js';
import { paint, markColor } from '../scheme.js';

let showHidden = false;

/** "Hide the rest" drops the scenes the filters leave out (ctx.hide is only
 * on while a filter is). */
function visibleScene(scene, ctx) {
  return !ctx.hide || matches(scene, ctx.filters);
}

function editable(scene) {
  return Boolean(scene.id) && !scene.readOnly;
}

/** The scene fields that place an entity of KIND in a scene, for the gaps:
 * the same as the storyline data from Emacs (plotGaps, characterGaps). */
const GAP_FIELDS = { plot: ['plot'], character: ['characters', 'pov'] };

/** The largest gap of ENTITY in GROUP, or null when the row is not checked. */
function gapOf(group, entity, storyline) {
  const table = group.kind === 'plot' ? storyline.plotGaps
    : group.kind === 'character' ? storyline.characterGaps || {} : null;
  if (!table) return null;
  if (group.kind === 'plot') return table[entity.id] || 0;
  return entity.id in table ? table[entity.id] : null;
}

/** The scenes of a row that lie in a gap longer than the threshold.  FIELDS
 * place the entity in a scene; the scenes counted are those with one of them
 * filled in.  Counted over every scene, so hiding some does not move a gap. */
function gapCells(scenes, entityId, fields, threshold) {
  const flagged = new Set();
  const filled = (scene) => fields.some((field) => (scene.fields[field] || {}).raw);
  const universe = scenes.filter(filled);
  let run = [];
  let seen = false;
  for (const scene of universe) {
    if (fields.some((field) => ids(scene, field).includes(entityId))) {
      if (seen && run.length > threshold) for (const id of run) flagged.add(id);
      run = [];
      seen = true;
    } else {
      run.push(scene);
    }
  }
  return flagged;
}

/** Say what a click changed: a single click writes to the manuscript, and in
 * a grid you are only looking at it is easy not to notice. */
function announce(ctx, fieldKey, entity, scene, added) {
  const field = ctx.snapshot.sceneFields.find((candidate) => candidate.key === fieldKey);
  ctx.notice(ctx.t(added ? 'companion-ui-storyline-added' : 'companion-ui-storyline-removed',
                   field ? field.name : fieldKey, entity.name, scene.title));
}

async function toggleLink(scene, group, entity, ctx) {
  // a change, not the whole list: two quick clicks must not undo each other
  const present = ids(scene, group.field).includes(entity.id);
  const answer = await ctx.edit('set-field', {
    id: scene.id, key: group.field, [present ? 'remove' : 'add']: [entity.id],
  });
  if (answer.ok) announce(ctx, group.field, entity, scene, !present);
}

async function togglePov(scene, entity, ctx) {
  const isPov = ids(scene, 'pov').includes(entity.id);
  const answer = await ctx.edit('set-field', {
    id: scene.id, key: 'pov', [isPov ? 'remove' : 'add']: [entity.id],
  });
  if (answer.ok) announce(ctx, 'pov', entity, scene, !isPov);
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
  // the marks are drawn by the CSS: a line for present (it joins the
  // neighbouring scenes), a bullseye on it for the PoV, a ring for a PoV
  // that is not among the scene's characters
  if (present) button.classList.add('present');
  if (pov) { button.classList.add('pov'); button.title += ` (${ctx.t('companion-ui-pov')})`; }
  const mark = markColor(scene, group, entity, ctx);
  if (mark) button.style.setProperty('--mark', mark);
  button.append(el('span', 'sl-mark'));
  if (!editable(scene)) button.disabled = true;
  button.addEventListener('click', (event) => {
    if (event.shiftKey && group.kind === 'character') togglePov(scene, entity, ctx);
    else toggleLink(scene, group, entity, ctx);
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
  if (entity.status) {
    const status = el('span', entity.closed ? 'sl-status closed' : 'sl-status', entity.status);
    status.title = ctx.t('companion-ui-thread-status', entity.status);
    th.append(status);
  }
  if (info.gap > ctx.snapshot.storyline.gapThreshold) {
    const warning = el('span', 'sl-warning', '⚠');
    warning.title = ctx.t('companion-ui-gap-warning', info.gap);
    warning.setAttribute('aria-label', warning.title);
    th.append(warning);
  }
  if (info.orphan) {
    const badge = el('span', 'sl-orphan', ctx.t('companion-ui-no-scene'));
    th.append(badge);
  }
  if (info.unfinished) {
    const badge = el('span', 'sl-orphan sl-unfinished', ctx.t('companion-ui-unfinished'));
    badge.title = ctx.t('companion-ui-unfinished-hint');
    th.append(badge);
  }
  return th;
}

/** The rows of GROUP.  SCENES are all the scenes (for the gaps), VISIBLE the
 * ones drawn as columns. */
function groupRows(group, scenes, visible, ctx) {
  const columns = visible.length;
  const { storyline } = ctx.snapshot;
  const all = (ctx.snapshot.entities[group.kind] || []).filter((entity) => entity.id);
  // the plot points keep the method's order; weights and names would give 1, 10, 11…
  const shown = orderEntities(group, all.filter((entity) => showHidden || !entity.hidden));
  const left = orderEntities(group, all.filter((entity) => !showHidden && entity.hidden));
  const orphans = new Set(storyline.orphans[group.kind] || []);
  const unfinished = new Set(group.kind === 'plot' ? storyline.unfinished || [] : []);
  const rows = [];

  const head = el('tr', 'sl-group');
  const title = el('th');
  // the title stays at the left while the board scrolls sideways
  title.append(el('span', 'sl-group-title', ctx.t(group.title)));
  title.colSpan = columns + 1;
  head.append(title);
  rows.push(head);

  for (const entity of shown) {
    const row = el('tr', 'sl-row');
    row.dataset.id = entity.id;
    const gap = gapOf(group, entity, storyline);
    const gaps = gap !== null && gap > storyline.gapThreshold
      ? gapCells(scenes, entity.id, GAP_FIELDS[group.kind], storyline.gapThreshold) : new Set();
    row.append(rowHeader(entity, group, ctx, {
      gap: gap || 0,
      orphan: orphans.has(entity.id),
      unfinished: unfinished.has(entity.id),
    }));
    for (const scene of visible) row.append(cell(scene, group, entity, { gap: gaps.has(scene) }, ctx));
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

function header(acts, ctx) {
  const chapters = el('tr', 'sl-chapters');
  chapters.append(el('th', 'sl-corner'));
  const numbers = el('tr', 'sl-scenes');
  numbers.append(el('th', 'sl-corner'));
  let number = 0;
  for (const act of acts) {
    for (const chapter of act.chapters) {
      // the number is the scene's place in the book, hidden scenes included
      const numbered = chapter.scenes.map((scene) => { number += 1; return [scene, number]; })
        .filter(([scene]) => visibleScene(scene, ctx));
      if (!numbered.length) continue;
      const th = el('th', 'sl-chapter', chapter.title);
      th.colSpan = numbered.length;
      th.title = chapter.title;
      chapters.append(th);
      numbered.forEach(([scene, sceneNumber], index) => {
        const cellNode = el('th', 'sl-scene');
        if (index === 0) cellNode.classList.add('chapter-start');
        if (scene.id) cellNode.dataset.id = scene.id;
        if (scene.id === ctx.focusId) cellNode.classList.add('focus');
        paint(cellNode, scene, ctx, 'to right');
        const link = el('button', 'sl-open', String(sceneNumber));
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
  // the board scrolls in its own box: keep its place across a re-render
  const old = container.querySelector('.sl-board');
  const keep = old ? { left: old.scrollLeft, top: old.scrollTop } : null;
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
  table.append(header(acts, ctx));
  const body = el('tbody');
  const visible = scenes.filter((scene) => visibleScene(scene, ctx));
  for (const group of visibleGroups(ctx)) {
    for (const row of groupRows(group, scenes, visible, ctx)) body.append(row);
  }
  table.append(body);
  const scroller = el('div', 'sl-scroll sl-board');
  scroller.append(table);
  container.append(scroller);
  stickHeader(table);
  if (keep) { scroller.scrollLeft = keep.left; scroller.scrollTop = keep.top; }
}

/** The scene numbers stick under the chapter row, whose height depends on
 * how its titles wrap: measure it once drawn.  The board's box is as tall as
 * the window under the sticky top bar, so scrolling the page down to it
 * leaves the whole board in view. */
function stickHeader(table) {
  const chapters = table.querySelector('.sl-chapters');
  if (chapters) table.style.setProperty('--sl-chapters-height', `${chapters.getBoundingClientRect().height}px`);
  const topbar = document.querySelector('.topbar');
  const top = topbar ? topbar.getBoundingClientRect().height : 0;
  table.parentElement.style.setProperty('--sl-max-height', `calc(100vh - ${Math.ceil(top) + 16}px)`);
}
