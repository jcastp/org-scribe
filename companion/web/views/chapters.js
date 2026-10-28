// Chapters: the storyline rolled up to chapters.  Chapters across (the act
// above, a number below, the title on hover), entities down in the same
// groups as the storyline.  A cell is the number of the chapter's scenes where
// the entity appears ("(nP)": n of them from that character's PoV), shaded by
// its share of the chapter's scenes; the last column is the total.  The view
// only reads: a cell opens the storyline at the chapter's first scene, a row
// header opens the entity in Emacs.  With a filter on, only the scenes that
// match are counted, and a chapter none of whose scenes match is dimmed.
// All text is put in with textContent.

import { matches, isActive } from '../filters.js';
import { el } from '../edit.js';
import { visibleGroups, ids, orderEntities } from './matrix.js';

let showHidden = false;

/** The act headers' spans: [{ title, count }] over the chapters in order. */
function actSpans(chapters) {
  const spans = [];
  for (const chapter of chapters) {
    const last = spans[spans.length - 1];
    if (last && last.act === chapter.act) last.count += 1;
    else spans.push({ act: chapter.act, title: chapter.actTitle, count: 1 });
  }
  return spans;
}

function appears(scene, group, entity) {
  return ids(scene, group.field).includes(entity.id)
    || (group.kind === 'character' && ids(scene, 'pov').includes(entity.id));
}

function counted(chapter, filtering, ctx) {
  return filtering ? chapter.scenes.filter((scene) => matches(scene, ctx.filters)) : chapter.scenes;
}

function dataCell(chapter, group, entity, filtering, ctx) {
  const td = el('td', 'ch-cell');
  const scenes = counted(chapter, filtering, ctx);
  const here = scenes.filter((scene) => appears(scene, group, entity));
  const pov = group.kind === 'character'
    ? scenes.filter((scene) => ids(scene, 'pov').includes(entity.id)).length : 0;
  if ((filtering || chapter.scenes.length === 0) && scenes.length === 0) td.classList.add('dimmed');
  if (here.length) {
    td.textContent = pov ? `${here.length} (${pov}P)` : String(here.length);
    const share = Math.round(15 + 60 * (here.length / chapter.scenes.length));
    td.style.setProperty('--share', `${share}%`);
    td.classList.add('filled');
    td.tabIndex = 0;
    const open = () => ctx.showInStoryline(chapter.firstSceneId || chapter.id);
    td.addEventListener('click', open);
    td.addEventListener('keydown', (event) => { if (event.key === 'Enter') open(); });
  }
  td.title = ctx.t('companion-ui-chapters-cell', entity.name, chapter.title, here.length, chapter.scenes.length);
  return td;
}

function rowHeader(entity, ctx) {
  const th = el('th', 'sl-name');
  th.scope = 'row';
  const open = el('button', 'sl-open', entity.name);
  open.type = 'button';
  open.title = ctx.t('companion-ui-open');
  open.addEventListener('click', () => ctx.goto(entity.id));
  th.append(open);
  if (entity.hidden) th.classList.add('hidden-entity');
  return th;
}

function groupRows(group, chapters, filtering, ctx) {
  const all = (ctx.snapshot.entities[group.kind] || []).filter((entity) => entity.id);
  const shown = orderEntities(group, all.filter((entity) => showHidden || !entity.hidden));
  const left = orderEntities(group, all.filter((entity) => !showHidden && entity.hidden));
  const rows = [];

  const head = el('tr', 'sl-group');
  const title = el('th', '', ctx.t(group.title));
  title.colSpan = chapters.length + 2;
  head.append(title);
  rows.push(head);

  for (const entity of shown) {
    const row = el('tr', 'sl-row');
    row.dataset.id = entity.id;
    row.append(rowHeader(entity, ctx));
    let total = 0;
    for (const chapter of chapters) {
      row.append(dataCell(chapter, group, entity, filtering, ctx));
      total += counted(chapter, filtering, ctx).filter((scene) => appears(scene, group, entity)).length;
    }
    row.append(el('td', 'ch-total', total ? String(total) : ''));
    rows.push(row);
  }
  if (left.length) {
    const note = el('tr', 'sl-hidden-note');
    const text = el('td', '', ctx.t('companion-ui-not-shown', left.map((entity) => entity.name).join(', ')));
    text.colSpan = chapters.length + 2;
    note.append(text);
    rows.push(note);
  }
  return rows;
}

function header(chapters, ctx) {
  const acts = el('tr', 'sl-chapters');
  acts.append(el('th', 'sl-corner'));
  for (const span of actSpans(chapters)) {
    const th = el('th', 'sl-chapter', span.title || '');
    th.colSpan = span.count;
    acts.append(th);
  }
  acts.append(el('th', 'sl-chapter', ''));
  const numbers = el('tr', 'sl-scenes');
  numbers.append(el('th', 'sl-corner'));
  chapters.forEach((chapter, index) => {
    const th = el('th', 'sl-scene chapter-start', String(index + 1));
    th.title = chapter.scenes.length ? chapter.title
      : ctx.t('companion-ui-chapters-empty', chapter.title);
    if (!chapter.scenes.length) th.classList.add('dimmed');
    numbers.append(th);
  });
  numbers.append(el('th', 'sl-scene chapter-start', ctx.t('companion-ui-chapters-total')));
  const head = el('thead');
  head.append(acts, numbers);
  return head;
}

export function render(container, ctx) {
  container.replaceChildren();
  const chapters = [];
  for (const act of ctx.snapshot.acts) {
    for (const chapter of act.chapters) {
      // an empty chapter keeps its column, so a number is always the chapter's
      // place in the book
      const first = chapter.scenes.find((scene) => scene.id);
      chapters.push({
        ...chapter, act: act.id || act.title, actTitle: act.title,
        firstSceneId: first ? first.id : null,
      });
    }
  }
  if (!chapters.length) {
    container.append(el('p', 'empty', ctx.t('companion-ui-empty')));
    return;
  }
  const filtering = isActive(ctx.filters);

  const tools = el('div', 'table-tools');
  const label = el('label', 'switch');
  const box = el('input');
  box.type = 'checkbox';
  box.checked = showHidden;
  box.addEventListener('change', () => { showHidden = box.checked; ctx.rerender(); });
  label.append(box, el('span', '', ctx.t('companion-ui-show-hidden')));
  tools.append(label, el('span', 'legend', ctx.t('companion-ui-chapters-legend')));
  container.append(tools);

  const table = el('table', 'storyline chapters');
  table.append(header(chapters, ctx));
  const body = el('tbody');
  for (const group of visibleGroups(ctx)) {
    for (const row of groupRows(group, chapters, filtering, ctx)) body.append(row);
  }
  table.append(body);
  const scroller = el('div', 'sl-scroll');
  scroller.append(table);
  container.append(scroller);
}
