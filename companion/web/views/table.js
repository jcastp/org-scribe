// Outline table: one row per scene, grouped by act and chapter (collapsible).
// Columns: a selection box, title, state, the scene fields you choose, words,
// objective.  Cells edit in place; link fields open a picker over the entities
// of that kind; rows drag between chapters.  The act and chapter rows add a
// chapter or a scene at the end, and carry their word total and how far it is
// from their objective; a scene row's own button adds one right after it.
// Sorting happens here, on the client, and is applied inside each chapter.
// Selected rows change together (one `batch' command, one undo step); the
// find box narrows the rows like a filter, over titles and text fields.
// All text is put in with textContent.

import { todoColor } from '../colors.js';
import { paint } from '../scheme.js';
import { matches, isActive } from '../filters.js';
import {
  el, startEdit, moveTo, sceneParent, addSceneButton, addChapterButton, detailButton, isDetailKey,
} from '../edit.js';
import { hasPlotPoints } from './matrix.js';
import { openPicker } from '../picker.js';

let STORE = 'org-scribe.table';
const DEFAULT_COLUMNS = ['pov', 'characters', 'plot-point', 'summary'];

function load(key, fallback) {
  try {
    const raw = localStorage.getItem(`${STORE}.${key}`);
    return raw === null ? fallback : JSON.parse(raw);
  } catch (error) {
    return fallback;
  }
}

function save(key, value) {
  try { localStorage.setItem(`${STORE}.${key}`, JSON.stringify(value)); } catch (error) { /* private mode */ }
}

const prefs = {
  columns: load('columns', DEFAULT_COLUMNS),
  sort: load('sort', null),          // { key, dir } | null
};

/** Keep the preferences per project: the origin is shared by all of them. */
export function useProject(root) {
  STORE = `org-scribe.table:${root}`;
  prefs.columns = load('columns', DEFAULT_COLUMNS);
  prefs.sort = load('sort', null);
  selected.clear();
  lastTicked = null;
  find = '';
}

// ---- which rows show -------------------------------------------------------

/** True when the find box's text is in the scene's title or a text field. */
function found(scene, ctx) {
  if (!find) return true;
  const needle = find.toLocaleLowerCase();
  if ((scene.title || '').toLocaleLowerCase().includes(needle)) return true;
  return ctx.snapshot.sceneFields.some((field) => {
    const value = field.kind === 'text' && scene.fields[field.key];
    return Boolean(value && value.raw && value.raw.toLocaleLowerCase().includes(needle));
  });
}

/** True while the filters or the find box narrow the rows. */
function narrowing(ctx) {
  return isActive(ctx.filters) || find !== '';
}

/** True when SCENE passes the filters and the find box. */
function shown(scene, ctx) {
  return (!isActive(ctx.filters) || matches(scene, ctx.filters)) && found(scene, ctx);
}

/** True when "Hide the rest" removes the whole group of SCENES. */
function groupHidden(scenes, ctx) {
  return Boolean(ctx.hideSwitch) && narrowing(ctx) && !scenes.some((scene) => shown(scene, ctx));
}

function editable(scene) {
  return Boolean(scene.id) && !scene.readOnly;
}

const collapsed = new Set();          // IDs of collapsed acts and chapters
const selected = new Set();           // IDs of the scenes ticked for a bulk edit
let lastTicked = null;                // the row a Shift+click range starts from
let find = '';                        // the find box's text
let findHasFocus = false;             // give the box its focus back after a redraw
const bulk = { field: '', entity: '' };   // the bulk bar's link choice

// ---- cell content ----------------------------------------------------------

function fieldNames(scene, key) {
  const value = scene.fields[key];
  if (!value) return [];
  const names = value.links ? value.links.map((link) => link.name) : [];
  return names.concat(value.unlinked || []);
}

function sortValue(scene, key) {
  if (key === 'title') return scene.title.toLowerCase();
  if (key === 'todo') return scene.todo || '';
  if (key === 'words') return scene.words;
  if (key === 'objective') return scene.objective || 0;
  // link fields sort by the names shown, never by the IDs inside the raw text
  const value = scene.fields[key];
  if (!value) return '';
  return (value.links ? fieldNames(scene, key).join(', ') : value.raw || '').toLowerCase();
}

function sorted(scenes) {
  if (!prefs.sort) return scenes;
  const { key, dir } = prefs.sort;
  const factor = dir === 'desc' ? -1 : 1;
  return [...scenes].sort((a, b) => {
    const x = sortValue(a, key);
    const y = sortValue(b, key);
    if (x === y) return 0;
    return (x < y ? -1 : 1) * factor;
  });
}

// ---- cells -----------------------------------------------------------------

function fieldCell(scene, field, ctx) {
  const cell = el('td', `cell field-${field.key}`);
  const can = Boolean(scene.id) && !scene.readOnly;
  const isText = field.kind === 'text';
  const names = fieldNames(scene, field.key);
  const content = el('span', 'cell-content');
  if (isText) {
    const value = scene.fields[field.key];
    content.textContent = value ? value.raw : '';
  } else {
    const value = scene.fields[field.key] || {};
    for (const link of value.links || []) content.append(el('span', 'chip-link', link.name));
    for (const name of value.unlinked || []) content.append(el('span', 'chip-link unlinked', name));
  }
  cell.append(content);
  if (!can) return cell;
  cell.tabIndex = 0;
  cell.classList.add('editable');
  const edit = () => {
    if (isText) {
      const current = scene.fields[field.key];
      startEdit(content, current ? current.raw : '', field.key === 'summary',
        (value) => ctx.edit('set-field', { id: scene.id, key: field.key, value }, scene.id), ctx);
    } else {
      openPicker(cell, scene, field, ctx);
    }
  };
  cell.addEventListener('click', (event) => {
    if (event.target.closest('.card-edit, .picker')) return;
    edit();
  });
  cell.addEventListener('keydown', (event) => {
    if (event.target === cell && event.key === 'Enter') { event.preventDefault(); edit(); }
  });
  cell.title = names.length ? names.join(', ') : '';
  return cell;
}

function stateCell(scene, ctx) {
  const cell = el('td', 'cell state');
  const select = el('select', 'state-select');
  select.setAttribute('aria-label', ctx.t('companion-ui-scene-state'));
  const none = el('option', '', ctx.t('companion-ui-no-todo'));
  none.value = '';
  select.append(none);
  for (const keyword of ctx.snapshot.todo.keywords) {
    const option = el('option', '', keyword);
    option.value = keyword;
    select.append(option);
  }
  select.value = scene.todo === null ? '' : scene.todo;
  select.disabled = !scene.id || scene.readOnly;
  select.addEventListener('change', () => ctx.edit('set-todo', { id: scene.id, state: select.value }, scene.id));
  cell.append(select);
  return cell;
}

/** A button adding a scene right after SCENE, under the container PARENT_ID. */
function insertAfterButton(scene, parentId, ctx) {
  const add = el('button', 'insert-after', '+');
  add.type = 'button';
  const label = ctx.t('companion-ui-new-scene-after');
  add.title = label;
  add.setAttribute('aria-label', label);
  add.addEventListener('click', (event) => {
    event.stopPropagation();
    ctx.edit('create-scene', { parent: parentId, after: scene.id }, null, true);
  });
  return add;
}

function titleCell(scene, parentId, ctx) {
  const cell = el('td', 'cell title');
  const handle = el('span', 'drag-handle', '⠿');
  handle.setAttribute('aria-hidden', 'true');
  const text = el('span', 'title-text', scene.title || ctx.t('companion-ui-untitled'));
  cell.append(handle, text);
  if (scene.id && !scene.readOnly) {
    text.classList.add('editable');
    text.title = ctx.t('companion-ui-edit-title');
    // focusable, so a new scene can take the focus and be renamed from the keyboard
    text.tabIndex = 0;
    const editTitle = () => startEdit(text, scene.title || '', false, (value) => {
      if (value.trim()) ctx.edit('set-title', { id: scene.id, title: value }, scene.id);
    }, ctx);
    text.addEventListener('click', (event) => { event.stopPropagation(); editTitle(); });
    text.addEventListener('keydown', (event) => {
      if (event.key === 'Enter' || event.key === 'F2') { event.preventDefault(); editTitle(); }
      if (isDetailKey(event)) { event.preventDefault(); ctx.openDetail(scene.id); }
    });
  }
  if (scene.id) {
    const open = el('button', 'open', '↗');
    open.type = 'button';
    const label = ctx.t('companion-ui-open');
    open.title = label;
    open.setAttribute('aria-label', label);
    open.addEventListener('click', (event) => { event.stopPropagation(); ctx.goto(scene.id); });
    cell.append(detailButton(scene, ctx), open);
    // "after" is the manuscript's order: under a sort the row below is not the next scene
    if (parentId && !scene.readOnly && !prefs.sort) cell.append(insertAfterButton(scene, parentId, ctx));
  } else {
    const warning = el('span', 'noid', '⚠');
    warning.title = ctx.t('companion-ui-no-id');
    cell.append(warning);
  }
  return cell;
}

function numberCell(value, className, language) {
  const cell = el('td', `cell number ${className}`);
  cell.textContent = value ? value.toLocaleString(language) : '';
  return cell;
}

/** The box that ticks a scene for a bulk edit.  Shift+click ticks (or
 * unticks) every row between it and the one ticked before. */
function selectCell(scene, ctx) {
  const cell = el('td', 'cell select');
  const box = el('input');
  box.type = 'checkbox';
  box.setAttribute('aria-label', `${ctx.t('companion-ui-select-scene')}: ${scene.title}`);
  box.checked = selected.has(scene.id);
  box.disabled = !editable(scene);
  box.addEventListener('click', (event) => {
    const rows = [...document.querySelectorAll('.outline .scene-row')];
    const here = rows.findIndex((row) => row.dataset.id === scene.id);
    const there = rows.findIndex((row) => row.dataset.id === lastTicked);
    const ids = event.shiftKey && there >= 0
      ? rows.slice(Math.min(here, there), Math.max(here, there) + 1).map((row) => row.dataset.id)
      : [scene.id];
    const all = ctx.snapshot.acts.flatMap((act) => act.chapters.flatMap((chapter) => chapter.scenes));
    for (const id of ids) {
      const target = all.find((candidate) => candidate.id === id);
      if (!target || !editable(target)) continue;
      if (box.checked) selected.add(id); else selected.delete(id);
    }
    lastTicked = scene.id;
    ctx.rerender();
  });
  cell.append(box);
  return cell;
}

/** PARENT_ID: the container new scenes go into, or null when there is none. */
function sceneRow(scene, columns, parentId, ctx) {
  const row = el('tr', 'scene-row');
  if (scene.id) row.dataset.id = scene.id;
  if (scene.id && !scene.readOnly) row.classList.add('movable');
  if (scene.readOnly) row.classList.add('readonly');
  if (scene.id === ctx.focusId) row.classList.add('focus');
  if (scene.id && scene.id === ctx.detailId) row.classList.add('in-detail');
  if (narrowing(ctx) && !shown(scene, ctx)) row.classList.add('dimmed');
  if (selected.has(scene.id)) row.classList.add('selected');
  // the left edge is always the TODO state; the colour-by choice is the
  // stripe beside it and the tint of the row (scheme.js)
  const color = todoColor(ctx.snapshot, scene.todo);
  if (color) row.style.setProperty('--row', color);
  paint(row, scene, ctx);
  row.append(selectCell(scene, ctx), titleCell(scene, parentId, ctx), stateCell(scene, ctx));
  for (const field of columns) row.append(fieldCell(scene, field, ctx));
  row.append(numberCell(scene.words, 'words', ctx.snapshot.language), numberCell(scene.objective, 'objective', ctx.snapshot.language));
  return row;
}

// ---- table -----------------------------------------------------------------

function toggle(id, rerender) {
  if (collapsed.has(id)) collapsed.delete(id); else collapsed.add(id);
  rerender();
}

/** The objective cell of a group row: the objective, and how far the words
 * are from it (negative: still to write). */
function objectiveCell(words, objective, language, ctx) {
  const cell = el('td', 'cell number objective');
  if (!objective) return cell;
  cell.append(objective.toLocaleString(language));
  const delta = words - objective;
  const mark = el('span', delta >= 0 ? 'delta reached' : 'delta short',
                  ` ${delta.toLocaleString(language, { signDisplay: 'always' })}`);
  mark.title = ctx.t('companion-ui-objective-delta');
  cell.append(mark);
  return cell;
}

/** EXTRA, when given, is a button placed at the row's end.  SCENES are the
 * group's scenes, for its word total. */
function groupRow(node, key, className, span, ctx, extra, scenes) {
  const row = el('tr', className);
  if (node.id) row.dataset.id = node.id;
  const cell = el('td');
  cell.colSpan = span - 2;
  const open = !collapsed.has(key);
  const button = el('button', 'twisty', open ? '▾' : '▸');
  button.type = 'button';
  button.setAttribute('aria-expanded', String(open));
  button.setAttribute('aria-label', ctx.t('companion-ui-collapse'));
  button.addEventListener('click', () => toggle(key, ctx.rerender));
  const head = el('div', 'group-head');
  head.append(button, el('span', 'group-title', node.title || ''));
  if (extra) head.append(extra);
  cell.append(head);
  const { language } = ctx.snapshot;
  const words = scenes.reduce((total, scene) => total + scene.words, 0);
  row.append(cell, numberCell(words, 'words', language),
             objectiveCell(words, node.objective, language, ctx));
  return row;
}

function makeSortable(body, ctx) {
  window.Sortable.create(body, {
    group: 'rows',
    draggable: '.scene-row.movable',
    handle: '.drag-handle',
    animation: 150,
    ghostClass: 'drag-ghost',
    onMove(event) {
      // never above the chapter's own heading row
      return !(event.related.classList.contains('chapter-row') && !event.willInsertAfter);
    },
    onEnd(event) {
      if (event.from === event.to && event.oldIndex === event.newIndex) return;
      const before = event.item.previousElementSibling;
      const after = before.classList.contains('chapter-row') ? '' : before.dataset.id || null;
      moveTo(ctx, event.item.dataset.id, event.to.dataset.id, after);
    },
  });
}

/** The scenes the select-all box ticks: editable, shown, not folded away. */
function selectable(ctx) {
  const ids = [];
  ctx.snapshot.acts.forEach((act, actIndex) => {
    const actKey = act.id || `act#${actIndex}`;
    if (collapsed.has(actKey)) return;
    act.chapters.forEach((chapter, chapterIndex) => {
      if (collapsed.has(chapter.id || `${actKey}/chapter#${chapterIndex}`)) return;
      for (const scene of chapter.scenes) {
        if (editable(scene) && shown(scene, ctx)) ids.push(scene.id);
      }
    });
  });
  return ids;
}

function selectAllCell(ctx) {
  const th = el('th', 'select');
  th.scope = 'col';
  const box = el('input');
  box.type = 'checkbox';
  box.setAttribute('aria-label', ctx.t('companion-ui-select-all'));
  box.title = ctx.t('companion-ui-select-all');
  const ids = selectable(ctx);
  const ticked = ids.filter((id) => selected.has(id)).length;
  box.checked = ids.length > 0 && ticked === ids.length;
  box.indeterminate = ticked > 0 && ticked < ids.length;
  box.disabled = ids.length === 0;
  box.addEventListener('change', () => {
    for (const id of ids) { if (box.checked) selected.add(id); else selected.delete(id); }
    ctx.rerender();
  });
  th.append(box);
  return th;
}

function header(columns, ctx) {
  const head = el('thead');
  const row = el('tr');
  row.append(selectAllCell(ctx));
  const cells = [
    { key: 'title', label: ctx.t('companion-ui-column-title') },
    { key: 'todo', label: ctx.t('companion-ui-filter-todo') },
    ...columns.map((field) => ({ key: field.key, label: field.name })),
    { key: 'words', label: ctx.t('companion-ui-column-words') },
    { key: 'objective', label: ctx.t('companion-ui-column-objective') },
  ];
  for (const { key, label } of cells) {
    const th = el('th');
    th.scope = 'col';
    const button = el('button', 'sort', label);
    button.type = 'button';
    const active = prefs.sort && prefs.sort.key === key;
    if (active) {
      button.append(el('span', 'arrow', prefs.sort.dir === 'asc' ? ' ▲' : ' ▼'));
      th.setAttribute('aria-sort', prefs.sort.dir === 'asc' ? 'ascending' : 'descending');
    }
    button.addEventListener('click', () => {
      if (!active) prefs.sort = { key, dir: 'asc' };
      else if (prefs.sort.dir === 'asc') prefs.sort = { key, dir: 'desc' };
      else prefs.sort = null;
      save('sort', prefs.sort);
      ctx.rerender();
    });
    th.append(button);
    row.append(th);
  }
  head.append(row);
  return head;
}

function toolbar(fields, ctx) {
  const bar = el('div', 'table-tools');
  const chooser = el('details', 'columns');
  chooser.append(el('summary', '', ctx.t('companion-ui-columns')));
  const choices = el('div', 'columns-list');
  for (const field of fields) {
    const label = el('label');
    const box = el('input');
    box.type = 'checkbox';
    box.checked = prefs.columns.includes(field.key);
    box.addEventListener('change', () => {
      prefs.columns = fields.map((f) => f.key)
        .filter((key) => (key === field.key ? box.checked : prefs.columns.includes(key)));
      save('columns', prefs.columns);
      ctx.rerender();
    });
    label.append(box, el('span', '', field.name));
    choices.append(label);
  }
  chooser.append(choices);
  chooser.open = toolbar.open === true;
  chooser.addEventListener('toggle', () => { toolbar.open = chooser.open; });

  const search = el('input', 'table-find');
  search.type = 'search';
  search.placeholder = ctx.t('companion-ui-find');
  search.setAttribute('aria-label', ctx.t('companion-ui-find'));
  search.value = find;
  search.addEventListener('input', () => {
    find = search.value.trim();
    findHasFocus = true;
    ctx.rerender();
  });
  search.addEventListener('blur', () => { findHasFocus = false; });
  bar.append(chooser, search);
  if (selected.size) bar.append(bulkBar(ctx));
  return bar;
}

// ---- bulk edits --------------------------------------------------------------

/** The selected scenes, as scene objects (the IDs that still name one). */
function selectedScenes(ctx) {
  const all = ctx.snapshot.acts.flatMap((act) => act.chapters.flatMap((chapter) => chapter.scenes));
  return all.filter((scene) => selected.has(scene.id) && editable(scene));
}

/** Send OPS as one batch; tell how many scenes changed. */
async function runBatch(ops, ctx) {
  if (!ops.length) { ctx.notice(ctx.t('companion-ui-bulk-nothing')); return; }
  const answer = await ctx.edit('batch', { ops });
  if (answer.ok) ctx.notice(ctx.t('companion-ui-bulk-done', ops.length));
}

/** The set-field operations that add ENTITY to (or remove it from) FIELD in
 * SCENES; scenes that would not change are left out. */
function linkOps(scenes, field, entity, add) {
  const ops = [];
  for (const scene of scenes) {
    const value = scene.fields[field.key] || {};
    const ids = (value.links || []).map((link) => link.id);
    let next;
    if (add) next = field.kind === 'link-one' ? [entity] : (ids.includes(entity) ? ids : [...ids, entity]);
    else next = ids.filter((id) => id !== entity);
    if (next.join() === ids.join()) continue;
    ops.push({ op: 'set-field', args: { id: scene.id, key: field.key, links: next, unlinked: value.unlinked || [] } });
  }
  return ops;
}

function bulkBar(ctx) {
  const scenes = selectedScenes(ctx);
  const bar = el('div', 'bulk-bar');
  bar.setAttribute('role', 'group');
  bar.append(el('span', 'bulk-count', ctx.t('companion-ui-bulk-selected', scenes.length)));

  const state = el('select');
  state.setAttribute('aria-label', ctx.t('companion-ui-bulk-state'));
  const prompt = el('option', '', ctx.t('companion-ui-bulk-state'));
  prompt.value = '';
  prompt.disabled = true;
  state.append(prompt);
  for (const [value, text] of ctx.snapshot.todo.keywords.map((keyword) => [keyword, keyword])
    .concat([['-', ctx.t('companion-ui-no-todo')]])) {
    const option = el('option', '', text);
    option.value = value;
    state.append(option);
  }
  state.value = '';
  state.addEventListener('change', () => {
    const wanted = state.value === '-' ? '' : state.value;
    runBatch(scenes.filter((scene) => (scene.todo || '') !== wanted)
      .map((scene) => ({ op: 'set-todo', args: { id: scene.id, state: wanted } })), ctx);
  });
  bar.append(state);

  const links = ctx.snapshot.sceneFields
    .filter((field) => field.kind !== 'text')
    .filter((field) => field.key !== 'plot-point' || hasPlotPoints(ctx));
  if (!links.some((field) => field.key === bulk.field)) bulk.field = links.length ? links[0].key : '';
  const field = links.find((candidate) => candidate.key === bulk.field);
  const fieldSelect = el('select');
  fieldSelect.setAttribute('aria-label', ctx.t('companion-ui-bulk-field'));
  for (const candidate of links) {
    const option = el('option', '', candidate.name);
    option.value = candidate.key;
    fieldSelect.append(option);
  }
  fieldSelect.value = bulk.field;
  fieldSelect.addEventListener('change', () => { bulk.field = fieldSelect.value; bulk.entity = ''; ctx.rerender(); });
  const entities = field ? (ctx.snapshot.entities[field.entity] || []).filter((entity) => entity.id) : [];
  if (!entities.some((entity) => entity.id === bulk.entity)) bulk.entity = entities.length ? entities[0].id : '';
  const entitySelect = el('select');
  entitySelect.setAttribute('aria-label', ctx.t('companion-ui-bulk-entity'));
  for (const entity of entities) {
    const option = el('option', entity.hidden ? 'hidden-entity' : '', entity.name);
    option.value = entity.id;
    entitySelect.append(option);
  }
  entitySelect.value = bulk.entity;
  entitySelect.addEventListener('change', () => { bulk.entity = entitySelect.value; });
  const addButton = el('button', '', ctx.t('companion-ui-bulk-add'));
  addButton.type = 'button';
  addButton.disabled = !field || !bulk.entity;
  addButton.addEventListener('click', () => runBatch(linkOps(scenes, field, bulk.entity, true), ctx));
  const removeButton = el('button', '', ctx.t('companion-ui-bulk-remove'));
  removeButton.type = 'button';
  removeButton.disabled = !field || !bulk.entity;
  removeButton.addEventListener('click', () => runBatch(linkOps(scenes, field, bulk.entity, false), ctx));
  bar.append(fieldSelect, entitySelect, addButton, removeButton);

  const clear = el('button', '', ctx.t('companion-ui-bulk-clear'));
  clear.type = 'button';
  clear.addEventListener('click', () => { selected.clear(); ctx.rerender(); });
  bar.append(clear);
  return bar;
}

export function render(container, ctx) {
  // taken before the old box leaves the page (its blur clears the flag)
  const keepFind = findHasFocus;
  container.replaceChildren();
  const fields = ctx.snapshot.sceneFields
    .filter((field) => field.key !== 'weight')
    .filter((field) => field.key !== 'plot-point' || hasPlotPoints(ctx));
  const columns = fields.filter((field) => prefs.columns.includes(field.key));
  const acts = ctx.snapshot.acts;
  if (acts.length === 0) {
    container.append(el('p', 'empty', ctx.t('companion-ui-empty')));
    return;
  }
  // a scene deleted or made read-only since cannot stay ticked
  const live = new Set(acts.flatMap((act) => act.chapters.flatMap((chapter) => chapter.scenes))
    .filter(editable).map((scene) => scene.id));
  for (const id of [...selected]) if (!live.has(id)) selected.delete(id);
  container.append(toolbar(fields, ctx));
  const span = 5 + columns.length;
  const table = el('table', 'outline');
  // the find box narrows like a filter, so "Hide the rest" applies to it too
  table.classList.toggle('hide-unmatched', Boolean(ctx.hideSwitch) && narrowing(ctx));
  table.append(header(columns, ctx));
  acts.forEach((act, actIndex) => {
    // an implicit act or chapter has no ID: its place stands in for one
    const actKey = act.id || `act#${actIndex}`;
    const actBody = el('tbody', 'act-body');
    const actScenes = act.chapters.flatMap((chapter) => chapter.scenes);
    if (groupHidden(actScenes, ctx)) actBody.classList.add('filtered-out');
    // a new row must not land in a collapsed group, out of sight
    const expand = (...keys) => () => { for (const key of keys) collapsed.delete(key); };
    const passthroughIndex = act.chapters.findIndex((chapter) => chapter.passthrough);
    let actButton = null;
    if (passthroughIndex >= 0) {
      // no chapters here: the act's own button adds a scene
      const passthrough = act.chapters[passthroughIndex];
      const parent = sceneParent(passthrough);
      if (parent) {
        actButton = addSceneButton(parent, passthrough.scenes, ctx,
                                   expand(actKey, `${actKey}/chapter#${passthroughIndex}`));
      }
    } else if (act.id && !act.readOnly) {
      actButton = addChapterButton(act, ctx, expand(actKey));
    }
    actBody.append(groupRow(act, actKey, 'act-row', span, ctx, actButton, actScenes));
    table.append(actBody);
    if (collapsed.has(actKey)) return;
    act.chapters.forEach((chapter, chapterIndex) => {
      const chapterKey = chapter.id || `${actKey}/chapter#${chapterIndex}`;
      const body = el('tbody', 'chapter-body');
      if (groupHidden(chapter.scenes, ctx)) body.classList.add('filtered-out');
      const parent = sceneParent(chapter);
      if (parent) body.dataset.id = parent;
      // a pass-through chapter keeps its (hidden) row, so the drag rules
      // that look for the row above the first scene still hold
      const canAdd = parent && (chapter.passthrough || !chapter.readOnly);
      const chapterButton = canAdd && !chapter.passthrough
        ? addSceneButton(parent, chapter.scenes, ctx, expand(chapterKey))
        : null;
      body.append(groupRow(chapter, chapterKey,
                           chapter.passthrough ? 'chapter-row passthrough' : 'chapter-row', span, ctx,
                           chapterButton, chapter.scenes));
      if (!collapsed.has(chapterKey)) {
        for (const scene of sorted(chapter.scenes)) {
          body.append(sceneRow(scene, columns, canAdd ? parent : null, ctx));
        }
      }
      table.append(body);
      if (canAdd && !prefs.sort && window.Sortable) makeSortable(body, ctx);
    });
  });
  container.append(table);
  if (keepFind) {
    findHasFocus = true;
    const box = container.querySelector('.table-find');
    box.focus();
    box.setSelectionRange(box.value.length, box.value.length);
  }
}
