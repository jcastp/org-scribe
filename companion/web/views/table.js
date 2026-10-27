// Outline table: one row per scene, grouped by act and chapter (collapsible).
// Columns: title, state, the scene fields you choose, words, objective.
// Cells edit in place; link fields open a picker over the entities of that
// kind; rows drag between chapters.  Sorting happens here, on the client, and
// is applied inside each chapter.  All text is put in with textContent.

import { matches, isActive } from '../filters.js';
import { el, startEdit, moveTo } from '../edit.js';

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
  colour: load('colour', 'todo'),
  sort: load('sort', null),          // { key, dir } | null
};

/** Keep the preferences per project: the origin is shared by all of them. */
export function useProject(root) {
  STORE = `org-scribe.table:${root}`;
  prefs.columns = load('columns', DEFAULT_COLUMNS);
  prefs.colour = load('colour', 'todo');
  prefs.sort = load('sort', null);
}

const collapsed = new Set();          // IDs of collapsed acts and chapters

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
  const value = scene.fields[key];
  return value ? (value.raw || '').toLowerCase() : '';
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

function povHue(scene) {
  const pov = scene.fields.pov;
  if (!pov || !pov.links || !pov.links.length) return null;
  let hash = 0;
  for (const char of pov.links[0].id) hash = (hash * 31 + char.charCodeAt(0)) % 360;
  return hash;
}

// ---- link picker -----------------------------------------------------------

function openPicker(cell, scene, field, ctx) {
  const entities = (ctx.snapshot.entities[field.entity] || []).filter((entity) => entity.id);
  const current = scene.fields[field.key] || {};
  const chosen = new Set((current.links || []).map((link) => link.id));
  const single = field.kind === 'link-one';

  const panel = el('div', 'picker');
  panel.setAttribute('role', 'dialog');
  panel.setAttribute('aria-label', field.name);
  const search = el('input', 'picker-search');
  search.type = 'search';
  search.placeholder = ctx.t('companion-ui-picker-search');
  const list = el('div', 'picker-list');
  const rows = entities.map((entity) => {
    const label = el('label', 'picker-item');
    const box = el('input');
    box.type = single ? 'radio' : 'checkbox';
    box.name = 'picker';
    box.checked = chosen.has(entity.id);
    box.addEventListener('change', () => {
      if (single) chosen.clear();
      if (box.checked) chosen.add(entity.id); else chosen.delete(entity.id);
    });
    label.append(box, el('span', entity.hidden ? 'hidden-entity' : '', entity.name));
    list.append(label);
    return { label, name: entity.name.toLowerCase() };
  });
  search.addEventListener('input', () => {
    const needle = search.value.toLowerCase();
    for (const row of rows) row.label.hidden = !row.name.includes(needle);
  });
  const actions = el('div', 'picker-actions');
  const clear = el('button', '', ctx.t('companion-ui-picker-clear'));
  clear.type = 'button';
  clear.addEventListener('click', () => {
    chosen.clear();
    for (const input of list.querySelectorAll('input')) input.checked = false;
  });
  const done = el('button', 'primary', ctx.t('companion-ui-picker-done'));
  done.type = 'button';
  actions.append(clear, done);
  panel.append(search, list, actions);

  let closed = false;
  const close = (apply) => {
    if (closed) return;
    closed = true;
    document.removeEventListener('mousedown', outside, true);
    panel.remove();
    ctx.editEnded();
    if (!apply) return;
    // the writer's own order stays; ids added now follow, in entity order
    // (ids not in the list, hidden by a filter or dangling, stay where they were)
    const before = (current.links || []).map((link) => link.id);
    const ids = before.filter((id) => chosen.has(id));
    for (const entity of entities) if (chosen.has(entity.id) && !ids.includes(entity.id)) ids.push(entity.id);
    if (ids.join() === before.join()) return;
    ctx.edit('set-field', {
      id: scene.id, key: field.key, links: ids, unlinked: current.unlinked || [],
    }, scene.id);
  };
  const outside = (event) => { if (!panel.contains(event.target)) close(true); };
  done.addEventListener('click', () => close(true));
  panel.addEventListener('keydown', (event) => {
    if (event.key === 'Escape') { event.preventDefault(); close(false); cell.focus(); }
  });

  ctx.editStarted();
  document.body.append(panel);
  const box = cell.getBoundingClientRect();
  panel.style.top = `${Math.min(box.bottom + window.scrollY, window.scrollY + window.innerHeight - 320)}px`;
  panel.style.left = `${Math.max(8, Math.min(box.left + window.scrollX, window.innerWidth - 300))}px`;
  document.addEventListener('mousedown', outside, true);
  search.focus();
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

function titleCell(scene, ctx) {
  const cell = el('td', 'cell title');
  const handle = el('span', 'drag-handle', '⠿');
  handle.setAttribute('aria-hidden', 'true');
  const text = el('span', 'title-text', scene.title || ctx.t('companion-ui-untitled'));
  cell.append(handle, text);
  if (scene.id && !scene.readOnly) {
    text.classList.add('editable');
    text.title = ctx.t('companion-ui-edit-title');
    text.addEventListener('click', (event) => {
      event.stopPropagation();
      startEdit(text, scene.title || '', false, (value) => {
        if (value.trim()) ctx.edit('set-title', { id: scene.id, title: value }, scene.id);
      }, ctx);
    });
  }
  if (scene.id) {
    const open = el('button', 'open', '↗');
    open.type = 'button';
    const label = ctx.t('companion-ui-open');
    open.title = label;
    open.setAttribute('aria-label', label);
    open.addEventListener('click', (event) => { event.stopPropagation(); ctx.goto(scene.id); });
    cell.append(open);
  } else {
    const warning = el('span', 'noid', '⚠');
    warning.title = ctx.t('companion-ui-no-id');
    cell.append(warning);
  }
  return cell;
}

function numberCell(value, className) {
  const cell = el('td', `cell number ${className}`);
  cell.textContent = value ? value.toLocaleString() : '';
  return cell;
}

function sceneRow(scene, columns, ctx) {
  const row = el('tr', 'scene-row');
  if (scene.id) row.dataset.id = scene.id;
  if (scene.id && !scene.readOnly) row.classList.add('movable');
  if (scene.readOnly) row.classList.add('readonly');
  if (scene.id === ctx.focusId) row.classList.add('focus');
  if (isActive(ctx.filters) && !matches(scene, ctx.filters)) row.classList.add('dimmed');
  if (prefs.colour === 'pov') {
    const hue = povHue(scene);
    if (hue !== null) row.style.setProperty('--row', `hsl(${hue} 55% 50%)`);
  } else {
    const color = scene.todo !== null && ctx.snapshot.todo.colors[scene.todo];
    if (color) row.style.setProperty('--row', color);
  }
  row.append(titleCell(scene, ctx), stateCell(scene, ctx));
  for (const field of columns) row.append(fieldCell(scene, field, ctx));
  row.append(numberCell(scene.words, 'words'), numberCell(scene.objective, 'objective'));
  return row;
}

// ---- table -----------------------------------------------------------------

function toggle(id, rerender) {
  if (collapsed.has(id)) collapsed.delete(id); else collapsed.add(id);
  rerender();
}

function groupRow(node, className, span, ctx) {
  const row = el('tr', className);
  if (node.id) row.dataset.id = node.id;
  const cell = el('td');
  cell.colSpan = span;
  const open = !collapsed.has(node.id);
  const button = el('button', 'twisty', open ? '▾' : '▸');
  button.type = 'button';
  button.setAttribute('aria-expanded', String(open));
  button.setAttribute('aria-label', ctx.t('companion-ui-collapse'));
  button.addEventListener('click', () => toggle(node.id, ctx.rerender));
  cell.append(button, el('span', 'group-title', node.title || ''));
  row.append(cell);
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

function header(columns, ctx) {
  const head = el('thead');
  const row = el('tr');
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

  const colour = el('label', 'filter');
  colour.append(el('span', '', ctx.t('companion-ui-colour-by')));
  const select = el('select');
  for (const [value, key] of [['todo', 'companion-ui-filter-todo'], ['pov', 'companion-ui-filter-pov']]) {
    const option = el('option', '', ctx.t(key));
    option.value = value;
    select.append(option);
  }
  select.value = prefs.colour;
  select.addEventListener('change', () => { prefs.colour = select.value; save('colour', prefs.colour); ctx.rerender(); });
  colour.append(select);
  bar.append(chooser, colour);
  return bar;
}

export function render(container, ctx) {
  container.replaceChildren();
  const fields = ctx.snapshot.sceneFields.filter((field) => field.key !== 'weight');
  const columns = fields.filter((field) => prefs.columns.includes(field.key));
  const acts = ctx.snapshot.acts;
  if (!acts.some((act) => act.chapters.some((chapter) => chapter.scenes.length))) {
    container.append(el('p', 'empty', ctx.t('companion-ui-empty')));
    return;
  }
  container.append(toolbar(fields, ctx));
  const span = 4 + columns.length;
  const table = el('table', 'outline');
  table.append(header(columns, ctx));
  for (const act of acts) {
    const actBody = el('tbody', 'act-body');
    actBody.append(groupRow(act, 'act-row', span, ctx));
    table.append(actBody);
    if (collapsed.has(act.id)) continue;
    for (const chapter of act.chapters) {
      const body = el('tbody', 'chapter-body');
      if (chapter.id) body.dataset.id = chapter.id;
      body.append(groupRow(chapter, 'chapter-row', span, ctx));
      if (!collapsed.has(chapter.id)) {
        for (const scene of sorted(chapter.scenes)) body.append(sceneRow(scene, columns, ctx));
      }
      table.append(body);
      if (chapter.id && !chapter.readOnly && !prefs.sort && window.Sortable) makeSortable(body, ctx);
    }
  }
  container.append(table);
}
