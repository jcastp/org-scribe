// Scene detail panel: every field of one scene, at the side of any view.
// It opens from a card (a click on its background, or I) or from a table row
// (its ⓘ button, or I on the title), and follows the scene where point is in
// Emacs while it is open.  Link fields change through the shared picker, text
// fields (the method's Gap, Outcome, Sequel-decision, …) in place; every
// change is a set-field, set-title or set-todo command, as in the views.
// Previous / next step through the scenes in book order, through the
// matching ones only while a filter is on.  All text is put in with
// textContent.

import { el, startEdit } from './edit.js';
import { openPicker } from './picker.js';
import { matches, isActive } from './filters.js';
import { hasPlotPoints } from './views/matrix.js';
import { item as markerItem } from './markers.js';

/** Every scene in book order, with its act and chapter. */
function allScenes(snapshot) {
  const list = [];
  for (const act of snapshot.acts) {
    for (const chapter of act.chapters) {
      for (const scene of chapter.scenes) list.push({ scene, act, chapter });
    }
  }
  return list;
}

/** The entry of the scene ID in SNAPSHOT, or null. */
export function findScene(snapshot, id) {
  return allScenes(snapshot).find((entry) => entry.scene.id === id) || null;
}

function editable(scene) {
  return Boolean(scene.id) && !scene.readOnly;
}

/** The scene STEP (+1 / -1) places away from INDEX, among the matching ones
 * while a filter is on.  Scenes without an ID cannot be shown (no address). */
function neighbour(list, index, step, ctx) {
  for (let at = index + step; at >= 0 && at < list.length; at += step) {
    const { scene } = list[at];
    if (!scene.id) continue;
    if (isActive(ctx.filters) && !matches(scene, ctx.filters)) continue;
    return scene;
  }
  return null;
}

function button(text, className, label, onClick) {
  const node = el('button', className, text);
  node.type = 'button';
  if (label) { node.title = label; node.setAttribute('aria-label', label); }
  node.addEventListener('click', onClick);
  return node;
}

function navButton(text, key, scene, ctx) {
  const label = ctx.t(key);
  const node = button(text, 'detail-nav-button', scene ? `${label}: ${scene.title}` : label,
                      () => { if (scene) ctx.openDetail(scene.id); });
  node.dataset.key = key;
  node.disabled = !scene;
  return node;
}

function stateSelect(scene, ctx) {
  const select = el('select', 'state-select');
  select.dataset.key = 'state';
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
  select.disabled = !editable(scene);
  select.addEventListener('change', () => ctx.edit('set-todo', { id: scene.id, state: select.value }));
  return select;
}

function words(scene, ctx) {
  const { language } = ctx.snapshot;
  return scene.objective
    ? ctx.t('companion-ui-words-of', scene.words.toLocaleString(language), scene.objective.toLocaleString(language))
    : ctx.t('companion-ui-words', scene.words.toLocaleString(language));
}

/** A link field: the linked names as chips, and a button opening the picker. */
function linkField(scene, field, ctx) {
  const value = scene.fields[field.key] || {};
  const dd = el('dd', 'detail-value');
  const chips = el('span', 'detail-chips');
  for (const link of value.links || []) chips.append(el('span', 'chip-link', link.name));
  for (const name of value.unlinked || []) chips.append(el('span', 'chip-link unlinked', name));
  if (!chips.childNodes.length) chips.append(el('span', 'detail-empty', ctx.t('companion-ui-detail-empty')));
  dd.append(chips);
  if (editable(scene)) {
    const change = button(ctx.t('companion-ui-detail-change'), 'detail-change',
                          ctx.t('companion-ui-detail-edit-field', field.name),
                          () => openPicker(change, scene, field, ctx));
    change.dataset.key = `field-${field.key}`;
    dd.append(change);
  }
  return dd;
}

/** A text field: its text, edited in place (Enter saves, Escape cancels). */
function textField(scene, field, ctx) {
  const value = scene.fields[field.key];
  const dd = el('dd', 'detail-value');
  const text = value && value.raw
    ? el('div', 'detail-text', value.raw)
    : el('div', 'detail-text detail-empty', ctx.t('companion-ui-detail-empty'));
  dd.append(text);
  if (!editable(scene)) return dd;
  text.classList.add('editable');
  text.tabIndex = 0;
  text.dataset.key = `field-${field.key}`;
  text.title = ctx.t('companion-ui-detail-edit-field', field.name);
  const edit = () => startEdit(text, value ? value.raw : '', true, (next) => {
    ctx.edit('set-field', { id: scene.id, key: field.key, value: next });
  }, ctx);
  text.addEventListener('click', edit);
  text.addEventListener('keydown', (event) => {
    if (event.key === 'Enter' || event.key === 'F2') { event.preventDefault(); edit(); }
  });
  return dd;
}

function fieldList(scene, fields, titleKey, ctx) {
  const section = el('section', 'detail-section');
  section.append(el('h3', '', ctx.t(titleKey)));
  const list = el('dl', 'detail-fields');
  for (const field of fields) {
    list.append(el('dt', '', field.name));
    list.append(field.kind === 'text' ? textField(scene, field, ctx) : linkField(scene, field, ctx));
  }
  section.append(list);
  return section;
}

/** The scene's edit markers, each with a ↗ to its line; null when it has none. */
function markerList(scene, ctx) {
  const markers = scene.edits || [];
  if (!markers.length) return null;
  const section = el('section', 'detail-section detail-markers');
  const heading = el('h3', '', ctx.t('companion-ui-edits-in-scene', markers.length));
  // the badges open the panel here (openDetail's field 'edits')
  heading.dataset.key = 'field-edits';
  heading.tabIndex = -1;
  section.append(heading);
  const list = el('ul', 'marker-list');
  for (const marker of markers) list.append(markerItem(marker, null, ctx));
  section.append(list);
  return section;
}

/** Draw the panel for ENTRY ({ scene, act, chapter }) into PANEL. */
export function render(panel, entry, ctx) {
  panel.replaceChildren();
  const { scene, act, chapter } = entry;
  const list = allScenes(ctx.snapshot);
  const index = list.findIndex((candidate) => candidate.scene === scene);

  const head = el('div', 'detail-head');
  const title = el('h2', 'detail-title', scene.title || ctx.t('companion-ui-untitled'));
  title.id = 'detail-title';
  if (editable(scene)) {
    title.classList.add('editable');
    title.tabIndex = 0;
    title.dataset.key = 'title';
    title.title = ctx.t('companion-ui-edit-title');
    const editTitle = () => startEdit(title, scene.title || '', false, (value) => {
      if (value.trim()) ctx.edit('set-title', { id: scene.id, title: value });
    }, ctx);
    title.addEventListener('click', editTitle);
    title.addEventListener('keydown', (event) => {
      if (event.key === 'Enter' || event.key === 'F2') { event.preventDefault(); editTitle(); }
    });
  }
  head.append(title);
  if (scene.id) head.append(button('↗', 'open', ctx.t('companion-ui-open'), () => ctx.goto(scene.id)));
  const close = button('×', 'detail-close', ctx.t('companion-ui-detail-close'), () => ctx.closeDetail());
  close.dataset.key = 'close';
  head.append(close);
  panel.append(head);

  const where = [act.title, chapter.passthrough ? '' : chapter.title].filter(Boolean).join(' › ');
  const place = ctx.t('companion-ui-detail-position', index + 1, list.length);
  panel.append(el('p', 'detail-where', where ? `${where} · ${place}` : place));

  const nav = el('div', 'detail-nav');
  nav.append(navButton('‹', 'companion-ui-detail-previous', neighbour(list, index, -1, ctx), ctx),
             navButton('›', 'companion-ui-detail-next', neighbour(list, index, 1, ctx), ctx));
  panel.append(nav);

  const status = el('div', 'detail-status');
  status.append(stateSelect(scene, ctx), el('span', 'detail-words', words(scene, ctx)));
  panel.append(status);
  if (!editable(scene)) panel.append(el('p', 'detail-note', ctx.t('companion-ui-no-id')));
  const markers = markerList(scene, ctx);
  if (markers) panel.append(markers);

  const fields = ctx.snapshot.sceneFields
    .filter((field) => field.key !== 'plot-point' || hasPlotPoints(ctx));
  panel.append(fieldList(scene, fields.filter((field) => field.kind !== 'text'), 'companion-ui-detail-links', ctx));
  panel.append(fieldList(scene, fields.filter((field) => field.kind === 'text'), 'companion-ui-detail-fields', ctx));
  panel.append(el('p', 'detail-hint', ctx.t('companion-ui-detail-hint')));

  // keys inside the panel; an editor or the picker keeps its own
  panel.onkeydown = (event) => {
    if (event.target.closest('.card-edit')) return;
    if (event.key === 'Escape') { event.preventDefault(); ctx.closeDetail(); }
    if (event.altKey && (event.key === 'ArrowUp' || event.key === 'ArrowDown')) {
      const next = neighbour(list, index, event.key === 'ArrowDown' ? 1 : -1, ctx);
      if (next) { event.preventDefault(); ctx.openDetail(next.id); }
    }
  };
}
