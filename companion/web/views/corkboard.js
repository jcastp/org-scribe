// Corkboard: acts as sections, chapters as rows, scenes as cards.
// Cards drag between chapters and chapters (by their handle) within and across
// acts (SortableJS, loaded as window.Sortable), titles
// and summaries edit in place, the state is a dropdown.  Every change is a
// command to Emacs, which answers with a new snapshot; a refused command
// re-renders from the last snapshot, which is the rollback.
// The toolbar chooses the fields a card shows and a compact density, and acts
// and chapters fold; those choices are kept per project in this browser.
// A folded group keeps its cards in the page, only hidden, so the keyboard
// moves still see where a neighbouring chapter ends.
// All text is put in with textContent.

import { todoColor, textOn } from '../colors.js';
import { paint } from '../scheme.js';
import { matches, isActive, hiddenGroup } from '../filters.js';
import {
  startEdit, moveTo, sceneParent, addSceneButton, addChapterButton, detailButton, isDetailKey,
} from '../edit.js';
import { hasPlotPoints } from './matrix.js';
import { badge as editsBadge } from '../markers.js';

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

// ---- preferences (per project, in this browser) ---------------------------

let STORE = 'org-scribe.corkboard';
// what a card showed before the chooser existed
const DEFAULT_FIELDS = ['summary', 'pov', 'plot-point'];

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
  fields: DEFAULT_FIELDS,
  compact: false,
  folded: new Set(),         // keys of folded acts and chapters (see groupKey)
};

function loadPrefs() {
  // storage is the user's to edit: a value of the wrong shape must not break the tab
  const fields = load('fields', DEFAULT_FIELDS);
  const folded = load('folded', []);
  prefs.fields = Array.isArray(fields) ? fields : DEFAULT_FIELDS;
  prefs.compact = load('compact', false) === true;
  prefs.folded = new Set(Array.isArray(folded) ? folded : []);
}
loadPrefs();

/** Keep the preferences per project: the origin is shared by all of them. */
export function useProject(root) {
  STORE = `org-scribe.corkboard:${root}`;
  loadPrefs();
}

/** The key a fold is kept under: the heading's ID, or its place and title when
 * it has none (the place alone moved the fold to another chapter when one
 * before it moved). */
function groupKey(node, place) {
  return node.id || `${place}:${node.title || ''}`;
}

let refocus = null;           // the fold button to focus again after a redraw

function setFolded(key, folded) {
  if (folded) prefs.folded.add(key); else prefs.folded.delete(key);
  save('folded', [...prefs.folded]);
}

/** The fold button of a group; KEY is its groupKey. */
function foldButton(key, ctx) {
  const open = !prefs.folded.has(key);
  const button = el('button', 'twisty', open ? '▾' : '▸');
  button.type = 'button';
  button.dataset.fold = key;
  button.setAttribute('aria-expanded', String(open));
  button.setAttribute('aria-label', ctx.t('companion-ui-collapse'));
  button.addEventListener('click', (event) => {
    event.stopPropagation();
    setFolded(key, open);
    refocus = key;
    ctx.rerender();
  });
  return button;
}

/** Unfold the group around NODE (a chapter row or an act section), so a scene
 * or chapter moved or added there stays in sight. */
function unfold(node) {
  if (node && node.dataset.fold) setFolded(node.dataset.fold, false);
}

function sceneCount(count, ctx) {
  return count === 1 ? ctx.t('companion-ui-scene-count-one') : ctx.t('companion-ui-scene-count', count);
}

// ---- card content ------------------------------------------------------------

/** The scene fields a card may show, in the language pack's order. */
function cardFields(ctx) {
  return ctx.snapshot.sceneFields
    .filter((field) => field.key !== 'plot-point' || hasPlotPoints(ctx));
}

function progress(words, objective, ctx) {
  const wrap = el('div', 'progress');
  const text = objective
    ? ctx.t('companion-ui-words-of', words.toLocaleString(ctx.snapshot.language), objective.toLocaleString(ctx.snapshot.language))
    : ctx.t('companion-ui-words', words.toLocaleString(ctx.snapshot.language));
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
  const color = todoColor(ctx.snapshot, heading.todo);
  if (color) {
    chip.style.setProperty('--todo', color);
    chip.style.setProperty('--todo-text', textOn(color));
  }
  if (ctx.snapshot.todo.done.includes(heading.todo)) chip.classList.add('done');
  return chip;
}

function stateSelect(scene, ctx) {
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
  select.addEventListener('click', (event) => event.stopPropagation());
  select.addEventListener('dblclick', (event) => event.stopPropagation());
  select.addEventListener('change', () => {
    ctx.edit('set-todo', { id: scene.id, state: select.value });
  });
  return select;
}

function editable(scene) {
  return Boolean(scene.id) && !scene.readOnly;
}

/** The ID of the card before NODE in its row, '' when it has none (to be first). */
function previousId(node) {
  const before = node.previousElementSibling;
  return before ? before.dataset.id || null : '';
}

/** At a chapter's edge, Alt+Up/Down moves the scene to the neighbouring chapter:
 * the end of the one before, the start of the one after. */
function moveToNeighbourChapter(node, down, ctx) {
  const chapters = [...document.querySelectorAll('.chapter')]
    .filter((chapter) => !(ctx.hide && chapter.classList.contains('filtered-out')));
  const next = chapters[chapters.indexOf(node.closest('.chapter')) + (down ? 1 : -1)];
  if (!next || !next.dataset.id) return;
  unfold(next);
  unfold(next.closest('.act'));
  let after = '';
  if (!down) {
    const last = [...next.querySelectorAll('.card')].pop();
    after = last ? last.dataset.id || null : '';
  }
  moveTo(ctx, node.dataset.id, next.dataset.id, after);
}

/** The next (DOWN) or previous element beside NODE that is on screen: with
 * "Hide the rest" on, cards and chapters the filters removed are skipped, or a
 * move would swap with something the writer cannot see and seem to do nothing. */
function visibleSibling(node, down, ctx) {
  let sibling = down ? node.nextElementSibling : node.previousElementSibling;
  while (sibling && ctx.hide
         && (sibling.classList.contains('dimmed') || sibling.classList.contains('filtered-out'))) {
    sibling = down ? sibling.nextElementSibling : sibling.previousElementSibling;
  }
  return sibling;
}

function keyboardMove(event, node, ctx) {
  const down = event.key === 'ArrowDown';
  const sibling = visibleSibling(node, down, ctx);
  if (!sibling) { moveToNeighbourChapter(node, down, ctx); return; }
  const anchor = down ? sibling : sibling.previousElementSibling;
  const chapter = node.closest('.chapter');
  const after = anchor ? anchor.dataset.id || null : '';
  moveTo(ctx, node.dataset.id, chapter.dataset.id, after);
}

/** The summary, edited in place. */
function summaryBlock(scene, ctx) {
  const summary = scene.fields.summary;
  const summaryNode = summary
    ? el('p', 'summary', summary.raw)
    : el('p', 'summary empty', ctx.t('companion-ui-no-summary'));
  if (editable(scene)) {
    summaryNode.classList.add('editable');
    summaryNode.title = ctx.t('companion-ui-edit-summary');
    summaryNode.addEventListener('click', (event) => {
      event.stopPropagation();
      startEdit(summaryNode, summary ? summary.raw : '', true,
        (value) => ctx.edit('set-field', { id: scene.id, key: 'summary', value }, scene.id), ctx);
    });
  }
  return summaryNode;
}

/** Another text field, as a short labelled line (nothing when it is empty).
 * The detail panel edits it; the card only shows it. */
function textBlock(scene, field) {
  const value = scene.fields[field.key];
  if (!value || !value.raw) return null;
  const block = el('p', 'card-field');
  block.append(el('span', 'card-field-name', field.name), el('span', 'card-field-value', value.raw));
  block.title = `${field.name}: ${value.raw}`;
  return block;
}

/** The link fields: the PoV in bold and the plot points as badges, as cards
 * always showed them; any other kind as labelled chips. */
function metaLine(scene, fields, ctx) {
  const meta = el('div', 'meta');
  for (const field of fields) {
    const names = linkNames(scene, field.key);
    if (!names.length) continue;
    if (field.key === 'pov') {
      meta.append(el('span', 'pov', names.join(', ')));
    } else if (field.key === 'plot-point') {
      for (const name of names) {
        const badge = el('span', 'badge', name);
        badge.title = ctx.t('companion-ui-plot-point');
        meta.append(badge);
      }
    } else {
      const group = el('span', 'meta-field');
      group.title = `${field.name}: ${names.join(', ')}`;
      group.append(el('span', 'card-field-name', field.name));
      for (const name of names) group.append(el('span', 'chip-link', name));
      meta.append(group);
    }
  }
  return meta.childNodes.length ? meta : null;
}

function card(scene, ctx) {
  const node = el('article', 'card');
  node.tabIndex = 0;
  if (scene.id) node.dataset.id = scene.id;
  const color = todoColor(ctx.snapshot, scene.todo);
  if (color) node.style.setProperty('--todo', color);
  paint(node, scene, ctx);
  if (scene.todo !== null && ctx.snapshot.todo.done.includes(scene.todo)) node.classList.add('done');
  if (isActive(ctx.filters) && !matches(scene, ctx.filters)) node.classList.add('dimmed');
  if (scene.readOnly) node.classList.add('readonly');
  if (editable(scene)) node.classList.add('movable');
  if (scene.id === ctx.focusId) node.classList.add('focus');
  if (scene.id && scene.id === ctx.detailId) node.classList.add('in-detail');

  const head = el('header', 'card-head');
  const title = el('h4', 'card-title', scene.title || ctx.t('companion-ui-untitled'));
  head.append(title);
  const editTitle = () => startEdit(title, scene.title || '', false,
    (value) => { if (value.trim()) ctx.edit('set-title', { id: scene.id, title: value }, scene.id); }, ctx);
  if (editable(scene)) {
    title.classList.add('editable');
    title.title = ctx.t('companion-ui-edit-title');
    title.addEventListener('click', (event) => { event.stopPropagation(); editTitle(); });
  }
  if (scene.id) {
    const open = el('button', 'open', '↗');
    open.type = 'button';
    const label = ctx.t('companion-ui-open');
    open.title = label;
    open.setAttribute('aria-label', label);
    open.addEventListener('click', (event) => { event.stopPropagation(); ctx.goto(scene.id); });
    const marks = editsBadge(scene, ctx);
    if (marks) head.append(marks);
    head.append(detailButton(scene, ctx), open);
  } else {
    head.append(el('span', 'noid', '⚠'));
    head.title = ctx.t('companion-ui-no-id');
  }
  node.append(head);

  node.append(stateSelect(scene, ctx));
  if (!prefs.compact) {
    const shown = cardFields(ctx).filter((field) => prefs.fields.includes(field.key));
    if (shown.some((field) => field.key === 'summary')) node.append(summaryBlock(scene, ctx));
    for (const field of shown) {
      if (field.kind === 'text' && field.key !== 'summary') {
        const block = textBlock(scene, field);
        if (block) node.append(block);
      }
    }
    const meta = metaLine(scene, shown.filter((field) => field.kind !== 'text'), ctx);
    if (meta) node.append(meta);
    node.append(progress(scene.words, scene.objective, ctx));
  }

  node.addEventListener('dblclick', () => { if (scene.id) ctx.goto(scene.id); });
  // a click on the card's background shows it in the detail panel; its own
  // controls (title, summary, state, buttons) keep their clicks
  node.addEventListener('click', (event) => {
    if (!scene.id || event.target.closest('button, select, input, textarea, .editable')) return;
    ctx.openDetail(scene.id);
  });
  node.addEventListener('keydown', (event) => {
    if (event.target !== node) return;
    if (isDetailKey(event) && scene.id) { event.preventDefault(); ctx.openDetail(scene.id); }
    if (event.key === 'Enter' && scene.id) ctx.goto(scene.id);
    if (event.key === 'F2' && editable(scene)) editTitle();
    if (event.altKey && editable(scene) && (event.key === 'ArrowUp' || event.key === 'ArrowDown')) {
      event.preventDefault();
      keyboardMove(event, node, ctx);
    }
  });
  return node;
}

function makeSortable(cards, ctx) {
  window.Sortable.create(cards, {
    group: 'scenes',
    draggable: '.card.movable',
    filter: 'input, textarea, select, button',
    preventOnFilter: false,
    animation: 150,
    ghostClass: 'drag-ghost',
    onEnd(event) {
      if (event.from === event.to && event.oldIndex === event.newIndex) return;
      const chapter = event.to.closest('.chapter');
      moveTo(ctx, event.item.dataset.id, chapter.dataset.id, previousId(event.item));
    },
  });
}

/** Alt+Up/Down moves a chapter; at an act's edge it crosses to the end of the
 * previous act or the start of the next one. */
function keyboardMoveChapter(event, row, ctx) {
  const down = event.key === 'ArrowDown';
  const sibling = visibleSibling(row, down, ctx);
  if (sibling && sibling.classList.contains('chapter')) {
    const anchor = down ? sibling : sibling.previousElementSibling;
    const after = anchor && anchor.classList.contains('chapter') ? anchor.dataset.id || null : '';
    moveTo(ctx, row.dataset.id, row.closest('.act').dataset.id, after);
    return;
  }
  const acts = [...document.querySelectorAll('.act')]
    .filter((act) => act.dataset.id && !(ctx.hide && act.classList.contains('filtered-out')));
  const next = acts[acts.indexOf(row.closest('.act')) + (down ? 1 : -1)];
  if (!next) return;
  unfold(next);
  let after = '';
  if (!down) {
    const last = [...next.querySelectorAll('.chapter')].pop();
    after = last ? last.dataset.id || null : '';
  }
  moveTo(ctx, row.dataset.id, next.dataset.id, after);
}

function makeChapterSortable(chapters, ctx) {
  window.Sortable.create(chapters, {
    group: 'chapters',
    handle: '.chapter-handle',
    draggable: '.chapter.movable',
    animation: 150,
    ghostClass: 'drag-ghost',
    // tall rows are awkward to drag: fold the cards away meanwhile
    onStart() { document.body.classList.add('dragging-chapter'); },
    onEnd(event) {
      document.body.classList.remove('dragging-chapter');
      if (event.from === event.to && event.oldIndex === event.newIndex) return;
      const act = event.to.closest('.act');
      moveTo(ctx, event.item.dataset.id, act.dataset.id, previousId(event.item));
    },
  });
}

/** Scenes straight under the act (a project with no chapters): cards only. */
function passthroughRow(chapter, ctx) {
  const row = el('div', 'chapter passthrough');
  if (hiddenGroup(chapter.scenes, ctx)) row.classList.add('filtered-out');
  const parent = sceneParent(chapter);
  if (parent) row.dataset.id = parent;
  const cards = el('div', 'cards');
  for (const scene of chapter.scenes) cards.append(card(scene, ctx));
  row.append(cards);
  if (parent && window.Sortable) makeSortable(cards, ctx);
  return row;
}

/** PLACE stands in for the chapter's ID in its fold key when it has none. */
function chapterRow(chapter, place, ctx) {
  if (chapter.passthrough) return passthroughRow(chapter, ctx);
  const row = el('div', 'chapter');
  if (hiddenGroup(chapter.scenes, ctx)) row.classList.add('filtered-out');
  if (chapter.id) row.dataset.id = chapter.id;
  const key = groupKey(chapter, place);
  const folded = prefs.folded.has(key);
  row.dataset.fold = key;
  if (folded) row.classList.add('folded');
  const changeable = editable(chapter);
  if (changeable) row.classList.add('movable');
  const head = el('div', 'chapter-head');
  head.append(foldButton(key, ctx));
  if (changeable) {
    head.tabIndex = 0;
    const handle = el('span', 'chapter-handle', '⠿');
    handle.title = ctx.t('companion-ui-move-chapter');
    head.append(handle);
  }
  const title = el('h3', 'chapter-title', chapter.title || ctx.t('companion-ui-untitled'));
  head.append(title);
  const editTitle = () => startEdit(title, chapter.title || '', false,
    (value) => { if (value.trim()) ctx.edit('set-title', { id: chapter.id, title: value }, chapter.id); }, ctx);
  if (changeable) {
    title.classList.add('editable');
    title.title = ctx.t('companion-ui-edit-title');
    title.addEventListener('click', (event) => { event.stopPropagation(); editTitle(); });
    head.addEventListener('keydown', (event) => {
      if (event.target !== head) return;
      if (event.key === 'F2') editTitle();
      if (event.altKey && (event.key === 'ArrowUp' || event.key === 'ArrowDown')) {
        event.preventDefault();
        keyboardMoveChapter(event, row, ctx);
      }
    });
  }
  const chip = todoChip(chapter, ctx);
  if (chip) head.append(chip);
  if (folded) head.append(el('span', 'folded-count', sceneCount(chapter.scenes.length, ctx)));
  head.append(progress(sumWords(chapter.scenes), chapter.objective, ctx));
  if (chapter.id && !chapter.readOnly) {
    head.append(addSceneButton(chapter.id, chapter.scenes, ctx, () => setFolded(key, false)));
  }
  row.append(head);
  const cards = el('div', 'cards');
  for (const scene of chapter.scenes) cards.append(card(scene, ctx));
  row.append(cards);
  if (chapter.id && !chapter.readOnly && window.Sortable) makeSortable(cards, ctx);
  return row;
}

/** The bar above the board: the card fields, the density, fold and unfold. */
function toolbar(acts, ctx) {
  const bar = el('div', 'table-tools');
  const chooser = el('details', 'columns');
  chooser.append(el('summary', '', ctx.t('companion-ui-card-fields')));
  const choices = el('div', 'columns-list');
  const fields = cardFields(ctx);
  for (const field of fields) {
    const label = el('label');
    const box = el('input');
    box.type = 'checkbox';
    box.checked = prefs.fields.includes(field.key);
    box.disabled = prefs.compact;
    box.addEventListener('change', () => {
      prefs.fields = fields.map((f) => f.key)
        .filter((key) => (key === field.key ? box.checked : prefs.fields.includes(key)));
      save('fields', prefs.fields);
      ctx.rerender();
    });
    label.append(box, el('span', '', field.name));
    choices.append(label);
  }
  chooser.append(choices);
  chooser.open = toolbar.open === true;
  chooser.addEventListener('toggle', () => { toolbar.open = chooser.open; });

  const compact = el('label', 'switch');
  const box = el('input');
  box.type = 'checkbox';
  box.checked = prefs.compact;
  box.addEventListener('change', () => {
    prefs.compact = box.checked;
    save('compact', prefs.compact);
    ctx.rerender();
  });
  compact.append(box, el('span', '', ctx.t('companion-ui-compact')));

  const foldAll = el('button', '', ctx.t('companion-ui-fold-all'));
  foldAll.type = 'button';
  foldAll.addEventListener('click', () => {
    acts.forEach((act, actIndex) => act.chapters.forEach((chapter, chapterIndex) => {
      if (!chapter.passthrough) prefs.folded.add(groupKey(chapter, `act#${actIndex}/chapter#${chapterIndex}`));
    }));
    save('folded', [...prefs.folded]);
    ctx.rerender();
  });
  const unfoldAll = el('button', '', ctx.t('companion-ui-unfold-all'));
  unfoldAll.type = 'button';
  unfoldAll.disabled = prefs.folded.size === 0;
  unfoldAll.addEventListener('click', () => {
    prefs.folded.clear();
    save('folded', []);
    ctx.rerender();
  });
  bar.append(chooser, compact, foldAll, unfoldAll);
  return bar;
}

export function render(container, ctx) {
  container.replaceChildren();
  const acts = ctx.snapshot.acts;
  if (acts.length === 0) {
    container.append(el('p', 'empty', ctx.t('companion-ui-empty')));
    return;
  }
  container.append(toolbar(acts, ctx));
  const board = el('div', prefs.compact ? 'board compact' : 'board');
  acts.forEach((act, actIndex) => {
    const section = el('section', 'act');
    if (hiddenGroup(act.chapters.flatMap((chapter) => chapter.scenes), ctx)) {
      section.classList.add('filtered-out');
    }
    if (act.id) section.dataset.id = act.id;
    const actKey = groupKey(act, `act#${actIndex}`);
    const folded = prefs.folded.has(actKey);
    section.dataset.fold = actKey;
    if (folded) section.classList.add('folded');
    const unfoldAct = () => setFolded(actKey, false);
    const head = el('div', 'act-head');
    head.append(foldButton(actKey, ctx), el('h2', 'act-title', act.title || ''));
    const chip = todoChip(act, ctx);
    if (chip) head.append(chip);
    const scenes = act.chapters.flatMap((chapter) => chapter.scenes);
    if (folded) head.append(el('span', 'folded-count', sceneCount(scenes.length, ctx)));
    head.append(progress(sumWords(scenes), act.objective, ctx));
    const passthrough = act.chapters.find((chapter) => chapter.passthrough);
    if (passthrough) {
      // no chapters here: the act's own button adds a scene
      const parent = sceneParent(passthrough);
      if (parent) head.append(addSceneButton(parent, passthrough.scenes, ctx, unfoldAct));
    } else if (act.id && !act.readOnly) {
      head.append(addChapterButton(act, ctx, unfoldAct));
    }
    section.append(head);
    const chapters = el('div', 'chapters');
    act.chapters.forEach((chapter, chapterIndex) => {
      chapters.append(chapterRow(chapter, `act#${actIndex}/chapter#${chapterIndex}`, ctx));
    });
    section.append(chapters);
    if (act.id && !act.readOnly && window.Sortable) makeChapterSortable(chapters, ctx);
    board.append(section);
  });
  container.append(board);
  if (refocus) {
    const key = refocus;
    refocus = null;
    const again = [...container.querySelectorAll('.twisty')].find((node) => node.dataset.fold === key);
    if (again) again.focus();
  }
}
