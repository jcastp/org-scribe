// Corkboard: acts as sections, chapters as rows, scenes as cards.
// Cards drag between chapters and chapters (by their handle) within and across
// acts (SortableJS, loaded as window.Sortable), titles
// and summaries edit in place, the state is a dropdown.  Every change is a
// command to Emacs, which answers with a new snapshot; a refused command
// re-renders from the last snapshot, which is the rollback.
// All text is put in with textContent.

import { todoColor, textOn } from '../colors.js';
import { matches, isActive } from '../filters.js';
import { startEdit, moveTo } from '../edit.js';

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
  const chapters = [...document.querySelectorAll('.chapter')];
  const next = chapters[chapters.indexOf(node.closest('.chapter')) + (down ? 1 : -1)];
  if (!next || !next.dataset.id) return;
  let after = '';
  if (!down) {
    const last = [...next.querySelectorAll('.card')].pop();
    after = last ? last.dataset.id || null : '';
  }
  moveTo(ctx, node.dataset.id, next.dataset.id, after);
}

function keyboardMove(event, node, ctx) {
  const down = event.key === 'ArrowDown';
  const sibling = down ? node.nextElementSibling : node.previousElementSibling;
  if (!sibling) { moveToNeighbourChapter(node, down, ctx); return; }
  const anchor = down ? sibling : sibling.previousElementSibling;
  const chapter = node.closest('.chapter');
  const after = anchor ? anchor.dataset.id || null : '';
  moveTo(ctx, node.dataset.id, chapter.dataset.id, after);
}

function card(scene, ctx) {
  const node = el('article', 'card');
  node.tabIndex = 0;
  if (scene.id) node.dataset.id = scene.id;
  const color = todoColor(ctx.snapshot, scene.todo);
  if (color) node.style.setProperty('--todo', color);
  if (scene.todo !== null && ctx.snapshot.todo.done.includes(scene.todo)) node.classList.add('done');
  if (isActive(ctx.filters) && !matches(scene, ctx.filters)) node.classList.add('dimmed');
  if (scene.readOnly) node.classList.add('readonly');
  if (editable(scene)) node.classList.add('movable');
  if (scene.id === ctx.focusId) node.classList.add('focus');

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
    head.append(open);
  } else {
    head.append(el('span', 'noid', '⚠'));
    head.title = ctx.t('companion-ui-no-id');
  }
  node.append(head);

  node.append(stateSelect(scene, ctx));

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
  node.append(summaryNode);

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
    if (event.target !== node) return;
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
  const sibling = down ? row.nextElementSibling : row.previousElementSibling;
  if (sibling && sibling.classList.contains('chapter')) {
    const anchor = down ? sibling : sibling.previousElementSibling;
    const after = anchor && anchor.classList.contains('chapter') ? anchor.dataset.id || null : '';
    moveTo(ctx, row.dataset.id, row.closest('.act').dataset.id, after);
    return;
  }
  const acts = [...document.querySelectorAll('.act')].filter((act) => act.dataset.id);
  const next = acts[acts.indexOf(row.closest('.act')) + (down ? 1 : -1)];
  if (!next) return;
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

function chapterRow(chapter, ctx) {
  const row = el('div', 'chapter');
  if (chapter.id) row.dataset.id = chapter.id;
  const changeable = editable(chapter);
  if (changeable) row.classList.add('movable');
  const head = el('div', 'chapter-head');
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
  head.append(progress(sumWords(chapter.scenes), chapter.objective, ctx));
  if (chapter.id && !chapter.readOnly) {
    const add = el('button', 'add-scene', '+');
    add.type = 'button';
    const label = ctx.t('companion-ui-new-scene');
    add.title = label;
    add.setAttribute('aria-label', label);
    add.addEventListener('click', () => {
      const last = [...chapter.scenes].reverse().find((scene) => scene.id);
      ctx.edit('create-scene', { parent: chapter.id, after: last ? last.id : null }, null, true);
    });
    head.append(add);
  }
  row.append(head);
  const cards = el('div', 'cards');
  for (const scene of chapter.scenes) cards.append(card(scene, ctx));
  row.append(cards);
  if (chapter.id && !chapter.readOnly && window.Sortable) makeSortable(cards, ctx);
  return row;
}

export function render(container, ctx) {
  container.replaceChildren();
  const acts = ctx.snapshot.acts;
  if (acts.length === 0) {
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
    if (act.id && !act.readOnly) {
      const add = el('button', 'add-scene', '+');
      add.type = 'button';
      const label = ctx.t('companion-ui-new-chapter');
      add.title = label;
      add.setAttribute('aria-label', label);
      // the title is asked before the chapter exists: Escape (or nothing
      // typed) leaves no stray heading behind
      add.addEventListener('click', () => {
        const last = [...act.chapters].reverse().find((chapter) => chapter.id);
        startEdit(add, '', false, (title) => {
          if (!title.trim()) return;
          ctx.edit('create-chapter', { parent: act.id, after: last ? last.id : null, title });
        }, ctx, label);
      });
      head.append(add);
    }
    section.append(head);
    const chapters = el('div', 'chapters');
    for (const chapter of act.chapters) chapters.append(chapterRow(chapter, ctx));
    section.append(chapters);
    if (act.id && !act.readOnly && window.Sortable) makeChapterSortable(chapters, ctx);
    container.append(section);
  }
}
