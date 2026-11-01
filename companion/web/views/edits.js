// The Edits tab: every *EDIT* and *NOTE* marker of the project (the F8 6
// index, GET /api/edits), grouped by category or by chapter.  A marker's ↗
// (or a click on its text) shows its line in Emacs; its scene's name opens
// the scene's details.  Reading only: markers are written and cleared in
// Emacs, in the prose.
//
// The list is fetched when the tab is drawn for a revision it does not have
// yet (the old list stays on screen meanwhile).  Markers outside the
// manuscript and the design notebook (another .org file of the project) do
// not change the revision: Refresh reads them again.

import { item, categoryName } from '../markers.js';
import { el } from '../edit.js';

const KEY = 'org-scribe.edits';
let store = KEY;
let prefs = { group: 'category', show: 'all' };
let loaded = null;     // { revision, markers } or { revision, error }
let fetching = null;
let fetchedAt = 0;     // when the last fetch began
let pause = null;      // the timer that fetches after the pause below
let forced = false;    // Refresh: fetch even for the same revision

// Every keystroke in Emacs moves the revision, and the index reads every .org
// file: while the writer types, the old list stays and the new one is fetched
// at most this often.  Refresh and the page's own changes fetch at once.
const MIN_GAP_MS = 8000;

function readPrefs() {
  try {
    const saved = JSON.parse(localStorage.getItem(store) || '{}');
    return {
      group: saved.group === 'chapter' ? 'chapter' : 'category',
      show: ['all', 'edits', 'notes'].includes(saved.show) ? saved.show : 'all',
    };
  } catch (error) {
    return { group: 'category', show: 'all' };
  }
}

function savePrefs() {
  try { localStorage.setItem(store, JSON.stringify(prefs)); } catch (error) { /* in memory only */ }
}

/** Per project: the origin is shared by all of them. */
export function useProject(root) {
  store = `${KEY}:${root}`;
  prefs = readPrefs();
  loaded = null;
  fetching = null;
  fetchedAt = 0;
  clearTimeout(pause);
}

function button(text, className, onClick, title) {
  const node = el('button', className, text);
  node.type = 'button';
  if (title) node.title = title;
  node.addEventListener('click', onClick);
  return node;
}

/** A row of toggle buttons; the pressed one is CURRENT. */
function toggles(label, choices, current, onPick) {
  const wrap = el('div', 'ed-toggle');
  wrap.setAttribute('role', 'group');
  wrap.append(el('span', 'ed-toggle-label', label));
  for (const [value, text] of choices) {
    const node = button(text, 'ed-choice', () => onPick(value));
    node.setAttribute('aria-pressed', String(value === current));
    wrap.append(node);
  }
  return wrap;
}

/** Where a marker is: its scene (a button to its details) and chapter, or its file and heading. */
function place(marker, ctx, withChapter) {
  const node = el('span', 'marker-place');
  if (marker.sceneId) {
    if (withChapter && marker.chapterTitle) node.append(el('span', null, `${marker.chapterTitle} › `));
    node.append(button(marker.sceneTitle || ctx.t('companion-ui-untitled'), 'ed-scene',
      () => ctx.openDetail(marker.sceneId, 'edits'), ctx.t('companion-ui-edits-open-scene')));
  } else {
    const where = marker.manuscript ? '' : `${marker.file} · `;
    node.append(el('span', null, `${where}${marker.heading || ctx.t('companion-ui-edits-no-heading')}`));
  }
  return node;
}

function markerRow(marker, ctx, withChapter) {
  // grouped by category (withChapter), the group's title already says it
  const row = item(marker, marker.manuscript ? null : marker.file, ctx,
    [place(marker, ctx, withChapter)], !withChapter);
  row.querySelector('.marker-text').addEventListener('click', () => ctx.gotoMarker(marker.manuscript ? null : marker.file, marker.line));
  return row;
}

function section(title, markers, ctx, withChapter) {
  const node = el('section', 'ed-group');
  const head = el('h2', 'ed-title', title);
  head.append(' ', el('span', 'hl-count', String(markers.length)));
  node.append(head);
  const list = el('ul', 'marker-list');
  for (const marker of markers) list.append(markerRow(marker, ctx, withChapter));
  node.append(list);
  return node;
}

function byCategory(markers, ctx) {
  const groups = [];
  const edits = markers.filter((marker) => marker.type === 'EDIT');
  const known = (ctx.snapshot.editCategories || []).map((entry) => entry.key);
  for (const key of known) {
    const inside = edits.filter((marker) => marker.category === key);
    if (inside.length) groups.push(section(categoryName(key, ctx), inside, ctx, true));
  }
  const other = edits.filter((marker) => !marker.category || !known.includes(marker.category));
  if (other.length) groups.push(section(ctx.t('companion-ui-edits-other'), other, ctx, true));
  const notes = markers.filter((marker) => marker.type !== 'EDIT');
  if (notes.length) groups.push(section(ctx.t('companion-ui-edits-notes'), notes, ctx, true));
  return groups;
}

function byChapter(markers, ctx) {
  const order = [];
  const groups = new Map();
  for (const marker of markers) {
    let key;
    let title;
    if (marker.manuscript && marker.chapterId) {
      key = `c:${marker.chapterId}`;
      title = marker.chapterTitle;
    } else if (marker.manuscript) {
      key = marker.sceneId ? 'm:scenes' : 'm:outside';
      title = ctx.t(marker.sceneId ? 'companion-ui-edits-manuscript' : 'companion-ui-edits-outside-scenes');
    } else {
      key = `f:${marker.file}`;
      title = marker.file;
    }
    if (!groups.has(key)) { groups.set(key, { title, markers: [] }); order.push(key); }
    groups.get(key).markers.push(marker);
  }
  return order.map((key) => section(groups.get(key).title, groups.get(key).markers, ctx, false));
}

function draw(container, markers, ctx) {
  const page = el('div', 'edits');
  const bar = el('div', 'ed-bar');
  const edits = markers.filter((marker) => marker.type === 'EDIT').length;
  const scenes = new Set(markers.filter((marker) => marker.sceneId).map((marker) => marker.sceneId)).size;
  bar.append(
    el('p', 'hl-hint', ctx.t('companion-ui-edits-summary', edits, markers.length - edits, scenes)),
    toggles(ctx.t('companion-ui-edits-group'), [
      ['category', ctx.t('companion-ui-edits-by-category')],
      ['chapter', ctx.t('companion-ui-edits-by-chapter')],
    ], prefs.group, (value) => { prefs.group = value; savePrefs(); ctx.rerender(); }),
    toggles(ctx.t('companion-ui-edits-show'), [
      ['all', ctx.t('companion-ui-edits-show-all')],
      ['edits', ctx.t('companion-ui-edits-show-edits')],
      ['notes', ctx.t('companion-ui-edits-show-notes')],
    ], prefs.show, (value) => { prefs.show = value; savePrefs(); ctx.rerender(); }),
    button(ctx.t('companion-ui-edits-refresh'), 'ed-refresh', () => { forced = true; ctx.rerender(); },
      ctx.t('companion-ui-edits-refresh-hint')),
  );
  page.append(bar);
  const shown = markers.filter((marker) => prefs.show === 'all'
    || (prefs.show === 'edits') === (marker.type === 'EDIT'));
  if (!shown.length) {
    page.append(el('p', 'empty', ctx.t(markers.length ? 'companion-ui-edits-none-shown' : 'companion-ui-edits-none')));
  } else {
    const groups = el('div', 'ed-groups');
    groups.append(...(prefs.group === 'chapter' ? byChapter(shown, ctx) : byCategory(shown, ctx)));
    page.append(groups);
  }
  container.replaceChildren(page);
}

export function render(container, ctx) {
  const { revision } = ctx.snapshot;
  const fresh = loaded && loaded.revision === revision && !forced;
  if (loaded && loaded.markers) draw(container, loaded.markers, ctx);
  else if (fresh && loaded.error) container.replaceChildren(el('p', 'empty', ctx.t('companion-ui-edits-failed')));
  else container.replaceChildren(el('p', 'empty', ctx.t('companion-ui-health-loading')));
  if (fresh || fetching === revision) return;
  const wait = fetchedAt + MIN_GAP_MS - Date.now();
  if (!forced && loaded && loaded.markers && wait > 0 && !ctx.recentEdit()) {
    clearTimeout(pause);
    pause = setTimeout(() => { if (container.isConnected && ctx.isShown('edits')) ctx.rerender(); }, wait);
    return;
  }
  clearTimeout(pause);
  forced = false;
  fetchedAt = Date.now();
  fetching = revision;
  ctx.fetchEdits()
    .then((answer) => { loaded = { revision, markers: answer.markers }; })
    .catch(() => { loaded = { revision, error: true }; })
    .finally(() => {
      fetching = null;
      if (container.isConnected && ctx.isShown('edits')) ctx.rerender();
    });
}
