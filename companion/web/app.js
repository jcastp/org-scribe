// org-scribe companion: state, API client, event stream, strings, tabs.
// The token comes in the URL (?t=...) and is kept in memory.  Nothing here
// parses org: Emacs sends display-ready data.  All visible text comes from
// /api/strings (data-i18n attributes and t()).

import {
  FILTERS, emptyFilters, loadFilters, saveFilters, isActive, setFilterScope, pruneFilters,
  filtersFrom, loadHide, saveHide,
} from './filters.js';
import * as collections from './collections.js';
import * as detail from './detail.js';
import * as corkboard from './views/corkboard.js';
import * as table from './views/table.js';
import * as storyline from './views/storyline.js';
import * as chapters from './views/chapters.js';
import * as timeline from './views/timeline.js';
import * as relations from './views/relations.js';
import * as forms from './views/forms.js';
import * as edits from './views/edits.js';
import * as health from './views/health.js';
import * as progress from './views/progress.js';
import * as colours from './colors.js';
import * as scheme from './scheme.js';
import * as help from './help.js';

const token = new URLSearchParams(location.search).get('t') || '';
const $ = (id) => document.getElementById(id);

// The views, in tab order.  
const VIEWS = [
  { id: 'corkboard', label: 'companion-ui-view-corkboard', module: corkboard, colourBy: true },
  { id: 'table', label: 'companion-ui-view-table', module: table, colourBy: true },
  { id: 'storyline', label: 'companion-ui-view-storyline', module: storyline, colourBy: true },
  { id: 'chapters', label: 'companion-ui-view-chapters', module: chapters, novelOnly: true },
  { id: 'timeline', label: 'companion-ui-view-timeline', module: timeline, colourBy: true },
  { id: 'relations', label: 'companion-ui-view-relations', module: relations, noFilters: true },
  { id: 'forms', label: 'companion-ui-view-forms', module: forms },
  { id: 'edits', label: 'companion-ui-view-edits', module: edits, noFilters: true },
  { id: 'health', label: 'companion-ui-view-health', module: health, noFilters: true },
  { id: 'progress', label: 'companion-ui-view-progress', module: progress, noFilters: true },
];

/** The key that switches to the tab at INDEX: 1-9, then 0 for the tenth. */
const tabKey = (index) => (index < 9 ? String(index + 1) : index === 9 ? '0' : null);

const state = {
  strings: {},
  snapshot: null,
  filters: emptyFilters(),
  project: null,
  stringsLanguage: null,
  view: VIEWS[0].id,
  focusId: null,
  pendingFocus: null,
  applied: null,       // { name, filters }: the collection last applied, in this tab
  hide: false,         // "Hide the rest": filtered-out scenes are hidden, not dimmed
  detailId: null,      // the scene shown in the detail panel, or null when closed
  followPage: false,   // "Follow in Emacs": selecting a scene moves Emacs' point there
  progress: null,      // GET /api/progress: words against objectives, the plan
  progressRevision: null,
};

// ---- strings -------------------------------------------------------------

/** Fill the %s / %d placeholders of a template, in order. */
function format(template, args) {
  let index = 0;
  return template.replace(/%[sd]/g, () => String(args[index++]));
}

function t(key, ...args) {
  const template = state.strings[key];
  return template === undefined ? key : format(template, args);
}

function applyStrings() {
  for (const node of document.querySelectorAll('[data-i18n]')) {
    node.textContent = t(node.dataset.i18n);
  }
}

// ---- API -----------------------------------------------------------------

async function request(path, options = {}) {
  const response = await fetch(path, {
    ...options,
    headers: { 'X-Org-Scribe-Token': token, ...(options.headers || {}) },
  });
  if (!response.ok) throw new Error(`${path}: ${response.status}`);
  return response.json();
}

async function command(op, args = {}, silent = []) {
  const answer = await request('/api/command', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ op, args }),
  });
  if (!answer.ok && !silent.includes(answer.error.code)) {
    showNotice(t('companion-ui-command-failed', answer.error.message));
  }
  if (answer.ok && answer.warning) showNotice(answer.warning);
  return answer;
}

const fetchEntity = (id) => request(`/api/entity?id=${encodeURIComponent(id)}`);

const fetchHealth = () => request('/api/health');

const goto = (id) => command('goto', { id });

/** Show an edit marker's line in Emacs; FILE null is the manuscript. */
const gotoMarker = (file, line) => command('goto-marker', file ? { file, line } : { line });

const fetchEdits = () => request('/api/edits');

// ---- progress: the header chip and the Progress tab -----------------------
// One fetch per revision, for both: the chip is on every tab.  Emacs loads
// the planner only for a project that opted in and has a plan file.

async function loadProgress() {
  const { revision } = state.snapshot;
  if (state.progressRevision === revision) return;
  state.progressRevision = revision;
  try {
    const answer = await request('/api/progress');
    if (state.progressRevision !== revision) return;   // a newer one is on its way
    state.progress = answer;
  } catch (error) {
    state.progress = null;
  }
  renderChip();
  if (state.view === 'progress') renderView();
}

/** "Today 350 / 800": shown while a plan is running. */
function renderChip() {
  const chip = $('progress-chip');
  const plan = state.progress && state.progress.plan;
  const running = plan && plan.state === 'active' && plan.phase === 'during' && plan.todayTarget !== null;
  chip.hidden = !running;
  if (!running) return;
  const language = state.snapshot.language;
  const actual = plan.todayActual.toLocaleString(language);
  chip.textContent = plan.todayTarget > 0
    ? t('companion-ui-progress-chip', actual, plan.todayTarget.toLocaleString(language))
    : t('companion-ui-progress-chip-rest', actual);
  chip.classList.toggle('met', plan.todayTarget > 0 && plan.todayActual >= plan.todayTarget);
  chip.title = t('companion-ui-progress-chip-hint');
}

/**
 * Run an editing command.  Success: the new snapshot arrives with the
 * answer's revision (the 'changed' event also comes); FOCUS names the card to
 * focus afterwards, or the new scene when FOCUS_NEW.  Failure: the notice is
 * shown (unless its code is in SILENT) and the board is drawn again from the
 * last snapshot (the rollback).  Returns the answer.
 */
let lastEditAt = 0;      // when the page last changed something (see recentEdit)

async function edit(op, args, focus = null, focusNew = false, silent = []) {
  let answer;
  try {
    answer = await command(op, args, silent);
  } catch (error) {
    answer = { ok: false };
    showNotice(t('companion-ui-connection-lost'));
  }
  if (answer.ok) { state.pendingFocus = focusNew ? answer.id : focus; lastEditAt = Date.now(); }
  await refresh().catch(() => {});
  if (!answer.ok) renderView();
  return answer;
}

// A re-render would throw away a half-typed edit: hold it until the edit ends.
// Editors can overlap (one blurs as the next takes the focus), so this counts
// them; and an editor that left the page without a blur must not hold the
// page for good, so the count only stands while something editable has the focus.
let editors = 0;
let refreshWaiting = false;
function editStarted() { editors += 1; }
function isEditing() {
  if (editors <= 0) return false;
  const active = document.activeElement;
  if (active && active.closest && active.closest('input, textarea, select, .picker, .ent-create')) return true;
  editors = 0;
  return false;
}
function editEnded() {
  editors = Math.max(0, editors - 1);
  if (refreshWaiting && !isEditing()) { refreshWaiting = false; refresh().catch(() => {}); }
}

// ---- rendering -----------------------------------------------------------

function showNotice(text) {
  const notice = $('notice');
  notice.textContent = text;
  notice.hidden = false;
  clearTimeout(showNotice.timer);
  showNotice.timer = setTimeout(() => { notice.hidden = true; }, 6000);
}

/** The tabs this project has: a short story has no chapters to roll up to. */
function availableViews() {
  const type = state.snapshot && state.snapshot.project.type;
  return VIEWS.filter((view) => !(view.novelOnly && type === 'short-story'));
}

/** Open the storyline with the scene ID in view. */
function showInStoryline(id) {
  state.view = 'storyline';
  renderAll();
  if (id) focusCard(id);
}

/** Show the view ID; from the keyboard (FOCUS_TAB) the focus goes to its tab. */
function switchView(id, focusTab = false) {
  state.view = id;
  renderAll();
  if (focusTab) {
    const tab = $('tabs').querySelector('[aria-selected="true"]');
    if (tab) tab.focus();
  }
}

function renderTabs() {
  const tabs = $('tabs');
  tabs.replaceChildren();
  availableViews().forEach((view, index) => {
    const tab = document.createElement('button');
    tab.type = 'button';
    tab.setAttribute('role', 'tab');
    tab.setAttribute('aria-selected', String(view.id === state.view));
    tab.textContent = t(view.label);
    const key = tabKey(index);
    if (key) {
      tab.setAttribute('aria-keyshortcuts', key);
      tab.title = t('companion-ui-tab-hint', key);
    }
    tab.addEventListener('click', () => switchView(view.id));
    tabs.append(tab);
  });
}

/** Fill the filters from COLLECTION, leaving out what no longer exists. */
function applyCollection(collection) {
  state.filters = filtersFrom(collection.filters);
  if (pruneFilters(state.filters, state.snapshot)) {
    showNotice(t('companion-ui-collection-stale-applied', collection.name));
  }
  state.applied = { name: collection.name, filters: { ...state.filters } };
  saveFilters(state.filters);
  renderFilters();
  renderView();
}

/** Save, rename or delete a collection through Emacs (set-collection). */
async function saveCollection(args) {
  const answer = await edit('set-collection', args);
  if (!answer.ok) return;
  state.applied = args.delete ? null : { name: answer.name, filters: { ...state.filters } };
  renderFilters();
}

const collectionsContext = () => ({
  t,
  snapshot: state.snapshot,
  filters: state.filters,
  applied: () => state.applied,
  apply: applyCollection,
  forget: () => { state.applied = null; renderFilters(); },
  save: saveCollection,
  editStarted,
  editEnded,
});

function select(labelKey, options, value, onChange) {
  const wrap = document.createElement('label');
  wrap.className = 'filter';
  const caption = document.createElement('span');
  caption.textContent = t(labelKey);
  const dropdown = document.createElement('select');
  const any = document.createElement('option');
  any.value = '';
  any.textContent = t('companion-ui-filter-any');
  dropdown.append(any);
  for (const [optionValue, text] of options) {
    const option = document.createElement('option');
    option.value = optionValue;
    option.textContent = text;
    dropdown.append(option);
  }
  dropdown.value = value;
  dropdown.addEventListener('change', () => onChange(dropdown.value));
  wrap.append(caption, dropdown);
  return wrap;
}

function renderFilters() {
  const container = $('filters');
  container.replaceChildren();
  // the filters pick scenes; a graph of characters has none to pick
  const current = availableViews().find((candidate) => candidate.id === state.view);
  container.hidden = Boolean(current && current.noFilters);
  const { entities, todo } = state.snapshot;
  let collectionsNode = collections.render(collectionsContext());
  const clear = document.createElement('button');
  // the parts of the bar that follow the filters; the dropdowns keep the focus
  const follow = () => {
    const fresh = collections.render(collectionsContext());
    collectionsNode.replaceWith(fresh);
    collectionsNode = fresh;
    clear.hidden = !isActive(state.filters);
  };
  const change = (key) => (value) => {
    state.filters[key] = value;
    saveFilters(state.filters);
    follow();
    renderView();
  };
  container.append(collectionsNode);
  for (const filter of FILTERS) {
    const options = [['-', t('companion-ui-filter-none')]].concat(
      (entities[filter.entity] || [])
        .filter((entity) => entity.id)
        .map((entity) => [entity.id, entity.name]),
    );
    container.append(select(filter.label, options, state.filters[filter.key], change(filter.key)));
  }
  const states = todo.keywords.map((keyword) => [keyword, keyword])
    .concat([['-', t('companion-ui-no-todo')]])
    .concat(todo.keywords.map((keyword) => [`!${keyword}`, t('companion-ui-filter-not', keyword)]));
  container.append(select('companion-ui-filter-todo', states, state.filters.todo, change('todo')));
  const hide = document.createElement('label');
  hide.className = 'switch';
  const box = document.createElement('input');
  box.type = 'checkbox';
  box.checked = state.hide;
  box.addEventListener('change', () => { state.hide = box.checked; saveHide(state.hide); renderView(); });
  const hideText = document.createElement('span');
  hideText.textContent = t('companion-ui-filter-hide');
  hide.append(box, hideText);
  container.append(hide);
  clear.type = 'button';
  clear.textContent = t('companion-ui-filter-clear');
  clear.hidden = !isActive(state.filters);
  clear.addEventListener('click', () => {
    state.filters = emptyFilters();
    saveFilters(state.filters);
    renderFilters();
    renderView();
  });
  container.append(clear);
}

function renderView() {
  const view = availableViews().find((candidate) => candidate.id === state.view) || VIEWS[0];
  const scroll = window.scrollY;
  // hiding only means something while a filter is on
  $('view').classList.toggle('hide-unmatched', state.hide && isActive(state.filters));
  view.module.render($('view'), viewContext());
  window.scrollTo(0, scroll);
  // the panel's previous / next follow the filters, and its data the snapshot
  renderDetail();
  if (state.pendingFocus) {
    const id = state.pendingFocus;
    state.pendingFocus = null;
    const target = [...document.querySelectorAll('.card, .chapter, .scene-row, .chapter-row')]
      .find((node) => node.dataset.id === id);
    if (target && target.classList.contains('chapter')) {
      // a new chapter: bring it into view and put the focus on its header
      target.scrollIntoView({ block: 'nearest' });
      const head = target.querySelector('.chapter-head');
      if (head) head.focus();
    } else if (target && target.matches('.scene-row, .chapter-row')) {
      // the table: a scene's title (Enter renames it), a chapter's twisty
      target.scrollIntoView({ block: 'nearest' });
      const spot = target.querySelector('.title-text.editable, .twisty');
      if (spot) spot.focus();
    } else if (target) {
      target.focus();
    }
  }
}

/** What the views (and the detail panel) are drawn with. */
function viewContext() {
  return {
    snapshot: state.snapshot,
    filters: state.filters,
    // hiding only means something while a filter is on
    hide: state.hide && isActive(state.filters),
    hideSwitch: state.hide,  // the switch itself: the table's find box narrows too
    focusId: state.focusId,
    detailId: state.detailId,
    t,
    goto,
    edit,
    fetchEntity,
    notice: showNotice,
    rerender: renderView,
    nameFor,
    showInStoryline,
    editStarted,
    editEnded,
    openDetail,
    openEntity,
    fetchHealth,
    fetchEdits,
    gotoMarker,
    isShown: (id) => state.view === id,
    // true just after the page's own change: the tabs that read a costly
    // report from Emacs (Health, Edits) then refetch at once, not on their pause
    recentEdit: () => Date.now() - lastEditAt < 4000,
    progress: () => state.progress,
  };
}

/** Show the entity ID's form in the Entities tab. */
function openEntity(id) {
  forms.select(id);
  switchView('forms');
}

// ---- scene detail panel --------------------------------------------------

/**
 * Show the scene ID in the detail panel and put the focus in it.  With
 * FIELD (a scene field's key), the focus goes to that field instead, and a
 * link field's picker opens (the Health tab's "missing PoV").
 */
function openDetail(id, field = null) {
  if (!id) return;
  state.detailId = id;
  followInEmacs(id);
  renderView();            // marks the scene in the view, and draws the panel
  const spot = field && $('detail').querySelector(`[data-key="field-${CSS.escape(field)}"]`);
  if (spot) {
    spot.scrollIntoView({ block: 'nearest' });
    spot.focus();
    spot.click();             // a link field opens its picker, a text field its editor
    return;
  }
  const title = document.getElementById('detail-title');
  if (title) title.focus();
}

/** Close the panel; the focus goes back to the scene in the view. */
function closeDetail() {
  const id = state.detailId;
  state.detailId = null;
  renderView();
  const target = [...document.querySelectorAll('#view [data-id]')].find((node) => node.dataset.id === id);
  if (target) {
    const spot = target.matches('.scene-row') ? target.querySelector('.title-text') || target : target;
    spot.focus();
  }
}

function renderDetail() {
  const panel = $('detail');
  const entry = state.detailId && state.snapshot && detail.findScene(state.snapshot, state.detailId);
  if (!entry) {
    // closed, or the scene is gone (deleted or moved out of the manuscript in Emacs)
    state.detailId = null;
    panel.hidden = true;
    panel.replaceChildren();
    document.body.classList.remove('detail-open');
    return;
  }
  // a re-render (after every change) keeps the focus on the same control
  const active = document.activeElement;
  const key = active && panel.contains(active) ? active.dataset.key : null;
  detail.render(panel, entry, {
    ...viewContext(),
    // the panel keeps the focus: no card in the view is to take it after a change
    edit: (op, args) => edit(op, args),
    closeDetail,
  });
  panel.hidden = false;
  document.body.classList.add('detail-open');
  if (key) {
    const again = [...panel.querySelectorAll('[data-key]')].find((node) => node.dataset.key === key);
    if (again) again.focus();
  }
}

function renderHeader() {
  const { snapshot } = state;
  $('project-title').textContent = snapshot.project.title || '';
  document.title = snapshot.project.title || 'org-scribe';
  document.documentElement.lang = snapshot.language;
  const unsaved = Object.values(snapshot.buffers).some((buffer) => buffer.modified);
  const status = $('save-state');
  status.textContent = t(unsaved ? 'companion-ui-unsaved' : 'companion-ui-saved');
  status.classList.toggle('unsaved', unsaved);
  $('save-button').disabled = !unsaved;
  const undoButton = $('undo-button');
  undoButton.disabled = !snapshot.canUndo;
  undoButton.title = t('companion-ui-undo-hint');
  $('follow-page').checked = state.followPage;
  $('follow-switch').title = t('companion-ui-follow-page-hint');
  $('help-button').title = t('companion-ui-help-hint');
}

/** Which entity list names the IDs a colour pin of each kind holds. */
const PIN_ENTITY = { pov: 'character', plot: 'plot', location: 'location', plotpoint: 'plot-point' };

/** The name to show for a colour pin: most pins hold an entity's or an act's ID. */
function nameFor(kind, value) {
  if (kind === 'act') {
    const act = state.snapshot.acts.find((candidate) => candidate.id === value);
    return act ? act.title : value;
  }
  if (!PIN_ENTITY[kind]) return value;
  const entity = (state.snapshot.entities[PIN_ENTITY[kind]] || []).find((candidate) => candidate.id === value);
  return entity ? entity.name : value;
}

const panelContext = () => ({
  t, edit, editStarted, editEnded, nameFor, closePanel: () => toggleColours(false),
});

function renderColours() {
  const panel = $('colors-panel');
  if (!panel.hidden) colours.renderPanel(panel, panelContext());
}

function toggleColours(show) {
  const panel = $('colors-panel');
  panel.hidden = !show;
  $('colors-button').setAttribute('aria-expanded', String(show));
  colours.closePicker();
  renderColours();
}

/** The "Colour by" choice in the header and its legend, on the tabs that use them. */
function renderColourBy() {
  const current = availableViews().find((candidate) => candidate.id === state.view);
  const uses = Boolean(current && current.colourBy);
  const slot = $('colour-by');
  const legend = $('colour-legend');
  slot.replaceChildren();
  legend.replaceChildren();
  slot.hidden = !uses;
  legend.hidden = true;
  if (!uses) return;
  const ctx = { ...panelContext(), snapshot: state.snapshot };
  slot.append(scheme.chooser(ctx, () => { renderColourBy(); renderView(); }));
  const key = scheme.legend(ctx);
  if (key) { legend.append(key); legend.hidden = false; }
}

function renderAll() {
  colours.setColors(state.snapshot.colors);
  renderHeader();
  renderColours();
  renderTabs();
  renderColourBy();
  renderFilters();
  renderView();
}

// ---- loading and events --------------------------------------------------

async function refresh() {
  if (isEditing()) { refreshWaiting = true; return; }
  const fresh = await request('/api/project');
  // an editor opened while the answer was on its way: draw it afterwards
  if (isEditing()) { refreshWaiting = true; return; }
  state.snapshot = fresh;
  const { language } = state.snapshot;
  if (state.stringsLanguage && state.stringsLanguage !== language) {   // another project
    state.strings = await request('/api/strings');
    applyStrings();
  }
  state.stringsLanguage = language;
  const { root } = state.snapshot.project;
  if (state.project !== root) {          // first load: this project's own saved state
    state.project = root;
    setFilterScope(root);
    state.filters = loadFilters();
    state.hide = loadHide();
    state.applied = null;
    state.detailId = null;
    table.useProject(root);
    corkboard.useProject(root);
    scheme.useProject(root);
    health.useProject(root);
    edits.useProject(root);
    state.followPage = loadFollowPage(root);
    followSent = null;
    state.progress = null;
    state.progressRevision = null;
  }
  if (pruneFilters(state.filters, state.snapshot)) saveFilters(state.filters);
  renderAll();
  loadProgress();
}

function focusCard(id) {
  state.focusId = id;
  for (const node of document.querySelectorAll('.focus')) node.classList.remove('focus');
  const target = [...document.querySelectorAll('[data-id]')].find((node) => node.dataset.id === id);
  if (!target) return;
  target.classList.add('focus');
  target.scrollIntoView({ block: 'center', behavior: 'smooth' });
}

let staleWhileHidden = false;
document.addEventListener('visibilitychange', () => {
  if (!document.hidden && staleWhileHidden) { staleWhileHidden = false; refresh().catch(() => {}); }
});

function connect() {
  const events = new EventSource(`/api/events?t=${encodeURIComponent(token)}`);
  events.addEventListener('open', () => {
    $('banner').hidden = true;
    refresh().catch(() => {});
  });
  events.addEventListener('changed', (event) => {
    const { revision } = JSON.parse(event.data);
    if (state.snapshot && revision === state.snapshot.revision) return;
    // a hidden page (another tab, a minimised window) has nobody to draw for:
    // Emacs builds the project while the writer waits, so do it on return
    if (document.hidden) { staleWhileHidden = true; return; }
    refresh().catch(() => {});
  });
  events.addEventListener('focus', (event) => {
    const { id } = JSON.parse(event.data);
    followSent = id;   // Emacs is there already: selecting it again sends nothing
    focusCard(id);
    // an open panel follows the scene where point is, unless a field is being edited
    if (state.detailId && !isEditing() && id !== state.detailId
        && detail.findScene(state.snapshot, id)) {
      state.detailId = id;
      renderView();
    }
  });
  events.addEventListener('bye', () => { events.close(); $('banner').hidden = false; });
  events.addEventListener('error', () => { $('banner').hidden = false; });
}

// ---- follow the page ----------------------------------------------------
// With the switch on, the scene the writer selects on the page (a click or
// the keyboard focus on a card, a table row, a timeline item, or the scene
// the detail panel opens) moves Emacs' point there, without raising Emacs
// (the follow op).  Tabbing through cards sends only where the focus stops.

const followKey = (root) => `org-scribe.follow-page:${root}`;

function loadFollowPage(root) {
  try { return localStorage.getItem(followKey(root)) === '1'; } catch (error) { return false; }
}

function saveFollowPage(root, on) {
  try { localStorage.setItem(followKey(root), on ? '1' : '0'); } catch (error) { /* in memory only */ }
}

let followSent = null;    // the scene Emacs was last sent to, or told the page it is at
let followTimer = null;

function followInEmacs(id) {
  if (!state.followPage || !id || !state.snapshot || !detail.findScene(state.snapshot, id)) return;
  clearTimeout(followTimer);
  followTimer = setTimeout(() => {
    if (!state.followPage || id === followSent) return;
    followSent = id;
    command('follow', { id }, ['not-found']).catch(() => {});
  }, 250);
}

$('view').addEventListener('focusin', (event) => {
  const holder = event.target.closest && event.target.closest('[data-id]');
  if (holder) followInEmacs(holder.dataset.id);
});

$('follow-page').addEventListener('change', () => {
  state.followPage = $('follow-page').checked;
  if (state.project) saveFollowPage(state.project, state.followPage);
  followSent = null;
});

$('colors-button').addEventListener('click', () => toggleColours($('colors-panel').hidden));
// light <-> dark: the palette slots and the warning colours change with it
colours.onModeChange(() => { if (state.snapshot) renderAll(); });

// Undo the page's last change.  Emacs refuses (and says why) when the buffer
// changed there since; the notice names the file and how to get it back.
let undoing = false;
async function undo() {
  if (undoing || isEditing() || !state.snapshot || !state.snapshot.canUndo) return;
  undoing = true;
  try {
    const answer = await edit('undo', {});
    if (answer.ok) showNotice(t('companion-ui-undone', answer.file));
  } finally {
    undoing = false;
  }
}

$('undo-button').addEventListener('click', undo);
// Ctrl+Z (Cmd+Z) outside a text field: a field keeps the browser's own undo
document.addEventListener('keydown', (event) => {
  if (!(event.ctrlKey || event.metaKey) || event.shiftKey || event.altKey) return;
  if (event.key !== 'z' && event.key !== 'Z') return;
  const target = event.target;
  if (target.closest && target.closest('input, textarea, select, [contenteditable]')) return;
  event.preventDefault();
  undo();
});

// The page's own keys, outside a text field and without Ctrl, Alt or Cmd:
// ? opens (and closes) the list of shortcuts, 1-9 switch tabs.  A view that
// handled the key itself (preventDefault) keeps it.
const toggleHelp = () => help.toggle($('help'), { t, tabCount: availableViews().length });
$('help-button').addEventListener('click', toggleHelp);
$('progress-chip').addEventListener('click', () => switchView('progress'));
document.addEventListener('keydown', (event) => {
  if (event.ctrlKey || event.metaKey || event.altKey || event.defaultPrevented) return;
  const target = event.target;
  if (target.closest && target.closest('input, textarea, select, [contenteditable]')) return;
  if (event.key === '?') { event.preventDefault(); toggleHelp(); return; }
  if ($('help').open || !state.snapshot || !/^[0-9]$/.test(event.key)) return;
  const view = availableViews().find((_, index) => tabKey(index) === event.key);
  if (!view) return;
  event.preventDefault();
  switchView(view.id, true);
});

$('save-button').addEventListener('click', async () => {
  const answer = await command('save');
  if (answer.ok) refresh().catch(() => {});
});

try {
  state.strings = await request('/api/strings');
  applyStrings();
  await refresh();
  connect();
} catch (error) {
  applyStrings();
  $('banner').hidden = false;
}
