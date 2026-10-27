// org-scribe companion: state, API client, event stream, strings, tabs.
// The token comes in the URL (?t=...) and is kept in memory.  Nothing here
// parses org: Emacs sends display-ready data.  All visible text comes from
// /api/strings (data-i18n attributes and t()).

import { FILTERS, emptyFilters, loadFilters, saveFilters, isActive, setFilterScope, pruneFilters } from './filters.js';
import * as corkboard from './views/corkboard.js';
import * as table from './views/table.js';
import * as storyline from './views/storyline.js';
import * as chapters from './views/chapters.js';
import * as relations from './views/relations.js';
import * as forms from './views/forms.js';

const token = new URLSearchParams(location.search).get('t') || '';
const $ = (id) => document.getElementById(id);

// The views, in tab order.  
const VIEWS = [
  { id: 'corkboard', label: 'companion-ui-view-corkboard', module: corkboard },
  { id: 'table', label: 'companion-ui-view-table', module: table },
  { id: 'storyline', label: 'companion-ui-view-storyline', module: storyline },
  { id: 'chapters', label: 'companion-ui-view-chapters', module: chapters, novelOnly: true },
  { id: 'relations', label: 'companion-ui-view-relations', module: relations, noFilters: true },
  { id: 'forms', label: 'companion-ui-view-forms', module: forms },
];

const state = {
  strings: {},
  snapshot: null,
  filters: emptyFilters(),
  project: null,
  stringsLanguage: null,
  view: VIEWS[0].id,
  focusId: null,
  pendingFocus: null,
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
  return answer;
}

const fetchEntity = (id) => request(`/api/entity?id=${encodeURIComponent(id)}`);

const goto = (id) => command('goto', { id });

/**
 * Run an editing command.  Success: the new snapshot arrives with the
 * answer's revision (the 'changed' event also comes); FOCUS names the card to
 * focus afterwards, or the new scene when FOCUS_NEW.  Failure: the notice is
 * shown (unless its code is in SILENT) and the board is drawn again from the
 * last snapshot (the rollback).  Returns the answer.
 */
async function edit(op, args, focus = null, focusNew = false, silent = []) {
  let answer;
  try {
    answer = await command(op, args, silent);
  } catch (error) {
    answer = { ok: false };
    showNotice(t('companion-ui-connection-lost'));
  }
  if (answer.ok) state.pendingFocus = focusNew ? answer.id : focus;
  await refresh().catch(() => {});
  if (!answer.ok) renderView();
  return answer;
}

// A re-render would throw away a half-typed edit: hold it until the edit ends.
let editing = false;
let refreshWaiting = false;
function editStarted() { editing = true; }
function editEnded() {
  editing = false;
  if (refreshWaiting) { refreshWaiting = false; refresh().catch(() => {}); }
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

function renderTabs() {
  const tabs = $('tabs');
  tabs.replaceChildren();
  for (const view of availableViews()) {
    const tab = document.createElement('button');
    tab.type = 'button';
    tab.setAttribute('role', 'tab');
    tab.setAttribute('aria-selected', String(view.id === state.view));
    tab.textContent = t(view.label);
    tab.addEventListener('click', () => { state.view = view.id; renderAll(); });
    tabs.append(tab);
  }
}

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
  const change = (key) => (value) => { state.filters[key] = value; saveFilters(state.filters); renderView(); };
  for (const filter of FILTERS) {
    const options = (entities[filter.entity] || [])
      .filter((entity) => entity.id)
      .map((entity) => [entity.id, entity.name]);
    container.append(select(filter.label, options, state.filters[filter.key], change(filter.key)));
  }
  const states = todo.keywords.map((keyword) => [keyword, keyword])
    .concat([['-', t('companion-ui-no-todo')]]);
  container.append(select('companion-ui-filter-todo', states, state.filters.todo, change('todo')));
  if (isActive(state.filters)) {
    const clear = document.createElement('button');
    clear.type = 'button';
    clear.textContent = t('companion-ui-filter-clear');
    clear.addEventListener('click', () => {
      state.filters = emptyFilters();
      saveFilters(state.filters);
      renderFilters();
      renderView();
    });
    container.append(clear);
  }
}

function renderView() {
  const view = availableViews().find((candidate) => candidate.id === state.view) || VIEWS[0];
  const scroll = window.scrollY;
  view.module.render($('view'), {
    snapshot: state.snapshot,
    filters: state.filters,
    focusId: state.focusId,
    t,
    goto,
    edit,
    fetchEntity,
    notice: showNotice,
    rerender: renderView,
    showInStoryline,
    editStarted,
    editEnded,
  });
  window.scrollTo(0, scroll);
  if (state.pendingFocus) {
    const id = state.pendingFocus;
    state.pendingFocus = null;
    const target = [...document.querySelectorAll('.card, .chapter')].find((node) => node.dataset.id === id);
    if (target && target.classList.contains('chapter')) {
      // a new chapter: straight into its title
      const title = target.querySelector('.chapter-title');
      if (title.classList.contains('editable')) title.click();
    } else if (target) {
      target.focus();
    }
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
}

function renderAll() {
  renderHeader();
  renderTabs();
  renderFilters();
  renderView();
}

// ---- loading and events --------------------------------------------------

async function refresh() {
  if (editing) { refreshWaiting = true; return; }
  state.snapshot = await request('/api/project');
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
    table.useProject(root);
  }
  if (pruneFilters(state.filters, state.snapshot)) saveFilters(state.filters);
  renderAll();
}

function focusCard(id) {
  state.focusId = id;
  for (const node of document.querySelectorAll('.focus')) node.classList.remove('focus');
  const target = [...document.querySelectorAll('[data-id]')].find((node) => node.dataset.id === id);
  if (!target) return;
  target.classList.add('focus');
  target.scrollIntoView({ block: 'center', behavior: 'smooth' });
}

function connect() {
  const events = new EventSource(`/api/events?t=${encodeURIComponent(token)}`);
  events.addEventListener('open', () => {
    $('banner').hidden = true;
    refresh().catch(() => {});
  });
  events.addEventListener('changed', (event) => {
    const { revision } = JSON.parse(event.data);
    if (!state.snapshot || revision !== state.snapshot.revision) refresh().catch(() => {});
  });
  events.addEventListener('focus', (event) => focusCard(JSON.parse(event.data).id));
  events.addEventListener('bye', () => { events.close(); $('banner').hidden = false; });
  events.addEventListener('error', () => { $('banner').hidden = false; });
}

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
