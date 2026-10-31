// Filters shared by the views: state, options, matching, persistence.
// A filtered-out scene is dimmed by the view, or hidden when the "Hide the
// rest" switch is on (a per-browser choice, kept per project).
// Values: a link filter holds an entity ID, or '-' for "no value"; the state
// filter holds a keyword, '-' for "no state", or '!KEYWORD' for "any state
// but KEYWORD".  Saved collections use the same values (Emacs checks them).

const STORAGE_KEY = 'org-scribe-companion-filters';
const HIDE_KEY = 'org-scribe-companion-hide';

// Every project is served from the same origin, so localStorage is shared:
// the saved filters are kept per project root.
let scope = '';
const storageKey = () => `${STORAGE_KEY}:${scope}`;
const hideKey = () => `${HIDE_KEY}:${scope}`;

export function setFilterScope(root) { scope = root || ''; }

/** Drop filter values that name nothing in this project (another project's
 * entity, or one deleted since).  Returns the same object, changed or not. */
export function pruneFilters(filters, snapshot) {
  let changed = false;
  for (const filter of FILTERS) {
    const wanted = filters[filter.key];
    if (!wanted) continue;
    if (wanted === '-') continue;
    const known = (snapshot.entities[filter.entity] || []).some((entity) => entity.id === wanted);
    if (!known) { filters[filter.key] = ''; changed = true; }
  }
  const keyword = filters.todo.startsWith('!') ? filters.todo.slice(1) : filters.todo;
  if (filters.todo && filters.todo !== '-' && !snapshot.todo.keywords.includes(keyword)) {
    filters.todo = '';
    changed = true;
  }
  return changed;
}

// Each filter: the scene field it looks at and the entity kind it lists.
export const FILTERS = [
  { key: 'pov', field: 'pov', entity: 'character', label: 'companion-ui-filter-pov' },
  { key: 'character', field: 'characters', entity: 'character', label: 'companion-ui-filter-character' },
  { key: 'plot', field: 'plot', entity: 'plot', label: 'companion-ui-filter-plot' },
  { key: 'location', field: 'location', entity: 'location', label: 'companion-ui-filter-location' },
  { key: 'object', field: 'objects', entity: 'object', label: 'companion-ui-filter-object' },
];

/** Every key a filter set has, the state last. */
export const FILTER_KEYS = FILTERS.map((filter) => filter.key).concat(['todo']);

export function emptyFilters() {
  return { pov: '', character: '', plot: '', location: '', object: '', todo: '' };
}

export function loadFilters() {
  try {
    const saved = JSON.parse(localStorage.getItem(storageKey()));
    if (saved && typeof saved === 'object') return { ...emptyFilters(), ...saved };
  } catch (error) { /* storage blocked or corrupt: start clean */ }
  return emptyFilters();
}

export function saveFilters(filters) {
  try { localStorage.setItem(storageKey(), JSON.stringify(filters)); } catch (error) { /* ignore */ }
}

export function isActive(filters) {
  return FILTER_KEYS.some((key) => (filters[key] || '') !== '');
}

/** True when the two filter sets pick the same scenes (same values). */
export function sameFilters(a, b) {
  return FILTER_KEYS.every((key) => (a[key] || '') === (b[key] || ''));
}

/** A filter set from a saved collection's filters: known keys only. */
export function filtersFrom(saved) {
  const filters = emptyFilters();
  for (const key of FILTER_KEYS) {
    if (typeof saved[key] === 'string') filters[key] = saved[key];
  }
  return filters;
}

/** True when SAVED names something this project no longer has. */
export function isStale(saved, snapshot) {
  return pruneFilters(filtersFrom(saved), snapshot);
}

/** The "Hide the rest" switch. */
export function loadHide() {
  try { return localStorage.getItem(hideKey()) === '1'; } catch (error) { return false; }
}

export function saveHide(hide) {
  try { localStorage.setItem(hideKey(), hide ? '1' : '0'); } catch (error) { /* ignore */ }
}

function linkIds(scene, field) {
  const value = scene.fields[field];
  return value && value.links ? value.links.map((link) => link.id) : [];
}

/** True when the scene's FIELD holds nothing: no link and no plain name. */
function isEmptyField(scene, field) {
  const value = scene.fields[field];
  return !value || (!(value.links && value.links.length) && !(value.unlinked && value.unlinked.length));
}

/** True when "Hide the rest" removes the whole group of SCENES (a chapter, an
 * act): CTX.hide is on and none of them matches. */
export function hiddenGroup(scenes, ctx) {
  return Boolean(ctx.hide) && !scenes.some((scene) => matches(scene, ctx.filters));
}

export function matches(scene, filters) {
  for (const filter of FILTERS) {
    const wanted = filters[filter.key];
    if (!wanted) continue;
    if (wanted === '-') {
      if (!isEmptyField(scene, filter.field)) return false;
    } else if (!linkIds(scene, filter.field).includes(wanted)) {
      return false;
    }
  }
  if (filters.todo) {
    // '-' stands for "no state", '!KEYWORD' for "any state but KEYWORD"
    const state = scene.todo === null ? '-' : scene.todo;
    if (filters.todo.startsWith('!')) {
      if (state === filters.todo.slice(1)) return false;
    } else if (state !== filters.todo) {
      return false;
    }
  }
  return true;
}
