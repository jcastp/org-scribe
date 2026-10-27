// Filters shared by the views: state, options, matching, persistence.
// A filtered-out scene is dimmed by the view, never removed.

const STORAGE_KEY = 'org-scribe-companion-filters';

// Each filter: the scene field it looks at and the entity kind it lists.
export const FILTERS = [
  { key: 'pov', field: 'pov', entity: 'character', label: 'companion-ui-filter-pov' },
  { key: 'character', field: 'characters', entity: 'character', label: 'companion-ui-filter-character' },
  { key: 'plot', field: 'plot', entity: 'plot', label: 'companion-ui-filter-plot' },
  { key: 'location', field: 'location', entity: 'location', label: 'companion-ui-filter-location' },
];

export function emptyFilters() {
  return { pov: '', character: '', plot: '', location: '', todo: '' };
}

export function loadFilters() {
  try {
    const saved = JSON.parse(localStorage.getItem(STORAGE_KEY));
    if (saved && typeof saved === 'object') return { ...emptyFilters(), ...saved };
  } catch (error) { /* storage blocked or corrupt: start clean */ }
  return emptyFilters();
}

export function saveFilters(filters) {
  try { localStorage.setItem(STORAGE_KEY, JSON.stringify(filters)); } catch (error) { /* ignore */ }
}

export function isActive(filters) {
  return Object.values(filters).some((value) => value !== '');
}

function linkIds(scene, field) {
  const value = scene.fields[field];
  return value && value.links ? value.links.map((link) => link.id) : [];
}

export function matches(scene, filters) {
  for (const filter of FILTERS) {
    const wanted = filters[filter.key];
    if (wanted && !linkIds(scene, filter.field).includes(wanted)) return false;
  }
  if (filters.todo) {
    // '-' stands for "no state"
    const state = scene.todo === null ? '-' : scene.todo;
    if (state !== filters.todo) return false;
  }
  return true;
}
