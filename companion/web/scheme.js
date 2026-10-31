// The colour scheme: which property of a scene the Corkboard, Table, Storyline
// and Timeline colour scenes by.  One choice for all of them, kept per project
// in localStorage.  Emacs decides the colours (the snapshot's `colors`); this
// file only reads a scene's values, looks their colours up and writes them
// into CSS variables the stylesheet draws:
//   --crit-edge  a gradient: one hard-stopped band per value (up to three)
//   --crit-tint  a light wash of the colour, or a neutral one when the scene
//                has several values, so the tint never favours one of them
// `todo` is the default and draws nothing here: the TODO colour is the
// edge already.  All text is put in with textContent.

import { colorOf, legendSwatch } from './colors.js';
import { ids, hasPlotPoints } from './views/matrix.js';
import { el } from './edit.js';

const KEY = 'org-scribe.colourBy';
const MAX_BANDS = 3;

/** The criteria, in picker order.  FIELD is the scene field read for the
 * values; KIND the colour kind (see the snapshot's `colors`); ENTITY the
 * snapshot's entity list that names the values. */
const CRITERIA = [
  { id: 'todo', label: 'companion-ui-filter-todo' },
  { id: 'pov', label: 'companion-ui-filter-pov', kind: 'pov', field: 'pov', entity: 'character' },
  { id: 'plot', label: 'companion-ui-filter-plot', kind: 'plot', field: 'plot', entity: 'plot' },
  { id: 'location', label: 'companion-ui-filter-location', kind: 'location', field: 'location', entity: 'location' },
  { id: 'plotpoint', label: 'companion-ui-plot-point', kind: 'plotpoint', field: 'plot-point', entity: 'plot-point' },
  { id: 'act', label: 'companion-ui-colour-act', kind: 'act' },
];

/** The colour kind of an entity group of the storyline, or null. */
const GROUP_KIND = { character: 'pov', plot: 'plot', location: 'location', 'plot-point': 'plotpoint' };

let store = KEY;
let current = 'todo';

function read() {
  try { return localStorage.getItem(store) || 'todo'; } catch (error) { return 'todo'; }
}

/** Keep the choice per project: the origin is shared by all of them. */
export function useProject(root) {
  store = `${KEY}:${root}`;
  current = read();
}

export const criterion = () => current;

export function setCriterion(id) {
  current = CRITERIA.some((entry) => entry.id === id) ? id : 'todo';
  try { localStorage.setItem(store, current); } catch (error) { /* private mode */ }
}

function actIndex(snapshot) {
  const index = new Map();
  for (const act of snapshot.acts) {
    if (!act.id) continue;
    for (const chapter of act.chapters) for (const scene of chapter.scenes) if (scene.id) index.set(scene.id, act.id);
  }
  return index;
}

const indexes = new WeakMap();
const actOf = (snapshot, scene) => {
  if (!indexes.has(snapshot)) indexes.set(snapshot, actIndex(snapshot));
  return indexes.get(snapshot).get(scene.id) || null;
};

/** The criteria this project can use now: no plot points in a short story, no acts without IDs. */
export function available(ctx) {
  return CRITERIA.filter((entry) => {
    if (entry.id === 'plotpoint') return hasPlotPoints(ctx);
    if (entry.id === 'act') return ctx.snapshot.acts.some((act) => act.id);
    return true;
  });
}

const spec = (id = current) => CRITERIA.find((entry) => entry.id === id);

/** The value IDs a scene has for the criterion. */
function valuesOf(scene, entry, snapshot) {
  if (entry.id === 'act') { const act = actOf(snapshot, scene); return act ? [act] : []; }
  return ids(scene, entry.field);
}

/** The CSS colours of a scene for the current criterion; empty when it has none. */
export function sceneColors(scene, snapshot) {
  const entry = spec();
  if (!entry || !entry.kind) return [];
  return valuesOf(scene, entry, snapshot).map((id) => colorOf(entry.kind, id)).filter(Boolean);
}

function gradient(colors, direction) {
  const shown = colors.slice(0, MAX_BANDS);
  if (shown.length === 1) return `linear-gradient(${shown[0]}, ${shown[0]})`;
  const step = 100 / shown.length;
  const stops = shown.map((color, index) => `${color} ${index * step}% ${(index + 1) * step}%`);
  return `linear-gradient(${direction}, ${stops.join(', ')})`;
}

function tint(colors) {
  if (colors.length === 1) return `color-mix(in srgb, ${colors[0]} 13%, transparent)`;
  if (colors.length > 1) return 'color-mix(in srgb, var(--muted) 10%, transparent)';
  return 'transparent';
}

/**
 * Colour NODE for SCENE by the current criterion: add the class `crit` and the
 * two variables.  DIRECTION is the way the bands run (`to bottom` for an edge
 * on the left, `to right` for one along the bottom).  Does nothing, and
 * returns false, when the criterion is the TODO state.
 */
export function paint(node, scene, ctx, direction = 'to bottom') {
  if (current === 'todo') return false;
  const colors = sceneColors(scene, ctx.snapshot);
  node.classList.add('crit');
  node.style.setProperty('--crit-edge', colors.length ? gradient(colors, direction) : 'linear-gradient(var(--line), var(--line))');
  node.style.setProperty('--crit-tint', tint(colors));
  return true;
}

/**
 * The colour a storyline mark takes: the row's own entity when the criterion
 * is the kind of that row (plot thread rows by plot colour, and so on), else
 * the scene's colour when it has one value, else a neutral grey (several
 * values or none).  Null on the TODO criterion: the page's accent.
 */
export function markColor(scene, group, entity, ctx) {
  const entry = spec();
  if (!entry || !entry.kind) return null;
  if (GROUP_KIND[group.kind] === entry.kind) return colorOf(entry.kind, entity.id);
  const colors = sceneColors(scene, ctx.snapshot);
  return colors.length === 1 ? colors[0] : 'var(--muted)';
}

// ---- the picker and the legend ----------------------------------------------

/** The "Colour by" select for the header. */
export function chooser(ctx, onChange) {
  const wrap = el('label', 'filter colour-by');
  wrap.append(el('span', '', ctx.t('companion-ui-colour-by')));
  const select = el('select');
  const choices = available(ctx);
  if (!choices.some((entry) => entry.id === current)) current = 'todo';
  for (const entry of choices) {
    const option = el('option', '', ctx.t(entry.label));
    option.value = entry.id;
    select.append(option);
  }
  select.value = current;
  select.addEventListener('change', () => { setCriterion(select.value); onChange(); });
  wrap.append(select);
  return wrap;
}

/** The values of the criterion in use by the scenes, each with a swatch that opens its picker; null for TODO. */
export function legend(ctx) {
  const entry = spec();
  if (!entry || !entry.kind) return null;
  const { snapshot } = ctx;
  const used = new Set();
  for (const act of snapshot.acts) {
    for (const chapter of act.chapters) {
      for (const scene of chapter.scenes) for (const id of valuesOf(scene, entry, snapshot)) used.add(id);
    }
  }
  const named = entry.id === 'act'
    ? snapshot.acts.filter((act) => act.id).map((act) => ({ id: act.id, name: act.title }))
    : (snapshot.entities[entry.entity] || []).filter((entity) => entity.id);
  const box = el('div', 'colour-legend');
  box.append(el('strong', '', ctx.t('companion-ui-colour-legend', ctx.t(entry.label))));
  let count = 0;
  for (const item of named) {
    if (!used.has(item.id) || !colorOf(entry.kind, item.id)) continue;
    const key = el('span', 'rel-key');
    key.append(legendSwatch({ kind: entry.kind, value: item.id, label: item.name }, 'dot', ctx), document.createTextNode(item.name));
    box.append(key);
    count += 1;
  }
  return count ? box : null;
}
