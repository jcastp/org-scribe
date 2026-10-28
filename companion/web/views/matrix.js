// What the storyline and the chapters views share: the entity groups they
// list, how a scene's linked IDs are read, and the order of the rows.

export const GROUPS = [
  { kind: 'character', field: 'characters', title: 'companion-ui-group-characters' },
  { kind: 'plot', field: 'plot', title: 'companion-ui-group-plots' },
  { kind: 'location', field: 'location', title: 'companion-ui-group-locations' },
  { kind: 'object', field: 'objects', title: 'companion-ui-group-objects' },
  { kind: 'plot-point', field: 'plot-point', title: 'companion-ui-group-plot-points' },
];

/** True when the project has plot points: a short story has none, and a
 * novel whose writer deleted them has none either, so nothing is shown. */
export function hasPlotPoints(ctx) {
  return (ctx.snapshot.entities['plot-point'] || []).length > 0;
}

/** GROUPS without the plot points when the project has none. */
export function visibleGroups(ctx) {
  return GROUPS.filter((group) => group.kind !== 'plot-point' || hasPlotPoints(ctx));
}

/** The IDs a scene links in FIELD. */
export function ids(scene, field) {
  const value = scene.fields[field];
  return value && value.links ? value.links.map((link) => link.id) : [];
}

/** Row order: hidden entities (negative weight) go last, not first. */
export function byWeight(a, b) {
  return (Boolean(a.hidden) - Boolean(b.hidden)) || (a.weight - b.weight) || a.name.localeCompare(b.name);
}

/** The group's entities in display order; the plot points keep the method's
 * order, since weights and names would give 1, 10, 11… */
export function orderEntities(group, entities) {
  return group.kind === 'plot-point' ? entities : [...entities].sort(byWeight);
}
