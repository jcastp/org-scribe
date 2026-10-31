// Edit markers (*EDIT* and *NOTE* in comment blocks): the pieces the views
// share.  A scene's own markers come with the snapshot (scene.edits: type,
// category, text, line), read by Emacs with the edit index's parser (F8 6).
// The corkboard and the table show a badge, the detail panel lists them, the
// Edits tab lists every marker of the project (GET /api/edits).  Reading
// only: a marker is changed in Emacs, where its prose is.

import { el } from './edit.js';

/** { edits, notes } for SCENE. */
export function counts(scene) {
  let edits = 0;
  let notes = 0;
  for (const marker of scene.edits || []) {
    if (marker.type === 'EDIT') edits += 1; else notes += 1;
  }
  return { edits, notes };
}

/** The badge for a scene with markers (a button opening its details), or null. */
export function badge(scene, ctx) {
  const { edits, notes } = counts(scene);
  if (!edits && !notes || !scene.id) return null;
  const node = el('button', edits ? 'edits-badge' : 'edits-badge notes-only', `✎ ${edits + notes}`);
  node.type = 'button';
  const label = ctx.t('companion-ui-edits-badge', edits, notes);
  node.title = label;
  node.setAttribute('aria-label', label);
  node.addEventListener('click', (event) => { event.stopPropagation(); ctx.openDetail(scene.id, 'edits'); });
  return node;
}

/** The name a marker's category takes in the project's language. */
export function categoryName(category, ctx) {
  if (!category) return ctx.t('companion-ui-edits-other');
  const known = (ctx.snapshot.editCategories || []).find((entry) => entry.key === category);
  return known ? known.name : category;
}

/** The tag before a marker: its category for an *EDIT*, "note" for a *NOTE*. */
export function tag(marker, ctx) {
  return marker.type === 'EDIT'
    ? el('span', `marker-tag edit cat-${marker.category || 'other'}`, categoryName(marker.category, ctx))
    : el('span', 'marker-tag note', ctx.t('companion-ui-edits-note'));
}

/**
 * One marker as a list item: tag, text, and a ↗ that shows its line in
 * Emacs (goto-marker).  FILE is the marker's file relative to the project,
 * or null for the manuscript.  EXTRA nodes (a place, a details button) go
 * after the tag; WITH_TAG false leaves the tag out (a list already grouped
 * by category).
 */
export function item(marker, file, ctx, extra = [], withTag = true) {
  const row = el('li', `marker ${marker.type === 'EDIT' ? 'is-edit' : 'is-note'}`);
  const body = el('div', 'marker-body');
  const text = el('p', 'marker-text', marker.text || '');
  body.append(text);
  const meta = el('div', 'marker-meta');
  if (withTag) meta.append(tag(marker, ctx));
  for (const node of extra) if (node) meta.append(node);
  body.append(meta);
  const go = el('button', 'open', '↗');
  go.type = 'button';
  const label = ctx.t('companion-ui-edits-goto');
  go.title = label;
  go.setAttribute('aria-label', label);
  go.addEventListener('click', (event) => {
    event.stopPropagation();
    ctx.gotoMarker(file, marker.line);
  });
  row.append(body, go);
  return row;
}
