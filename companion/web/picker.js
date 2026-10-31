// The link picker: the entities of one kind, with a search box, for a scene's
// link field.  Shared by the table's cells and the scene detail panel.
// All text is put in with textContent.

import { el } from './edit.js';

/**
 * Open the picker for the link FIELD of SCENE under ANCHOR (the element it
 * hangs from, focused again on Escape).  Done, or a click outside, writes the
 * choice with set-field; Escape drops it.  CTX needs t, snapshot, edit,
 * editStarted and editEnded.
 */
export function openPicker(anchor, scene, field, ctx) {
  const entities = (ctx.snapshot.entities[field.entity] || []).filter((entity) => entity.id);
  const current = scene.fields[field.key] || {};
  const chosen = new Set((current.links || []).map((link) => link.id));
  const single = field.kind === 'link-one';

  const panel = el('div', 'picker');
  panel.setAttribute('role', 'dialog');
  panel.setAttribute('aria-label', field.name);
  const search = el('input', 'picker-search');
  search.type = 'search';
  search.placeholder = ctx.t('companion-ui-picker-search');
  const list = el('div', 'picker-list');
  const rows = entities.map((entity) => {
    const label = el('label', 'picker-item');
    const box = el('input');
    box.type = single ? 'radio' : 'checkbox';
    box.name = 'picker';
    box.checked = chosen.has(entity.id);
    box.addEventListener('change', () => {
      if (single) chosen.clear();
      if (box.checked) chosen.add(entity.id); else chosen.delete(entity.id);
    });
    label.append(box, el('span', entity.hidden ? 'hidden-entity' : '', entity.name));
    list.append(label);
    return { label, name: entity.name.toLowerCase() };
  });
  search.addEventListener('input', () => {
    const needle = search.value.toLowerCase();
    for (const row of rows) row.label.hidden = !row.name.includes(needle);
  });
  const actions = el('div', 'picker-actions');
  const clear = el('button', '', ctx.t('companion-ui-picker-clear'));
  clear.type = 'button';
  clear.addEventListener('click', () => {
    chosen.clear();
    for (const input of list.querySelectorAll('input')) input.checked = false;
  });
  const done = el('button', 'primary', ctx.t('companion-ui-picker-done'));
  done.type = 'button';
  actions.append(clear, done);
  panel.append(search, list, actions);

  let closed = false;
  const close = (apply) => {
    if (closed) return;
    closed = true;
    document.removeEventListener('mousedown', outside, true);
    panel.remove();
    ctx.editEnded();
    if (!apply) return;
    // the writer's own order stays; ids added now follow, in entity order
    // (ids not in the list, hidden by a filter or dangling, stay where they were)
    const before = (current.links || []).map((link) => link.id);
    const ids = before.filter((id) => chosen.has(id));
    for (const entity of entities) if (chosen.has(entity.id) && !ids.includes(entity.id)) ids.push(entity.id);
    if (ids.join() === before.join()) return;
    ctx.edit('set-field', {
      id: scene.id, key: field.key, links: ids, unlinked: current.unlinked || [],
    }, scene.id);
  };
  const outside = (event) => { if (!panel.contains(event.target)) close(true); };
  done.addEventListener('click', () => close(true));
  panel.addEventListener('keydown', (event) => {
    if (event.key === 'Escape') { event.preventDefault(); close(false); anchor.focus(); }
  });

  ctx.editStarted();
  document.body.append(panel);
  const box = anchor.getBoundingClientRect();
  panel.style.top = `${Math.min(box.bottom + window.scrollY, window.scrollY + window.innerHeight - 320)}px`;
  panel.style.left = `${Math.max(8, Math.min(box.left + window.scrollX, window.innerWidth - 300))}px`;
  document.addEventListener('mousedown', outside, true);
  search.focus();
}
