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
    return { label, box, name: entity.name.toLowerCase() };
  });
  search.addEventListener('input', () => {
    const needle = search.value.toLowerCase();
    for (const row of rows) row.label.hidden = !row.name.includes(needle);
  });
  // Enter in the search box ticks the first match and clears the search; on
  // an empty box it is Done.  Without it only the mouse, or tabbing through
  // every box, could pick anything.
  search.addEventListener('keydown', (event) => {
    if (event.key !== 'Enter') return;
    event.preventDefault();
    const needle = search.value.trim();
    if (!needle) { close(true); return; }
    const first = rows.find((row) => !row.label.hidden);
    if (!first) return;
    first.box.checked = !first.box.checked;
    first.box.dispatchEvent(new Event('change'));
    search.value = '';
    for (const row of rows) row.label.hidden = false;
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
    // sent as a change to the field as Emacs has it now, not as a new list
    const before = (current.links || []).map((link) => link.id);
    const remove = before.filter((id) => !chosen.has(id));
    const add = entities.map((entity) => entity.id).filter((id) => chosen.has(id) && !before.includes(id));
    if (!add.length && !remove.length) return;
    ctx.edit('set-field', { id: scene.id, key: field.key, add, remove }, scene.id);
  };
  const outside = (event) => { if (!panel.contains(event.target)) close(true); };
  // tabbing out of the picker leaves it too; it used to stay open, holding the page
  panel.addEventListener('focusout', (event) => {
    if (event.relatedTarget && !panel.contains(event.relatedTarget)) close(true);
  });
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
