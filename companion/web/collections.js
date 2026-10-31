// Collections: named sets of the scene filters, saved in the project by Emacs
// (set-collection writes a "# Companion-collection:" line).  Applying one only
// fills the filters, in this browser; saving, renaming and deleting go through
// Emacs.  A collection that names something deleted since is marked, and
// applying it leaves that part out; the saved line is never emptied.
// All text is put in with textContent.

import { el, startEdit } from './edit.js';
import { isActive, sameFilters, filtersFrom, isStale } from './filters.js';

/** The collection the current filters show: the one they equal, or the one last
 * applied while the filters have not changed since (a stale collection never
 * equals its own pruned filters). */
function selectedCollection(ctx) {
  const { collections } = ctx.snapshot;
  const equal = collections.find((collection) => sameFilters(filtersFrom(collection.filters), ctx.filters));
  if (equal) return equal;
  const applied = ctx.applied();
  if (applied && sameFilters(applied.filters, ctx.filters)) {
    return collections.find((collection) => collection.name === applied.name) || null;
  }
  return null;
}

function button(text, onClick) {
  const node = el('button', 'collection-action', text);
  node.type = 'button';
  node.addEventListener('click', onClick);
  return node;
}

/** Replace NODE with a one-line editor for a name; COMMIT gets the new name. */
function askName(node, initial, commit, ctx) {
  startEdit(node, initial, false, (name) => {
    if (name.trim()) commit(name.trim());
  }, ctx, ctx.t('companion-ui-collection-name'));
}

/** The collection controls, for the start of the filter bar. */
export function render(ctx) {
  const wrap = el('div', 'collections');
  const current = selectedCollection(ctx);

  // no collection yet: only "Save as…" (once a filter is on) is worth showing
  const { length } = ctx.snapshot.collections;
  if (!length && !isActive(ctx.filters)) {
    wrap.hidden = true;
    return wrap;
  }
  const label = el('label', 'filter');
  label.append(el('span', '', ctx.t('companion-ui-collection')));
  const select = el('select');
  const none = el('option', '', ctx.t('companion-ui-collection-pick'));
  none.value = '';
  select.append(none);
  ctx.snapshot.collections.forEach((collection, index) => {
    const stale = isStale(collection.filters, ctx.snapshot);
    const option = el('option', '', stale
      ? ctx.t('companion-ui-collection-stale-name', collection.name)
      : collection.name);
    option.value = String(index);
    select.append(option);
  });
  select.value = current ? String(ctx.snapshot.collections.indexOf(current)) : '';
  select.addEventListener('change', () => {
    const collection = ctx.snapshot.collections[Number(select.value)];
    if (select.value !== '' && collection) ctx.apply(collection);
    else ctx.forget();
  });
  label.append(select);
  if (length) wrap.append(label);

  if (current) {
    const rename = button(ctx.t('companion-ui-collection-rename'), () => {
      askName(rename, current.name, (name) => {
        if (name !== current.name) ctx.save({ old: current.name, name });
      }, ctx);
    });
    const remove = button(ctx.t('companion-ui-collection-delete'), () => {
      if (window.confirm(ctx.t('companion-ui-collection-delete-confirm', current.name))) {
        ctx.save({ old: current.name, delete: true });
      }
    });
    wrap.append(rename, remove);
  } else if (isActive(ctx.filters)) {
    const applied = ctx.applied();
    const base = applied && ctx.snapshot.collections.find((collection) => collection.name === applied.name);
    if (base) {
      // the filters were changed after applying BASE: offer to store them in it
      wrap.append(button(ctx.t('companion-ui-collection-update', base.name), () => {
        ctx.save({ old: base.name, name: base.name, filters: { ...ctx.filters } });
      }));
    }
    const save = button(ctx.t('companion-ui-collection-save'), () => {
      askName(save, '', (name) => ctx.save({ name, filters: { ...ctx.filters } }), ctx);
    });
    wrap.append(save);
  }
  return wrap;
}
