// Entity forms: the entities of the design notebook by kind on the left, the
// selected one's form on the right.  A form has property fields (the heading's
// property drawer; a dropdown where the templates offer values) and section
// fields (the body of the entity and of each sub-heading, as raw org text).
// Saving a section sends the hash of the text the form loaded; if Emacs has
// changed it meanwhile the answer is a conflict and both versions are shown.
// Plot points are method prose and stay read-only.
// The "+" of a group opens a short form (name, and the role or type) that
// files the new entity from its capture template in Emacs (create-entity)
// and opens its form; "In Emacs…" runs the full capture there instead.
// "Appears in" is a strip of the book, one tick per scene.  A field Emacs
// holds unsaved (the buffer differs from the file) has a dot.
// All text is put in with textContent.

import { el } from '../edit.js';
import { hasPlotPoints, ids } from './matrix.js';

// property: the one the short "new" form asks for besides the name
const KINDS = [
  { kind: 'character', title: 'companion-ui-group-characters', create: 'companion-ui-new-character', property: 'Role' },
  { kind: 'location', title: 'companion-ui-group-locations', create: 'companion-ui-new-location', property: 'Type' },
  { kind: 'object', title: 'companion-ui-group-objects', create: 'companion-ui-new-object', property: 'Type' },
  { kind: 'plot', title: 'companion-ui-group-plots', create: 'companion-ui-new-plot', property: 'THREAD-TYPE' },
  { kind: 'plot-point', title: 'companion-ui-group-plot-points', create: null },
];
// the scene fields that place an entity of a kind in a scene
const SCENE_FIELDS = {
  character: ['pov', 'characters'], location: ['location'], object: ['objects'],
  plot: ['plot'], 'plot-point': ['plot-point'],
};
const HIDDEN_PROPERTIES = ['id', 'relationshipsdata'];

let selected = null;      // entity ID
let loaded = null;        // { id, revision, data }
let conflict = null;      // { id, index, mine, current, hash, title }
let creating = null;      // the kind whose short "new" form is open
let draft = { name: '', value: '' };

function byWeight(a, b) {
  // hidden entities (negative weight) go last, not first
  return (Boolean(a.hidden) - Boolean(b.hidden)) || (a.weight - b.weight) || a.name.localeCompare(b.name);
}

// ---- list ------------------------------------------------------------------

function list(ctx) {
  const side = el('nav', 'ent-list');
  for (const { kind, title, create } of KINDS) {
    if (kind === 'plot-point' && !hasPlotPoints(ctx)) continue;
    const group = el('section', 'ent-group');
    const head = el('div', 'ent-group-head');
    head.append(el('h3', '', ctx.t(title)));
    if (create) {
      const add = el('button', 'add-scene', '+');
      add.type = 'button';
      add.title = ctx.t(create);
      add.setAttribute('aria-label', ctx.t(create));
      add.setAttribute('aria-expanded', String(creating === kind));
      add.addEventListener('click', () => {
        creating = creating === kind ? null : kind;
        draft = { name: '', value: '' };
        ctx.rerender();
      });
      head.append(add);
    }
    group.append(head);
    if (creating === kind) group.append(createForm(KINDS.find((entry) => entry.kind === kind), ctx));
    const entities = [...(ctx.snapshot.entities[kind] || [])].filter((e) => e.id);
    if (kind !== 'plot-point') entities.sort(byWeight);
    for (const entity of entities) {
      const item = el('button', 'ent-item', entity.name);
      item.type = 'button';
      item.dataset.id = entity.id;
      if (entity.hidden) item.classList.add('hidden-entity');
      if (entity.id === selected) item.classList.add('selected');
      item.addEventListener('click', () => { selected = entity.id; conflict = null; ctx.rerender(); });
      group.append(item);
    }
    side.append(group);
  }
  return side;
}

/** The short "new" form of a group: a name and its role or type. */
function createForm({ kind, create, property }, ctx) {
  const box = el('form', 'ent-create');
  box.setAttribute('aria-label', ctx.t(create));
  const name = el('input');
  name.type = 'text';
  name.required = true;
  name.value = draft.name;
  name.placeholder = ctx.t('companion-ui-create-name');
  name.setAttribute('aria-label', ctx.t('companion-ui-create-name'));
  name.addEventListener('input', () => { draft.name = name.value; });
  const value = el('input');
  value.type = 'text';
  value.value = draft.value;
  value.placeholder = property;
  value.setAttribute('aria-label', property);
  value.addEventListener('input', () => { draft.value = value.value; });
  const offered = ((ctx.snapshot.entityChoices || {})[kind] || {})[property] || [];
  if (offered.length) {
    const list = el('datalist');
    list.id = `ent-create-${kind}`;
    for (const choice of offered) {
      const option = el('option');
      option.value = choice;
      list.append(option);
    }
    value.setAttribute('list', list.id);
    box.append(list);
  }
  const ok = el('button', 'primary', ctx.t('companion-ui-create'));
  ok.type = 'submit';
  const full = el('button', '', ctx.t('companion-ui-create-in-emacs'));
  full.type = 'button';
  full.title = ctx.t('companion-ui-create-in-emacs-hint');
  const cancel = el('button', '', ctx.t('companion-ui-relations-close'));
  cancel.type = 'button';
  const close = () => { creating = null; ctx.editEnded(); ctx.rerender(); };
  cancel.addEventListener('click', close);
  full.addEventListener('click', () => { creating = null; ctx.editEnded(); ctx.edit('capture-entity', { kind }); });
  box.addEventListener('keydown', (event) => { if (event.key === 'Escape') { event.preventDefault(); close(); } });
  box.addEventListener('focusin', () => ctx.editStarted());
  box.addEventListener('focusout', (event) => { if (!box.contains(event.relatedTarget)) ctx.editEnded(); });
  box.addEventListener('submit', async (event) => {
    event.preventDefault();
    if (!name.value.trim()) { name.focus(); return; }
    ok.disabled = true;
    ctx.editEnded();
    const answer = await ctx.edit('create-entity', { kind, name: name.value, value: value.value });
    ok.disabled = false;
    if (answer.ok) {
      creating = null;
      draft = { name: '', value: '' };
      selected = answer.id;
      conflict = null;
      loaded = null;
      ctx.notice(ctx.t('companion-ui-created', name.value.trim()));
      ctx.rerender();
    }
  });
  box.append(name, value, ok, full, cancel);
  queueMicrotask(() => { if (!box.contains(document.activeElement)) name.focus(); });
  return box;
}

// ---- form pieces -----------------------------------------------------------

/** The dot on a field Emacs holds unsaved. */
function unsavedDot(field, ctx) {
  if (field.unsaved !== true) return null;
  const dot = el('span', 'unsaved-dot', '●');
  dot.title = ctx.t('companion-ui-unsaved-field');
  dot.setAttribute('aria-label', dot.title);
  return dot;
}

function propertyField(entity, property, ctx) {
  const row = el('label', 'ent-prop');
  const label = el('span', 'ent-label', property.name);
  const dot = unsavedDot(property, ctx);
  if (dot) label.append(dot);
  row.append(label);
  const choices = entity.choices[property.name];
  let input;
  if (choices && choices.length) {
    input = el('select');
    const values = choices.includes(property.value) ? choices : [property.value, ...choices];
    for (const value of ['', ...values]) {
      const option = el('option', '', value);
      option.value = value;
      input.append(option);
    }
    input.value = property.value;
    input.addEventListener('change', () => save());
  } else {
    input = el('input');
    input.type = 'text';
    input.value = property.value;
    input.addEventListener('keydown', (event) => {
      if (event.key === 'Enter') { event.preventDefault(); input.blur(); }
      if (event.key === 'Escape') { input.value = property.value; input.blur(); }
    });
  }
  const save = () => {
    if (input.value === property.value) return;
    const value = input.value;
    ctx.edit('set-entity-property', { id: entity.id, key: property.name, value })
      .then((answer) => { if (answer.ok) property.value = value; });
  };
  input.addEventListener('focus', () => ctx.editStarted());
  input.addEventListener('blur', () => { ctx.editEnded(); if (input.tagName !== 'SELECT') save(); });
  row.append(input);
  return row;
}

function sectionTitle(section, entity, ctx) {
  if (section.index === 0) return ctx.t('companion-ui-entity-intro');
  return section.title;
}

function sectionField(entity, section, ctx) {
  const wrap = el('div', 'ent-section');
  wrap.style.setProperty('--depth', String(Math.max(0, section.level - 3)));
  const label = el('label');
  const name = el('span', 'ent-label', sectionTitle(section, entity, ctx));
  const dot = unsavedDot(section, ctx);
  if (dot) name.append(dot);
  label.append(name);
  const area = el('textarea', 'ent-body');
  area.value = section.body;
  area.rows = Math.min(14, Math.max(3, section.body.split('\n').length + 1));
  area.spellcheck = true;
  label.append(area);
  wrap.append(label);
  const save = async () => {
    if (area.value === section.body) return;
    const answer = await ctx.edit('set-entity-section', {
      id: entity.id, index: section.index, title: section.title,
      hash: section.hash, body: area.value,
    }, null, false, ['conflict']);
    if (answer.ok) {
      section.body = answer.body;
      section.hash = answer.hash;
    } else if (answer.error.code === 'conflict') {
      conflict = {
        id: entity.id, index: section.index, title: section.title, mine: area.value,
        current: answer.error.current, hash: answer.error.hash || null,
      };
      ctx.rerender();
    }
  };
  area.addEventListener('focus', () => ctx.editStarted());
  area.addEventListener('blur', () => { ctx.editEnded(); save(); });
  area.addEventListener('keydown', (event) => { if (event.key === 'Escape') { area.value = section.body; area.blur(); } });
  if (conflict && conflict.id === entity.id && conflict.index === section.index) {
    wrap.append(conflictPanel(entity, ctx));
  }
  return wrap;
}

function conflictPanel(entity, ctx) {
  const panel = el('div', 'ent-conflict');
  panel.setAttribute('role', 'alert');
  panel.append(el('strong', '', ctx.t('companion-ui-conflict-title')));
  const columns = el('div', 'ent-conflict-columns');
  for (const [key, text] of [['companion-ui-conflict-mine', conflict.mine], ['companion-ui-conflict-theirs', conflict.current]]) {
    const column = el('div');
    column.append(el('span', 'ent-label', ctx.t(key)));
    const area = el('textarea', 'ent-body');
    area.readOnly = true;
    area.rows = 6;
    area.value = text === null ? '' : text;
    column.append(area);
    columns.append(column);
  }
  const actions = el('div', 'ent-conflict-actions');
  const keep = el('button', 'primary', ctx.t('companion-ui-keep-mine'));
  keep.type = 'button';
  keep.disabled = conflict.hash === null;
  keep.addEventListener('click', async () => {
    const mine = conflict;
    conflict = null;
    const answer = await ctx.edit('set-entity-section', {
      id: mine.id, index: mine.index, title: mine.title, hash: mine.hash, body: mine.mine,
    }, null, false, ['conflict']);
    if (!answer.ok && answer.error.code === 'conflict') {
      conflict = { ...mine, current: answer.error.current, hash: answer.error.hash || null };
      ctx.rerender();
    }
  });
  const theirs = el('button', '', ctx.t('companion-ui-use-theirs'));
  theirs.type = 'button';
  theirs.addEventListener('click', () => { conflict = null; loaded = null; ctx.rerender(); });
  actions.append(keep, theirs);
  panel.append(columns, actions);
  return panel;
}

function relationships(entity, ctx) {
  const box = el('section', 'ent-block');
  box.append(el('h4', '', ctx.t('companion-ui-relationships')));
  const ul = el('ul', 'ent-links');
  for (const link of entity.relationships) {
    const li = el('li');
    const open = el('button', 'sl-open', link.name);
    open.type = 'button';
    open.addEventListener('click', () => { selected = link.id; conflict = null; ctx.rerender(); });
    li.append(open);
    if (link.detail) li.append(el('span', 'ent-detail', ` ${link.detail}`));
    ul.append(li);
  }
  box.append(ul);
  return box;
}

/** The book as a strip, one tick per scene, the entity's scenes filled in:
 * the storyline row of one entity.  Chapters are set apart by a gap. */
function strip(entity, ctx) {
  const fields = SCENE_FIELDS[entity.kind] || [];
  const box = el('div', 'ent-strip');
  box.setAttribute('role', 'group');
  box.setAttribute('aria-label', ctx.t('companion-ui-appears-in'));
  let number = 0;
  let found = 0;
  for (const act of ctx.snapshot.acts) {
    for (const chapter of act.chapters) {
      if (!chapter.scenes.length) continue;
      const group = el('span', 'ent-strip-chapter');
      group.title = chapter.title || act.title || '';
      for (const scene of chapter.scenes) {
        number += 1;
        const here = fields.some((field) => ids(scene, field).includes(entity.id));
        const label = `${number}. ${scene.title}`;
        let tick;
        if (here && scene.id) {
          found += 1;
          tick = el('button', 'ent-tick on');
          tick.type = 'button';
          tick.addEventListener('click', () => ctx.goto(scene.id));
        } else {
          tick = el('span', here ? 'ent-tick on' : 'ent-tick');
          if (here) found += 1;
        }
        if (entity.kind === 'character' && ids(scene, 'pov').includes(entity.id)) {
          tick.classList.add('pov');
          tick.title = `${label} (${ctx.t('companion-ui-pov')})`;
        } else {
          tick.title = label;
        }
        tick.setAttribute('aria-label', tick.title);
        group.append(tick);
      }
      box.append(group);
    }
  }
  const wrap = el('div', 'ent-strip-wrap');
  wrap.append(box, el('span', 'legend', ctx.t('companion-ui-appears-count', found, number)));
  return wrap;
}

function appearances(entity, ctx) {
  const box = el('section', 'ent-block');
  box.append(el('h4', '', ctx.t('companion-ui-appears-in')));
  if (!entity.appearsIn.length) {
    box.append(el('p', 'empty', ctx.t('companion-ui-no-appearances')));
    return box;
  }
  const ul = el('ul', 'ent-links');
  for (const scene of entity.appearsIn) {
    const li = el('li');
    const open = el('button', 'sl-open', scene.title);
    open.type = 'button';
    open.addEventListener('click', () => ctx.goto(scene.id));
    li.append(open);
    ul.append(li);
  }
  box.append(ul);
  return box;
}

function form(entity, ctx) {
  const root = el('article', 'ent-form');
  root.dataset.id = entity.id;
  const head = el('header', 'ent-head');
  head.append(el('h2', '', entity.name));
  const open = el('button', '', ctx.t('companion-ui-open'));
  open.type = 'button';
  open.addEventListener('click', () => ctx.goto(entity.id));
  head.append(open);
  root.append(head);
  // where it is in the book, at a glance, before the long fields
  root.append(strip(entity, ctx));

  if (entity.readOnly) {
    root.append(el('p', 'legend', ctx.t('companion-ui-readonly-entity')));
    for (const section of entity.sections) {
      if (!section.body.trim()) continue;
      const block = el('section', 'ent-block');
      block.append(el('h4', '', section.index === 0 ? ctx.t('companion-ui-entity-intro') : section.title));
      block.append(el('pre', 'ent-prose', section.body.trim()));
      root.append(block);
    }
    root.append(appearances(entity, ctx));
    return root;
  }

  const props = el('section', 'ent-block ent-props');
  props.append(el('h4', '', ctx.t('companion-ui-properties')));
  for (const property of entity.properties) {
    if (!HIDDEN_PROPERTIES.includes(property.name.toLowerCase())) props.append(propertyField(entity, property, ctx));
  }
  root.append(props);
  if (entity.relationships.length) root.append(relationships(entity, ctx));
  const sections = el('section', 'ent-block');
  for (const section of entity.sections) sections.append(sectionField(entity, section, ctx));
  root.append(sections);
  root.append(appearances(entity, ctx));
  return root;
}

// ---- view ------------------------------------------------------------------

async function show(pane, ctx) {
  if (!selected) { pane.replaceChildren(el('p', 'empty', ctx.t('companion-ui-select-entity'))); return; }
  const revision = ctx.snapshot.revision;
  try {
    if (!loaded || loaded.id !== selected || loaded.revision !== revision) {
      const data = await ctx.fetchEntity(selected);
      if (!data.ok) throw new Error(data.error.message);
      loaded = { id: selected, revision, data };
    }
  } catch (error) {
    selected = null;
    pane.replaceChildren(el('p', 'empty', ctx.t('companion-ui-entity-error')));
    return;
  }
  pane.replaceChildren(form(loaded.data, ctx));
}

export function render(container, ctx) {
  container.replaceChildren();
  // the selected entity went away (deleted in Emacs, or a creation undone)
  if (selected && !Object.values(ctx.snapshot.entities).some((list) => list.some((entity) => entity.id === selected))) {
    selected = null;
    loaded = null;
    conflict = null;
  }
  const layout = el('div', 'ent-layout');
  const pane = el('div', 'ent-pane');
  layout.append(list(ctx), pane);
  container.append(layout);
  show(pane, ctx);
}
