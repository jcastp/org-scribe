// Entity forms: the entities of the design notebook by kind on the left, the
// selected one's form on the right.  A form has property fields (the heading's
// property drawer; a dropdown where the templates offer values) and section
// fields (the body of the entity and of each sub-heading, as raw org text).
// Saving a section sends the hash of the text the form loaded; if Emacs has
// changed it meanwhile the answer is a conflict and both versions are shown.
// Plot points are method prose and stay read-only.
// All text is put in with textContent.

import { el } from '../edit.js';
import { hasPlotPoints } from './matrix.js';

const KINDS = [
  { kind: 'character', title: 'companion-ui-group-characters', create: 'companion-ui-new-character' },
  { kind: 'location', title: 'companion-ui-group-locations', create: 'companion-ui-new-location' },
  { kind: 'object', title: 'companion-ui-group-objects', create: 'companion-ui-new-object' },
  { kind: 'plot', title: 'companion-ui-group-plots', create: 'companion-ui-new-plot' },
  { kind: 'plot-point', title: 'companion-ui-group-plot-points', create: null },
];
const HIDDEN_PROPERTIES = ['id', 'relationshipsdata'];

let selected = null;      // entity ID
let loaded = null;        // { id, revision, data }
let conflict = null;      // { id, index, mine, current, hash, title }

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
      add.addEventListener('click', () => ctx.edit('capture-entity', { kind }));
      head.append(add);
    }
    group.append(head);
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

// ---- form pieces -----------------------------------------------------------

function propertyField(entity, property, ctx) {
  const row = el('label', 'ent-prop');
  row.append(el('span', 'ent-label', property.name));
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
  label.append(el('span', 'ent-label', sectionTitle(section, entity, ctx)));
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
  const layout = el('div', 'ent-layout');
  const pane = el('div', 'ent-pane');
  layout.append(list(ctx), pane);
  container.append(layout);
  show(pane, ctx);
}
