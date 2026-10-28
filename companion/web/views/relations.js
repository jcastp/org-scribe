// Relations: the characters as nodes and their relationships as typed lines,
// drawn from the notebook's RelationshipsData, so it cannot drift from it.
// A relationship is directed (A to B is stored on A).  If A to B and B to A
// have the same type they are drawn as one plain line; otherwise each is an
// arrow.  The layout is automatic (a small force simulation, deterministic:
// the same data gives the same picture) and positions are kept in memory only,
// so an edit does not reshuffle the graph.  Click a character, then another,
// to add a relationship; click a line to change or remove its entries; double-
// click a character to open it in Emacs.  Node colour is the character's role,
// size the number of scenes it is in.  A table under the graph holds the same
// data.  Every change is a set-relationship command to Emacs.
// All text is put in with textContent.

import { colorOf, rankOf, legendSwatch } from '../colors.js';
import { el } from '../edit.js';
import { ids } from './matrix.js';

const SVG = 'http://www.w3.org/2000/svg';
const WIDTH = 900;
const HEIGHT = 600;

let showHidden = false;
let selected = null;          // character ID, or null
let panel = null;             // { kind: 'add', source, target } | { kind: 'pair', a, b }
let panelHeld = false;        // whether the app is holding re-renders for an open form
const positions = new Map();  // ID -> { x, y }, kept across renders of one project
let positionsRoot = null;

function svg(tag, attributes = {}, text) {
  const node = document.createElementNS(SVG, tag);
  for (const [name, value] of Object.entries(attributes)) node.setAttribute(name, String(value));
  if (text !== undefined) node.textContent = text;
  return node;
}

// Colour comes from Emacs (theme, pins, distinct palette slots).  A second cue
// that does not depend on colour: a role also has a node shape and a type a
// line pattern, by its position among all of them.
const FALLBACK = 'var(--todo-default)';
const typeColour = (type) => colorOf('reltype', type) || FALLBACK;
const roleColour = (role) => (role ? colorOf('role', role) || FALLBACK : FALLBACK);
const DASHES = ['', '9 4', '2 4', '10 4 2 4', '5 3', '1 3'];
const dashOf = (type) => DASHES[rankOf('reltype', type) % DASHES.length];
const SHAPES = ['circle', 'square', 'diamond', 'triangle', 'hexagon', 'triangle-down'];
const shapeOf = (role) => (role ? SHAPES[1 + (rankOf('role', role) % (SHAPES.length - 1))] : 'circle');

/** The outline of SHAPE around the origin, fitting a circle of radius R. */
function shapePath(shape, r) {
  const points = {
    square: [[-0.85, -0.85], [0.85, -0.85], [0.85, 0.85], [-0.85, 0.85]],
    diamond: [[0, -1.15], [1.15, 0], [0, 1.15], [-1.15, 0]],
    triangle: [[0, -1.15], [1.05, 0.85], [-1.05, 0.85]],
    'triangle-down': [[0, 1.15], [1.05, -0.85], [-1.05, -0.85]],
    hexagon: [[1, 0], [0.5, 0.87], [-0.5, 0.87], [-1, 0], [-0.5, -0.87], [0.5, -0.87]],
  }[shape];
  if (!points) return `M ${-r} 0 a ${r} ${r} 0 1 0 ${2 * r} 0 a ${r} ${r} 0 1 0 ${-2 * r} 0 z`;
  return `${points.map(([x, y], index) => `${index ? 'L' : 'M'} ${(x * r).toFixed(1)} ${(y * r).toFixed(1)}`).join(' ')} z`;
}

// ---- the data -------------------------------------------------------------

/** Characters, the entries between them and the lines to draw. */
function buildModel(ctx) {
  const snapshot = ctx.snapshot;
  const all = (snapshot.entities.character || []).filter((entity) => entity.id);
  const byId = new Map(all.map((entity) => [entity.id, entity]));
  const shown = all.filter((entity) => showHidden || !entity.hidden);
  const shownIds = new Set(shown.map((entity) => entity.id));
  const hiddenNames = all.filter((entity) => !shownIds.has(entity.id)).map((entity) => entity.name);

  const counts = new Map();
  for (const act of snapshot.acts) {
    for (const chapter of act.chapters) {
      for (const scene of chapter.scenes) {
        const here = new Set([...ids(scene, 'pov'), ...ids(scene, 'characters')]);
        for (const id of here) counts.set(id, (counts.get(id) || 0) + 1);
      }
    }
  }

  const entries = [];
  const ghosts = new Map();
  for (const entity of shown) {
    for (const rel of entity.relationships || []) {
      if (!byId.has(rel.target)) ghosts.set(rel.target, rel.name || rel.target);
      else if (!shownIds.has(rel.target)) continue;
      entries.push({ source: entity.id, target: rel.target, type: rel.type });
    }
  }

  const linked = new Set();
  for (const entry of entries) { linked.add(entry.source); linked.add(entry.target); }
  const nodes = [
    ...shown.filter((entity) => linked.has(entity.id)).map((entity) => ({
      id: entity.id, name: entity.name, role: entity.role || '', ghost: false,
      r: Math.min(30, 10 + 4 * Math.sqrt(counts.get(entity.id) || 0)),
    })),
    ...[...ghosts].map(([id, name]) => ({ id, name, role: '', ghost: true, r: 12 })),
  ];
  const alone = shown.filter((entity) => !linked.has(entity.id));
  return { all, byId, shown, nodes, alone, entries, ghosts, hiddenNames, counts };
}

/** Group the entries by pair into lines: a type both ways is one plain line. */
function buildLinks(entries) {
  const pairs = new Map();
  for (const entry of entries) {
    const [a, b] = entry.source < entry.target ? [entry.source, entry.target] : [entry.target, entry.source];
    const key = `${a}\u0000${b}`;
    if (!pairs.has(key)) pairs.set(key, { a, b, forward: new Set(), backward: new Set() });
    // "Rival" and "rival" are one type (colours ignore case too)
    const side = entry.source === a ? pairs.get(key).forward : pairs.get(key).backward;
    if (![...side].some((type) => type.toLowerCase() === entry.type.toLowerCase())) side.add(entry.type);
  }
  const links = [];
  for (const pair of pairs.values()) {
    const group = [];
    const other = (set, type) => [...set].find((t) => t.toLowerCase() === type.toLowerCase());
    for (const type of pair.forward) {
      if (other(pair.backward, type)) group.push({ a: pair.a, b: pair.b, type, directed: false });
      else group.push({ a: pair.a, b: pair.b, type, directed: true, from: pair.a, to: pair.b });
    }
    for (const type of pair.backward) {
      if (!other(pair.forward, type)) group.push({ a: pair.a, b: pair.b, type, directed: true, from: pair.b, to: pair.a });
    }
    group.forEach((link, index) => { link.index = index; link.total = group.length; links.push(link); });
  }
  return links;
}

// ---- layout ---------------------------------------------------------------

/** Fruchterman-Reingold, deterministic.  Nodes in FIXED do not move. */
function layout(nodes, links, fixed) {
  const count = nodes.length;
  if (!count) return;
  const cx = WIDTH / 2;
  const cy = HEIGHT / 2;
  const free = nodes.filter((node) => !fixed.has(node.id));
  free.forEach((node, index) => {
    const neighbours = links
      .filter((link) => link.a === node.id || link.b === node.id)
      .map((link) => positions.get(link.a === node.id ? link.b : link.a))
      .filter(Boolean);
    if (neighbours.length) {
      const x = neighbours.reduce((sum, p) => sum + p.x, 0) / neighbours.length;
      const y = neighbours.reduce((sum, p) => sum + p.y, 0) / neighbours.length;
      const angle = index * 2.4;
      positions.set(node.id, { x: x + 70 * Math.cos(angle), y: y + 70 * Math.sin(angle) });
    } else {
      const angle = (2 * Math.PI * index) / free.length;
      const radius = 0.35 * Math.min(WIDTH, HEIGHT);
      positions.set(node.id, { x: cx + radius * Math.cos(angle), y: cy + radius * Math.sin(angle) });
    }
  });
  const k = 0.6 * Math.sqrt((WIDTH * HEIGHT) / count);
  const steps = 300;
  const freeIds = new Set(free.map((node) => node.id));
  for (let step = 0; step < steps; step++) {
    const temperature = (WIDTH / 8) * (1 - step / steps) + 1;
    const move = new Map(nodes.map((node) => [node.id, { x: 0, y: 0 }]));
    for (let i = 0; i < count; i++) {
      for (let j = i + 1; j < count; j++) {
        const p = positions.get(nodes[i].id);
        const q = positions.get(nodes[j].id);
        let dx = p.x - q.x;
        let dy = p.y - q.y;
        let distance = Math.hypot(dx, dy);
        if (distance < 0.01) { dx = 0.1 * (i + 1); dy = 0.1 * (j + 1); distance = Math.hypot(dx, dy); }
        // repulsion only within reach: summed over every node it would push a
        // cluster that has no line to the rest against the border
        if (distance > 3 * k) continue;
        const force = (k * k) / distance;
        move.get(nodes[i].id).x += (dx / distance) * force;
        move.get(nodes[i].id).y += (dy / distance) * force;
        move.get(nodes[j].id).x -= (dx / distance) * force;
        move.get(nodes[j].id).y -= (dy / distance) * force;
      }
    }
    for (const link of links) {
      if (link.index > 0) continue;
      const p = positions.get(link.a);
      const q = positions.get(link.b);
      const dx = p.x - q.x;
      const dy = p.y - q.y;
      const distance = Math.max(Math.hypot(dx, dy), 0.01);
      const force = (distance * distance) / k;
      move.get(link.a).x -= (dx / distance) * force;
      move.get(link.a).y -= (dy / distance) * force;
      move.get(link.b).x += (dx / distance) * force;
      move.get(link.b).y += (dy / distance) * force;
    }
    for (const node of nodes) {
      if (!freeIds.has(node.id)) continue;
      const p = positions.get(node.id);
      const m = move.get(node.id);
      // stronger than the textbook's: a cluster with no line to the rest
      // would otherwise be pushed against the border and its nodes pile up
      m.x += (cx - p.x) * 0.25;
      m.y += (cy - p.y) * 0.25;
      const length = Math.max(Math.hypot(m.x, m.y), 0.01);
      const limited = Math.min(length, temperature);
      p.x = Math.min(WIDTH - 40, Math.max(40, p.x + (m.x / length) * limited));
      p.y = Math.min(HEIGHT - 40, Math.max(40, p.y + (m.y / length) * limited));
    }
  }
}

// ---- geometry of one line ---------------------------------------------------

function geometry(link, radii) {
  const p = positions.get(link.a);
  const q = positions.get(link.b);
  const [start, end] = link.directed && link.from === link.b ? [q, p] : [p, q];
  const [rStart, rEnd] = link.directed && link.from === link.b
    ? [radii.get(link.b), radii.get(link.a)] : [radii.get(link.a), radii.get(link.b)];
  const dx = end.x - start.x;
  const dy = end.y - start.y;
  const length = Math.max(Math.hypot(dx, dy), 0.01);
  const offset = (link.index - (link.total - 1) / 2) * 38;
  const control = {
    x: (start.x + end.x) / 2 + (-dy / length) * offset * 2,
    y: (start.y + end.y) / 2 + (dx / length) * offset * 2,
  };
  const towards = (from, to, distance) => {
    const ex = to.x - from.x;
    const ey = to.y - from.y;
    const size = Math.max(Math.hypot(ex, ey), 0.01);
    return { x: from.x + (ex / size) * distance, y: from.y + (ey / size) * distance };
  };
  const a = towards(start, control, rStart);
  const b = towards(end, control, rEnd + (link.directed ? 3 : 0));
  // the point of the curve at t (0 = start, 1 = end)
  const at = (t) => ({
    x: (1 - t) * (1 - t) * a.x + 2 * (1 - t) * t * control.x + t * t * b.x,
    y: (1 - t) * (1 - t) * a.y + 2 * (1 - t) * t * control.y + t * t * b.y,
  });
  return { d: `M ${a.x} ${a.y} Q ${control.x} ${control.y} ${b.x} ${b.y}`, at };
}

// Rough boxes of what a label should stay clear of: each node's circle and
// the name written under it (about 6.5px a letter at 12px).
function obstacles(model) {
  return model.nodes.map((node) => {
    const p = positions.get(node.id);
    const half = Math.max(node.r, (node.name.length * 6.5) / 2);
    return { left: p.x - half, right: p.x + half, top: p.y - node.r, bottom: p.y + node.r + 18 };
  });
}

/** Where on the line to write TEXT: the middle, or the nearest spot clear of the nodes. */
function labelSpot(geo, text, boxes) {
  const halfW = (text.length * 5.5) / 2 + 2;
  const clear = (point) => !boxes.some((box) => point.x + halfW > box.left && point.x - halfW < box.right
    && point.y + 3 > box.top && point.y - 11 < box.bottom);
  for (const t of [0.5, 0.4, 0.6, 0.32, 0.68, 0.25, 0.75]) {
    const point = geo.at(t);
    if (clear(point)) return point;
  }
  return geo.at(0.5);
}

// ---- panels and actions ------------------------------------------------------

function holdRenders(ctx, hold) {
  if (hold && !panelHeld) { panelHeld = true; ctx.editStarted(); }
  if (!hold && panelHeld) { panelHeld = false; ctx.editEnded(); }
}

function closePanel(ctx) {
  panel = null;
  holdRenders(ctx, false);
  ctx.rerender();
}

function openPanel(next, ctx) {
  panel = next;
  holdRenders(ctx, true);
  ctx.rerender();
}

/** A click on a character: select it, or add a relationship from the selected one. */
function pick(id, ctx) {
  if (selected === null) { selected = id; }
  else if (selected === id) { selected = null; }
  else { openPanel({ kind: 'add', source: selected, target: id }, ctx); return; }
  ctx.rerender();
}

function typeChoices(ctx, model) {
  const used = model.entries.map((entry) => entry.type);
  return [...new Set([...(ctx.snapshot.relationshipTypes || []), ...used])].filter(Boolean);
}

function nameOf(model, id) {
  const entity = model.byId.get(id);
  return entity ? entity.name : model.ghosts.get(id) || id;
}

function typeInput(value, model, ctx) {
  const input = el('input', 'rel-type');
  input.type = 'text';
  input.value = value;
  input.setAttribute('aria-label', ctx.t('companion-ui-relations-type'));
  input.setAttribute('list', 'rel-types');
  return input;
}

function addPanel(model, ctx) {
  const box = el('section', 'rel-panel');
  box.append(el('h3', '', ctx.t('companion-ui-relations-add')));
  box.append(el('p', '', ctx.t('companion-ui-relations-from-to',
    nameOf(model, panel.source), nameOf(model, panel.target))));
  const input = typeInput('', model, ctx);
  const submit = () => {
    const type = input.value.trim();
    if (!type) return;
    const { source, target } = panel;
    selected = null;
    panel = null;
    holdRenders(ctx, false);
    ctx.edit('set-relationship', { source, target, type });
  };
  input.addEventListener('keydown', (event) => {
    if (event.key === 'Enter') { event.preventDefault(); submit(); }
    if (event.key === 'Escape') { event.preventDefault(); closePanel(ctx); }
  });
  const add = el('button', '', ctx.t('companion-ui-relations-add'));
  add.type = 'button';
  add.addEventListener('click', submit);
  box.append(input, add, closeButton(ctx));
  queueMicrotask(() => input.focus());
  return box;
}

function closeButton(ctx) {
  const close = el('button', '', ctx.t('companion-ui-relations-close'));
  close.type = 'button';
  close.addEventListener('click', () => closePanel(ctx));
  return close;
}

function pairPanel(model, ctx) {
  const box = el('section', 'rel-panel');
  const mine = model.entries.filter((entry) =>
    (entry.source === panel.a && entry.target === panel.b) || (entry.source === panel.b && entry.target === panel.a));
  box.append(el('h3', '', ctx.t('companion-ui-relations-from-to', nameOf(model, panel.a), nameOf(model, panel.b))));
  for (const entry of mine) {
    const row = el('div', 'rel-row');
    row.append(el('span', 'rel-from-to',
      ctx.t('companion-ui-relations-from-to', nameOf(model, entry.source), nameOf(model, entry.target))));
    const input = typeInput(entry.type, model, ctx);
    const send = (type) => {
      holdRenders(ctx, false);
      panel = null;
      ctx.edit('set-relationship', { source: entry.source, target: entry.target, old: entry.type, type });
    };
    const save = el('button', '', ctx.t('companion-ui-relations-save'));
    save.type = 'button';
    save.addEventListener('click', () => { if (input.value.trim() && input.value.trim() !== entry.type) send(input.value.trim()); });
    const remove = el('button', '', ctx.t('companion-ui-relations-remove'));
    remove.type = 'button';
    remove.addEventListener('click', () => send(''));
    input.addEventListener('keydown', (event) => {
      if (event.key === 'Enter') { event.preventDefault(); save.click(); }
      if (event.key === 'Escape') { event.preventDefault(); closePanel(ctx); }
    });
    row.append(input, save, remove);
    box.append(row);
  }
  const add = el('button', '', ctx.t('companion-ui-relations-add'));
  add.type = 'button';
  // a character that no longer exists cannot be the end of a new line
  const both = model.byId.has(panel.a) && model.byId.has(panel.b);
  add.disabled = !both;
  if (!both) add.title = ctx.t('companion-ui-relations-dangling');
  add.addEventListener('click', () => {
    if (both) openPanel({ kind: 'add', source: panel.a, target: panel.b }, ctx);
  });
  box.append(add, closeButton(ctx));
  return box;
}

// ---- drawing -----------------------------------------------------------------

function legend(model, links, ctx) {
  const box = el('div', 'rel-legend');
  const roles = [...new Set(model.nodes.filter((node) => !node.ghost).map((node) => node.role))];
  if (roles.length) {
    box.append(el('strong', '', ctx.t('companion-ui-relations-role-legend')));
    for (const role of roles) {
      const item = el('span', 'rel-key');
      const name = role || ctx.t('companion-ui-relations-no-role');
      if (role) {
        item.append(legendSwatch({ kind: 'role', value: role, label: name }, 'shape', ctx, (button) => {
          const mark = svg('svg', { viewBox: '-10 -10 20 20', width: 16, height: 16, 'aria-hidden': 'true' });
          mark.append(svg('path', { d: shapePath(shapeOf(role), 8) }));
          button.append(mark);
        }));
      } else {
        const dot = el('span', 'rel-dot');
        dot.style.background = FALLBACK;
        item.append(dot);
      }
      item.append(document.createTextNode(name));
      box.append(item);
    }
  }
  const types = [...new Set(links.map((link) => link.type))];
  if (types.length) {
    box.append(el('strong', '', ctx.t('companion-ui-relations-type-legend')));
    for (const type of types) {
      const item = el('span', 'rel-key');
      item.append(legendSwatch({ kind: 'reltype', value: type, label: type }, 'line', ctx, (button) => {
        const mark = svg('svg', { viewBox: '0 0 30 6', width: 30, height: 6, 'aria-hidden': 'true' });
        mark.append(svg('line', { x1: 0, y1: 3, x2: 30, y2: 3, 'stroke-dasharray': dashOf(type) }));
        button.append(mark);
      }), document.createTextNode(type));
      box.append(item);
    }
  }
  return box;
}

function drawGraph(model, links, ctx) {
  const root = svg('svg', {
    class: 'rel-graph', viewBox: `0 0 ${WIDTH} ${HEIGHT}`, role: 'group',
    'aria-label': ctx.t('companion-ui-view-relations'),
  });
  // many lines: the labels collide, so show them only for the lines in focus
  if (links.length > 25) root.classList.add('dense');
  const radii = new Map(model.nodes.map((node) => [node.id, node.r]));
  const defs = svg('defs');
  const markers = new Map();
  for (const type of new Set(links.filter((link) => link.directed).map((link) => link.type))) {
    const id = `rel-arrow-${markers.size}`;
    markers.set(type, id);
    const marker = svg('marker', {
      id, viewBox: '0 0 10 10', refX: 9, refY: 5, markerWidth: 8, markerHeight: 8, orient: 'auto-start-reverse',
    });
    marker.append(svg('path', { d: 'M 0 0 L 10 5 L 0 10 z', fill: typeColour(type) }));
    defs.append(marker);
  }
  root.append(defs);

  const lineLayer = svg('g');
  const nodeLayer = svg('g');
  root.append(lineLayer, nodeLayer);

  const lines = links.map((link) => {
    const group = svg('g', { class: 'rel-line', tabindex: 0, role: 'button' });
    const a = nameOf(model, link.a);
    const b = nameOf(model, link.b);
    group.setAttribute('aria-label', ctx.t('companion-ui-relations-line',
      ctx.t('companion-ui-relations-from-to', a, b), link.type));
    const hit = svg('path', { class: 'rel-hit' });
    const path = svg('path', { class: 'rel-path', stroke: typeColour(link.type) });
    if (dashOf(link.type)) path.setAttribute('stroke-dasharray', dashOf(link.type));
    if (link.directed) path.setAttribute('marker-end', `url(#${markers.get(link.type)})`);
    const label = svg('text', { class: 'rel-label' }, link.type);
    const ends = link.directed
      ? `${nameOf(model, link.from)} → ${nameOf(model, link.to)}` : `${a} ↔ ${b}`;
    group.append(svg('title', {}, `${ends}: ${link.type}`), hit, path, label);
    const open = () => openPanel({ kind: 'pair', a: link.a, b: link.b }, ctx);
    group.addEventListener('click', open);
    group.addEventListener('keydown', (event) => { if (event.key === 'Enter') { event.preventDefault(); open(); } });
    if (selected !== null) {
      group.classList.add(link.a === selected || link.b === selected ? 'active' : 'faded');
    }
    lineLayer.append(group);
    return { link, hit, path, label };
  });

  const place = () => {
    const boxes = obstacles(model);
    for (const line of lines) {
      const geo = geometry(line.link, radii);
      const { d } = geo;
      const label = labelSpot(geo, line.link.type, boxes);
      line.hit.setAttribute('d', d);
      line.path.setAttribute('d', d);
      line.label.setAttribute('x', label.x);
      line.label.setAttribute('y', label.y);
    }
  };

  const nodeEls = new Map();
  for (const node of model.nodes) {
    const group = svg('g', { class: 'rel-node', tabindex: 0, role: 'button', 'aria-label': node.name });
    if (node.ghost) group.classList.add('ghost');
    if (node.id === selected) group.classList.add('selected');
    const circle = svg('path', { class: 'rel-shape', d: shapePath(node.ghost ? 'circle' : shapeOf(node.role), node.r) });
    if (!node.ghost) circle.setAttribute('fill', roleColour(node.role));
    const title = svg('title', {}, node.ghost ? ctx.t('companion-ui-relations-dangling') : (node.role ? `${node.name} (${node.role})` : node.name));
    const text = svg('text', { class: 'rel-name', y: node.r + 14 }, node.name);
    group.append(title, circle, text);
    nodeLayer.append(group);
    nodeEls.set(node.id, group);

    let drag = null;
    group.addEventListener('pointerdown', (event) => {
      if (node.ghost) return;
      drag = { x: event.clientX, y: event.clientY, moved: false };
      group.setPointerCapture(event.pointerId);
    });
    group.addEventListener('pointermove', (event) => {
      if (!drag) return;
      if (!drag.moved && Math.hypot(event.clientX - drag.x, event.clientY - drag.y) < 4) return;
      drag.moved = true;
      const point = root.createSVGPoint();
      point.x = event.clientX;
      point.y = event.clientY;
      const local = point.matrixTransform(root.getScreenCTM().inverse());
      positions.set(node.id, {
        x: Math.min(WIDTH - 20, Math.max(20, local.x)), y: Math.min(HEIGHT - 20, Math.max(20, local.y)),
      });
      group.setAttribute('transform', `translate(${positions.get(node.id).x} ${positions.get(node.id).y})`);
      place();
    });
    group.addEventListener('pointerup', () => {
      const moved = drag && drag.moved;
      drag = null;
      if (!moved && !node.ghost) pick(node.id, ctx);
    });
    group.addEventListener('dblclick', () => { if (!node.ghost) ctx.goto(node.id); });
    group.addEventListener('keydown', (event) => {
      if (node.ghost) return;
      if (event.key === ' ') { event.preventDefault(); pick(node.id, ctx); }
      if (event.key === 'Enter') { event.preventDefault(); ctx.goto(node.id); }
    });
    group.setAttribute('transform', `translate(${positions.get(node.id).x} ${positions.get(node.id).y})`);
  }
  place();
  return root;
}

function strip(model, ctx) {
  const box = el('section', 'rel-strip');
  box.append(el('h3', '', ctx.t('companion-ui-relations-unlinked')));
  for (const entity of model.alone) {
    const chip = el('button', 'rel-chip', entity.name);
    chip.type = 'button';
    if (entity.id === selected) chip.classList.add('selected');
    chip.addEventListener('click', () => pick(entity.id, ctx));
    chip.addEventListener('dblclick', () => ctx.goto(entity.id));
    box.append(chip);
  }
  return box;
}

function table(model, ctx) {
  const details = el('details', 'rel-table');
  details.append(el('summary', '', ctx.t('companion-ui-relations-table')));
  const grid = el('table', 'storyline');
  const head = el('tr');
  for (const key of ['companion-ui-relations-col-from', 'companion-ui-relations-type', 'companion-ui-relations-col-to']) {
    head.append(el('th', '', ctx.t(key)));
  }
  grid.append(head);
  for (const entry of model.entries) {
    const row = el('tr');
    row.append(el('td', '', nameOf(model, entry.source)), el('td', '', entry.type), el('td', '', nameOf(model, entry.target)));
    grid.append(row);
  }
  details.append(grid);
  return details;
}

export function render(container, ctx) {
  container.replaceChildren();
  const root = ctx.snapshot.project.root;
  if (positionsRoot !== root) { positions.clear(); positionsRoot = root; selected = null; panel = null; }

  const model = buildModel(ctx);
  if (!model.all.length) {
    container.append(el('p', 'empty', ctx.t('companion-ui-relations-empty')));
    return;
  }
  const links = buildLinks(model.entries);
  const present = new Set(model.nodes.map((node) => node.id));
  for (const id of [...positions.keys()]) if (!present.has(id)) positions.delete(id);
  if (selected !== null && !model.byId.has(selected)) selected = null;
  const fixed = new Set(model.nodes.filter((node) => positions.has(node.id)).map((node) => node.id));
  if (fixed.size < model.nodes.length) layout(model.nodes, links, fixed);

  const tools = el('div', 'table-tools');
  const label = el('label', 'switch');
  const box = el('input');
  box.type = 'checkbox';
  box.checked = showHidden;
  box.addEventListener('change', () => { showHidden = box.checked; ctx.rerender(); });
  label.append(box, el('span', '', ctx.t('companion-ui-show-hidden')));
  tools.append(label, el('span', 'legend', ctx.t('companion-ui-relations-legend')));
  container.append(tools);
  if (selected !== null) {
    container.append(el('p', 'legend', ctx.t('companion-ui-relations-selected', nameOf(model, selected))));
  }
  if (model.hiddenNames.length) {
    container.append(el('p', 'legend', ctx.t('companion-ui-not-shown', model.hiddenNames.join(', '))));
  }

  const datalist = el('datalist');
  datalist.id = 'rel-types';
  for (const type of typeChoices(ctx, model)) {
    const option = el('option');
    option.value = type;
    datalist.append(option);
  }
  container.append(datalist);

  if (model.nodes.length) {
    container.append(legend(model, links, ctx));
    const wrap = el('div', 'rel-wrap');
    wrap.append(drawGraph(model, links, ctx));
    container.append(wrap);
  }
  const shown = panel && (panel.kind === 'add' ? addPanel(model, ctx) : pairPanel(model, ctx));
  if (shown) {
    container.append(shown);
    // the graph is tall: bring the form into view instead of leaving it below the fold
    queueMicrotask(() => shown.scrollIntoView({ block: 'nearest', behavior: 'smooth' }));
  }
  if (model.alone.length) container.append(strip(model, ctx));
  if (model.entries.length) container.append(table(model, ctx));
}
