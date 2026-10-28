// Colours: what colour a role, a relationship type, a PoV character or a TODO
// state is drawn in, and the picker that changes it.  Emacs decides (theme,
// pins, the order palette slots are handed out: see the snapshot's `colors`);
// this file only looks colours up for the mode the page is in and asks Emacs
// to change them with the `set-color` command.  Text goes in with textContent.

import { el } from './edit.js';

const query = window.matchMedia('(prefers-color-scheme: dark)');
let colors = null;

/** Light or dark, as the browser says now. */
export const mode = () => (query.matches ? 'dark' : 'light');

/** Call HANDLER when the system switches between light and dark. */
export function onModeChange(handler) {
  query.addEventListener('change', handler);
}

/** Take the snapshot's colours and put the page's own (warning, focus) in place. */
export function setColors(snapshotColors) {
  colors = snapshotColors;
  const style = document.documentElement.style;
  style.setProperty('--warn', colors.warn[mode()]);
  style.setProperty('--focus', colors.focus[mode()]);
}

export const current = () => colors;

/** The key a value has among the colours: a PoV is an ID, the rest ignore case. */
const keyOf = (kind, value) => (kind === 'pov' ? String(value).trim() : String(value).trim().toLowerCase());

/** The entry (`{ index }` or `{ color }`, maybe `reused`) of a value, or null. */
export function entryOf(kind, value) {
  if (!colors || value === null || value === undefined || String(value).trim() === '') return null;
  return colors[kind][keyOf(kind, value)] || null;
}

/** The CSS colour of a value of KIND for the current mode, or null. */
export function colorOf(kind, value) {
  const entry = entryOf(kind, value);
  if (!entry) return null;
  return entry.color || colors.palette[mode()][entry.index % colors.palette[mode()].length];
}

/** A number that belongs to the value itself, for the non-colour cue (line
 *  pattern, node shape): Emacs derives it from the value, so adding another
 *  value never changes it. */
export function rankOf(kind, value) {
  if (!colors) return 0;
  const entry = colors[kind][keyOf(kind, value)];
  return entry && Number.isInteger(entry.rank) ? entry.rank : 0;
}

/** The colour of TODO keyword, light or dark as the page is. */
export function todoColor(snapshot, keyword) {
  if (keyword === null || keyword === undefined) return null;
  const table = mode() === 'dark' && snapshot.todo.colorsDark ? snapshot.todo.colorsDark : snapshot.todo.colors;
  return table[keyword] || null;
}

/** Black or white, whichever reads on COLOR (a #rrggbb string). */
export function textOn(color) {
  const channel = (index) => {
    const value = parseInt(color.slice(1 + index * 2, 3 + index * 2), 16) / 255;
    return value <= 0.04045 ? value / 12.92 : ((value + 0.055) / 1.055) ** 2.4;
  };
  const luminance = 0.2126 * channel(0) + 0.7152 * channel(1) + 0.0722 * channel(2);
  return luminance > 0.18 ? '#000000' : '#ffffff';
}

// ---- the picker --------------------------------------------------------------

let open = null;

export function closePicker() {
  if (!open) return;
  open.node.remove();
  document.removeEventListener('pointerdown', open.outside, true);
  document.removeEventListener('keydown', open.keys, true);
  const { ctx } = open;
  open = null;
  ctx.editEnded();
}

/**
 * Open a picker under ANCHOR for one item.  SPEC: `kind` (`role`, `reltype`,
 * `pov`, `warn`, `focus`), `value` (the item; omitted for warn and focus) and
 * `label` (its name).  Choosing a swatch, or the system colour dialog, pins
 * that colour; Reset takes the pin away.
 */
export function openPicker(anchor, spec, ctx) {
  closePicker();
  const node = el('div', 'picker colour-picker');
  node.setAttribute('role', 'dialog');
  node.setAttribute('aria-label', ctx.t('companion-ui-color-pick', spec.label));
  node.append(el('strong', '', ctx.t('companion-ui-color-pick', spec.label)));
  node.append(chooser(spec, ctx, () => closePicker()));
  const rect = anchor.getBoundingClientRect();
  node.style.left = `${Math.max(8, Math.min(rect.left + window.scrollX, window.innerWidth - 300))}px`;
  node.style.top = `${rect.bottom + window.scrollY + 4}px`;
  document.body.append(node);
  const outside = (event) => { if (!node.contains(event.target) && !anchor.contains(event.target)) closePicker(); };
  const keys = (event) => { if (event.key === 'Escape') { event.preventDefault(); closePicker(); } };
  document.addEventListener('pointerdown', outside, true);
  document.addEventListener('keydown', keys, true);
  open = { node, outside, keys, ctx };
  ctx.editStarted();            // hold re-renders: they would take the anchor away
  const first = node.querySelector('button, input');
  if (first) first.focus();
}

/** Ask Emacs to pin COLOR ('' removes the pin) for the item SPEC names. */
export function pin(spec, color, ctx) {
  const args = { kind: spec.kind, color };
  if (spec.value !== undefined) args.value = spec.value;
  return ctx.edit('set-color', args);
}

/** The swatches of the current palette, a free colour and Reset, for SPEC. */
export function chooser(spec, ctx, done) {
  const box = el('div', 'colour-chooser');
  const palette = colors ? colors.palette[mode()] : [];
  const row = el('div', 'colour-swatches');
  for (const color of palette) {
    const swatch = el('button', 'colour-swatch');
    swatch.type = 'button';
    swatch.style.background = color;
    swatch.title = color;
    swatch.setAttribute('aria-label', color);
    swatch.addEventListener('click', () => { done(); pin(spec, color, ctx); });
    row.append(swatch);
  }
  const free = el('input', 'colour-free');
  free.type = 'color';
  const now = spec.kind === 'warn' || spec.kind === 'focus' ? colors[spec.kind][mode()] : colorOf(spec.kind, spec.value);
  if (/^#[0-9a-f]{6}$/i.test(now || '')) free.value = now;
  free.setAttribute('aria-label', ctx.t('companion-ui-color-pick', spec.label));
  free.addEventListener('change', () => { done(); pin(spec, free.value, ctx); });
  const reset = el('button', '', ctx.t('companion-ui-color-reset'));
  reset.type = 'button';
  reset.addEventListener('click', () => { done(); pin(spec, '', ctx); });
  box.append(row, el('div', 'colour-actions'));
  box.lastChild.append(free, reset);
  return box;
}

/**
 * A legend swatch for one item that opens its picker.  SHAPE draws it:
 * `dot` (a disc) or `line` (a bar), with STYLE added to the element.
 */
export function legendSwatch(spec, className, ctx, decorate = () => {}) {
  const button = el('button', `colour-key ${className}`);
  button.type = 'button';
  const color = colorOf(spec.kind, spec.value);
  if (color) button.style.setProperty('--swatch', color);
  const entry = entryOf(spec.kind, spec.value);
  button.title = entry && entry.reused ? ctx.t('companion-ui-color-reused') : ctx.t('companion-ui-color-pick', spec.label);
  button.setAttribute('aria-label', ctx.t('companion-ui-color-pick', spec.label));
  decorate(button);
  button.addEventListener('click', () => openPicker(button, spec, ctx));
  return button;
}

// ---- the colours panel -------------------------------------------------------

/** Draw the panel into CONTAINER: theme, warning and focus, and the pinned list. */
export function renderPanel(container, ctx) {
  container.replaceChildren();
  if (!colors) return;
  const head = el('div', 'colours-head');
  head.append(el('h2', '', ctx.t('companion-ui-colors')));
  const close = el('button', '', ctx.t('companion-ui-color-close'));
  close.type = 'button';
  close.addEventListener('click', () => ctx.closePanel());
  head.append(close);
  container.append(head);

  const themes = el('fieldset', 'colours-themes');
  themes.append(el('legend', '', ctx.t('companion-ui-colors-theme')));
  for (const name of colors.themes) {
    const label = el('label', 'colours-theme');
    const radio = el('input');
    radio.type = 'radio';
    radio.name = 'colour-theme';
    radio.checked = name === colors.theme;
    radio.addEventListener('change', () => ctx.edit('set-color', { kind: 'theme', color: name }));
    const strip = el('span', 'colours-strip');
    for (const color of colors.palettes[name][mode()]) {
      const cell = el('span', 'colours-cell');
      cell.style.background = color;
      strip.append(cell);
    }
    label.append(radio, el('span', '', ctx.t(`companion-ui-theme-${name}`)), strip);
    themes.append(label);
  }
  container.append(themes);

  const ui = el('div', 'colours-ui');
  for (const [kind, key] of [['warn', 'companion-ui-color-warn'], ['focus', 'companion-ui-color-focus']]) {
    const row = el('div', 'colours-row');
    const button = el('button', 'colour-key dot');
    button.type = 'button';
    button.style.setProperty('--swatch', colors[kind][mode()]);
    button.setAttribute('aria-label', ctx.t('companion-ui-color-pick', ctx.t(key)));
    button.addEventListener('click', () => openPicker(button, { kind, label: ctx.t(key) }, ctx));
    row.append(button, el('span', '', ctx.t(key)));
    ui.append(row);
  }
  container.append(ui);

  const chosen = el('div', 'colours-pinned');
  chosen.append(el('h3', '', ctx.t('companion-ui-color-chosen')));
  const pins = colors.pins.filter((entry) => entry.kind !== 'warn' && entry.kind !== 'focus');
  if (pins.length === 0) chosen.append(el('p', 'empty', ctx.t('companion-ui-color-none')));
  for (const entry of pins) {
    const row = el('div', 'colours-row');
    const label = `${ctx.t(`companion-ui-color-kind-${entry.kind}`)}: ${ctx.nameFor(entry.kind, entry.value)}`;
    const button = el('button', 'colour-key dot');
    button.type = 'button';
    button.style.setProperty('--swatch', entry.color);
    button.setAttribute('aria-label', ctx.t('companion-ui-color-pick', label));
    button.addEventListener('click', () => openPicker(button, { kind: entry.kind, value: entry.value, label }, ctx));
    const reset = el('button', '', ctx.t('companion-ui-color-reset'));
    reset.type = 'button';
    reset.addEventListener('click', () => pin({ kind: entry.kind, value: entry.value }, '', ctx));
    row.append(button, el('span', '', label), reset);
    chosen.append(row);
  }
  const all = el('button', '', ctx.t('companion-ui-color-reset-all'));
  all.type = 'button';
  all.disabled = colors.pins.length === 0;
  all.addEventListener('click', () => ctx.edit('set-color', { kind: 'all' }));
  chosen.append(all);
  container.append(chosen);
}
