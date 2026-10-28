// Timeline: the scenes on a story-time axis, with reading order visible.
// Emacs sends each scene's key (minutes from the start of Day 1, negative
// before it); this view only sorts and lays out, it never parses a time.  The
// axis is compressed: one column per occupied day, and a gap marker between
// columns that are not consecutive, so nine years of backstory next to a
// 25-day story still fit.  A card shows the time as the writer wrote it
// (click to change it: the answer re-sorts the view), the scene's place in
// reading order, and a back arrow when the scene happens before the dated
// scene read just ahead of it.  Notebook events appear as plain cards.  What
// has no key goes to an "undated" group at the end.  All text is put in with
// textContent.

import { el, startEdit } from '../edit.js';

const DAY = 1440;
const YEAR = 365;

/** "3 days", "1 year": a length of time in whole days, as text. */
function span(days, ctx) {
  if (days >= 2 * YEAR) return ctx.t('companion-ui-timeline-years', Math.floor(days / YEAR));
  return days === 1 ? ctx.t('companion-ui-timeline-day-one') : ctx.t('companion-ui-timeline-days', days);
}

/** The day label of a column: Day 1 is index 0, there is no Day 0. */
function dayLabel(index, ctx) {
  return ctx.t('companion-ui-timeline-day', index >= 0 ? index + 1 : index);
}

/** Every scene of the snapshot in reading order, joined to its story time. */
function readingScenes(ctx) {
  const times = new Map();
  for (const entry of ctx.snapshot.storyTime.scenes) if (entry.id) times.set(entry.id, entry);
  const scenes = [];
  for (const act of ctx.snapshot.acts) {
    for (const chapter of act.chapters) {
      for (const scene of chapter.scenes) {
        const entry = (scene.id && times.get(scene.id)) || {};
        const raw = scene.fields && scene.fields.timeline ? scene.fields.timeline.raw : '';
        scenes.push({
          kind: 'scene', id: scene.id, title: scene.title, text: raw,
          key: entry.key === undefined ? null : entry.key,
          problem: entry.problem || (raw ? 'unreadable' : 'blank'),
          readOnly: !scene.id || scene.readOnly,
          chapter: chapter.title,
        });
      }
    }
  }
  scenes.forEach((scene, index) => { scene.position = index + 1; });
  return scenes;
}

/** Set `back` (minutes) on each scene earlier than the dated scene read before it. */
function markJumps(scenes) {
  let previous = null;
  for (const scene of scenes) {
    if (scene.key === null) continue;
    if (previous !== null && scene.key < previous) scene.back = previous - scene.key;
    previous = scene.key;
  }
}

function timeText(scene, ctx) {
  const button = el('button', 'tl-time', scene.text || ctx.t('companion-ui-timeline-blank'));
  button.type = 'button';
  if (!scene.text) button.classList.add('empty');
  if (scene.readOnly) { button.disabled = true; return button; }
  button.addEventListener('click', () => {
    startEdit(button, scene.text, false,
      (value) => ctx.edit('set-field', { id: scene.id, key: 'timeline', value }, scene.id), ctx);
  });
  return button;
}

function card(item, ctx) {
  const node = el('div', `tl-card ${item.kind}`);
  if (item.id) node.dataset.id = item.id;
  const top = el('div', 'tl-top');
  if (item.kind === 'scene') {
    const number = el('span', 'tl-position', String(item.position));
    top.append(number);
    if (item.back) {
      const arrow = el('span', 'tl-back', '↶');
      arrow.title = ctx.t('companion-ui-timeline-jump', span(Math.max(1, Math.round(item.back / DAY)), ctx));
      top.append(arrow);
    }
    top.append(timeText(item, ctx));
  } else {
    const label = el('span', 'tl-event-time', item.text);
    label.title = ctx.t('companion-ui-timeline-event');
    top.append(label);
  }
  node.append(top);
  const title = el('button', 'tl-title', item.title);
  title.type = 'button';
  if (item.id) title.addEventListener('click', () => ctx.goto(item.id));
  else title.disabled = true;
  node.append(title);
  return node;
}

function problemText(item, ctx) {
  const key = {
    blank: 'companion-ui-timeline-blank',
    unreadable: 'companion-ui-timeline-unreadable',
    'no-anchor': 'companion-ui-timeline-no-anchor',
  }[item.problem];
  return key ? ctx.t(key) : '';
}

export function render(container, ctx) {
  container.replaceChildren();
  const scenes = readingScenes(ctx);
  if (!scenes.length) {
    container.append(el('p', 'empty', ctx.t('companion-ui-empty')));
    return;
  }
  markJumps(scenes);
  const events = ctx.snapshot.storyTime.events.map((event) => ({
    kind: 'event', id: null, title: event.title, text: event.text,
    key: event.key === undefined ? null : event.key, problem: event.problem,
  }));

  const dated = [...scenes, ...events].filter((item) => item.key !== null);
  // Array.sort is stable: equal keys keep reading order, events after scenes.
  dated.sort((a, b) => a.key - b.key);
  const undated = [...scenes, ...events].filter((item) => item.key === null);

  const columns = [];
  for (const item of dated) {
    const index = Math.floor(item.key / DAY);
    const last = columns[columns.length - 1];
    if (last && last.index === index) last.items.push(item);
    else columns.push({ index, items: [item] });
  }

  container.append(el('p', 'legend', ctx.t('companion-ui-timeline-legend')));
  const lane = el('div', 'tl-lane');
  columns.forEach((column, position) => {
    const before = columns[position - 1];
    if (before && column.index - before.index > 1) {
      const gap = el('div', 'tl-gap', ctx.t('companion-ui-timeline-gap', span(column.index - before.index - 1, ctx)));
      lane.append(gap);
    }
    const node = el('section', 'tl-column');
    node.append(el('h3', 'tl-day', dayLabel(column.index, ctx)));
    for (const item of column.items) node.append(card(item, ctx));
    lane.append(node);
  });
  const scroller = el('div', 'tl-scroll');
  scroller.append(lane);
  container.append(scroller);

  if (undated.length) {
    const group = el('section', 'tl-undated');
    group.append(el('h3', 'tl-day', `${ctx.t('companion-ui-timeline-undated')} (${undated.length})`));
    const cards = el('div', 'tl-undated-cards');
    for (const item of undated) {
      const node = card(item, ctx);
      const reason = problemText(item, ctx);
      if (reason && item.kind === 'scene') node.append(el('span', 'tl-problem', reason));
      cards.append(node);
    }
    group.append(cards);
    container.append(group);
  }
}
