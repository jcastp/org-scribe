// The keyboard help: the shortcuts the page has, in a dialog that ? opens and
// closes.  The list is data, so a new shortcut is one row here.  A key label
// that names a word (Enter, Escape, Shift+click) is a string key; the others
// (F2, Ctrl+Z, arrows) read the same in every language.

/** Each group: its title and its rows, [key labels, description]. */
function groups(tabCount) {
  return [
    { title: 'companion-ui-help-anywhere', rows: [
      [['?'], 'companion-ui-help-toggle'],
      [[tabCount > 9 ? '1–9, 0' : tabCount > 1 ? `1–${tabCount}` : '1'], 'companion-ui-help-tabs'],
      [['Ctrl+Z'], 'companion-ui-help-undo'],
    ] },
    { title: 'companion-ui-help-scenes', rows: [
      [['companion-ui-key-enter'], 'companion-ui-help-enter'],
      [['F2'], 'companion-ui-help-rename'],
      [['I'], 'companion-ui-help-details'],
      [['Alt+↑', 'Alt+↓'], 'companion-ui-help-move'],
    ] },
    { title: 'companion-ui-help-panel', rows: [
      [['Alt+↑', 'Alt+↓'], 'companion-ui-help-step'],
      [['companion-ui-key-escape'], 'companion-ui-help-close-panel'],
    ] },
    { title: 'companion-ui-help-editing', rows: [
      [['companion-ui-key-enter'], 'companion-ui-help-save'],
      [['companion-ui-key-escape'], 'companion-ui-help-cancel'],
    ] },
    { title: 'companion-ui-help-mouse', rows: [
      [['companion-ui-key-double-click'], 'companion-ui-help-double-click'],
      [['companion-ui-key-shift-click'], 'companion-ui-help-shift-table'],
      [['companion-ui-key-shift-click'], 'companion-ui-help-shift-storyline'],
    ] },
  ];
}

function el(tag, className, text) {
  const node = document.createElement(tag);
  if (className) node.className = className;
  if (text !== undefined) node.textContent = text;
  return node;
}

function keyCell(labels, t) {
  const cell = el('td', 'help-keys');
  labels.forEach((label, index) => {
    if (index) cell.append(' ');
    cell.append(el('kbd', null, label.startsWith('companion-') ? t(label) : label));
  });
  return cell;
}

function fill(dialog, ctx) {
  const { t } = ctx;
  const head = el('div', 'help-head');
  const title = el('h2', null, t('companion-ui-help-title'));
  title.id = 'help-title';
  const close = el('button', 'help-close', '×');
  close.type = 'button';
  close.title = t('companion-ui-help-close');
  close.setAttribute('aria-label', t('companion-ui-help-close'));
  close.addEventListener('click', () => dialog.close());
  head.append(title, close);
  const body = el('div', 'help-body');
  for (const group of groups(ctx.tabCount)) {
    const section = el('section', 'help-group');
    section.append(el('h3', null, t(group.title)));
    const table = el('table');
    for (const [labels, text] of group.rows) {
      const row = el('tr');
      row.append(keyCell(labels, t), el('td', null, t(text)));
      table.append(row);
    }
    section.append(table);
    body.append(section);
  }
  dialog.replaceChildren(head, body);
}

let wired = false;

/** Open the help in DIALOG, drawn afresh (the strings and tab count may change). */
export function open(dialog, ctx) {
  if (!wired) {
    wired = true;
    // a click on the backdrop (the dialog itself, outside its content) closes it
    dialog.addEventListener('click', (event) => { if (event.target === dialog) dialog.close(); });
  }
  fill(dialog, ctx);
  dialog.showModal();
}

export function toggle(dialog, ctx) {
  if (dialog.open) dialog.close(); else open(dialog, ctx);
}
