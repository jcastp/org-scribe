// Helpers shared by the views: in-place editing and the move command.
// All text is put in with textContent.

export function el(tag, className, text) {
  const node = document.createElement(tag);
  if (className) node.className = className;
  if (text !== undefined) node.textContent = text;
  return node;
}

/** Replace NODE's text with an editor; COMMIT gets the new value if it changed. */
export function startEdit(node, initial, multiline, commit, ctx) {
  const editor = el(multiline ? 'textarea' : 'input', 'card-edit');
  if (multiline) editor.rows = 4;
  else editor.type = 'text';
  editor.value = initial;
  const parent = node.parentNode;
  let finished = false;
  const finish = (save) => {
    if (finished) return;
    finished = true;
    editor.replaceWith(node);
    ctx.editEnded();
    if (save && editor.value !== initial) commit(editor.value);
  };
  editor.addEventListener('keydown', (event) => {
    event.stopPropagation();
    if (event.key === 'Enter' && !event.shiftKey) { event.preventDefault(); finish(true); }
    if (event.key === 'Escape') { event.preventDefault(); finish(false); }
  });
  editor.addEventListener('blur', () => finish(true));
  ctx.editStarted();
  parent.replaceChild(editor, node);
  editor.focus();
  editor.select();
}

export function moveTo(ctx, id, chapterId, after) {
  if (after === null) {
    ctx.notice(ctx.t('companion-ui-cannot-place'));
    ctx.rerender();
    return;
  }
  ctx.edit('move-node', { id, parent: chapterId, after: after || null }, id);
}

