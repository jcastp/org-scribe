// Helpers shared by the views: in-place editing and the move command.
// All text is put in with textContent.

export function el(tag, className, text) {
  const node = document.createElement(tag);
  if (className) node.className = className;
  if (text !== undefined) node.textContent = text;
  return node;
}

/** Replace NODE's text with an editor; COMMIT gets the new value if it changed. */
export function startEdit(node, initial, multiline, commit, ctx, placeholder) {
  const editor = el(multiline ? 'textarea' : 'input', 'card-edit');
  if (multiline) editor.rows = 4;
  else editor.type = 'text';
  editor.value = initial;
  if (placeholder) editor.placeholder = placeholder;
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

/** The ID a scene is added or moved under: its chapter's, or the act's when the
 * project has no chapters (the chapter is then only a pass-through). */
export function sceneParent(chapter) {
  return chapter.passthrough ? chapter.containerId : chapter.id;
}

function addButton(label) {
  const add = el('button', 'add-scene', '+');
  add.type = 'button';
  add.title = label;
  add.setAttribute('aria-label', label);
  return add;
}

/** A "new scene" button for the container PARENT_ID, adding after the last of
 * SCENES (in the manuscript's order).  BEFORE runs first, when given. */
export function addSceneButton(parentId, scenes, ctx, before) {
  const add = addButton(ctx.t('companion-ui-new-scene'));
  add.addEventListener('click', (event) => {
    event.stopPropagation();
    if (before) before();
    const last = [...scenes].reverse().find((scene) => scene.id);
    ctx.edit('create-scene', { parent: parentId, after: last ? last.id : null }, null, true);
  });
  return add;
}

/** A "new chapter" button for ACT, adding after its last chapter.  The title
 * is asked before the chapter exists: Escape (or nothing typed) leaves no
 * stray heading behind.  BEFORE runs once a title is given. */
export function addChapterButton(act, ctx, before) {
  const label = ctx.t('companion-ui-new-chapter');
  const add = addButton(label);
  add.addEventListener('click', (event) => {
    event.stopPropagation();
    const last = [...act.chapters].reverse().find((chapter) => chapter.id);
    startEdit(add, '', false, (title) => {
      if (!title.trim()) return;
      if (before) before();
      ctx.edit('create-chapter', { parent: act.id, after: last ? last.id : null, title }, null, true);
    }, ctx, label);
  });
  return add;
}

export function moveTo(ctx, id, parentId, after) {
  if (after === null) {
    ctx.notice(ctx.t('companion-ui-cannot-place'));
    ctx.rerender();
    return;
  }
  ctx.edit('move-node', { id, parent: parentId, after: after || null }, id);
}

