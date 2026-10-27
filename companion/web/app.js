// Placeholder page (Phase 2): fetches the snapshot and prints the title.
// The token comes in the URL (?t=...) and is kept in memory.

const token = new URLSearchParams(location.search).get('t') || '';
const headers = { 'X-Org-Scribe-Token': token };
const $ = (id) => document.getElementById(id);

async function getJSON(path) {
  const response = await fetch(path, { headers });
  if (!response.ok) throw new Error(`${path}: ${response.status}`);
  return response.json();
}

async function load(strings) {
  const project = await getJSON('/api/project');
  document.documentElement.lang = project.language;
  document.title = project.project.title;
  $('status').textContent = strings['companion-ui-project'];
  $('title').textContent = project.project.title;
}

let strings = {};
try {
  strings = await getJSON('/api/strings');
  $('status').textContent = strings['companion-ui-loading'];
  await load(strings);
  const events = new EventSource(`/api/events?t=${encodeURIComponent(token)}`);
  events.addEventListener('changed', () => load(strings));
  events.addEventListener('bye', () => events.close());
} catch (error) {
  $('status').textContent = strings['companion-ui-error'] || String(error);
}
