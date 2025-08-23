const BASE = ''; // proxy from CRA dev server (setupProxy.js)

async function fetchJson(path, body){
  const r = await fetch(`${BASE}${path}`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body),
  });
  if (!r.ok) {
    const text = await r.text().catch(()=>null);
    throw new Error(`HTTP ${r.status} ${text || ''}`);
  }
  return await r.json();
}

export function getQuestions(suspect, crime, answers = {}) {
  return fetchJson('/api/questions', { suspect, crime, answers });
}

export function evaluate(suspect, crime, answers = {}) {
  return fetchJson('/api/evaluate', { suspect, crime, answers });
}