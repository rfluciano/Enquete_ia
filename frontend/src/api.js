const BASE = ''; // proxy from CRA dev server (setupProxy.js)

// Utilitaire pour POST JSON
async function fetchJson(path, body) {
  const r = await fetch(`${BASE}${path}`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body),
  });
  if (!r.ok) {
    const text = await r.text().catch(() => null);
    throw new Error(`HTTP ${r.status} ${text || ''}`);
  }
  return await r.json();
}

// --- Fonctions existantes ---
export function getQuestions(suspect, crime, answers = {}) {
  return fetchJson('/api/questions', { suspect, crime, answers });
}

export function evaluate(suspect, crime, answers = {}) {
  return fetchJson('/api/evaluate', { suspect, crime, answers });
}

// ---- NOUVELLE FONCTION ----
// Maintenant en GET avec query params, conforme à server.pl
export async function getFactsState(suspect, crime) {
  const params = new URLSearchParams({ suspect, crime });
  const r = await fetch(`${BASE}/api/facts_state?${params.toString()}`, {
    method: 'GET',
  });
  if (!r.ok) {
    const text = await r.text().catch(() => null);
    throw new Error(`HTTP ${r.status} ${text || ''}`);
  }
  return await r.json();
}
