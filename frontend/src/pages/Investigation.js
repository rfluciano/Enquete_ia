import React, { useEffect, useState } from 'react';
import { getQuestions, evaluate } from '../api';

export default function Investigation({ suspect, crime, mode, onBack }) {
  const [questions, setQuestions] = useState([]);
  const [answers, setAnswers] = useState({});
  const [verdict, setVerdict] = useState(null);
  const [loading, setLoading] = useState(false);

  useEffect(() => {
    // initial fetch questions (no answers)
    loadQuestions({});
    // eslint-disable-next-line
  }, []);

  async function loadQuestions(currentAnswers) {
    setLoading(true);
    try {
      const res = await getQuestions(suspect, crime, currentAnswers);
      setQuestions(res.questions || []);
    } catch (e) {
      console.error('loadQuestions', e);
      alert('Erreur serveur: ' + e.message);
    } finally { setLoading(false); }
  }

  function setAnswer(id, value) {
    const next = { ...answers, [id]: value };
    setAnswers(next);
  }

  async function askFollowUp() {
    // envoyer les reponses pour obtenir questions de suivi
    await loadQuestions(answers);
  }

  async function doEvaluate() {
    setLoading(true);
    try {
      const res = await evaluate(suspect, crime, answers);
      setVerdict(res);
    } catch (e) {
      console.error('evaluate', e);
      alert('Erreur serveur: ' + e.message);
    } finally { setLoading(false); }
  }

  return (
    <div style={{padding:20}}>
      <button onClick={onBack}>← Retour</button>
      <h3>Enquête: {suspect} — {crime} ({mode})</h3>

      <div style={{marginTop:12}}>
        <h4>Questions</h4>
        {loading && <div>Chargement…</div>}
        {questions.length === 0 && !loading && <div>Aucune question additionnelle.</div>}
        <div style={{display:'flex',flexDirection:'column', gap:10, marginTop:10}}>
          {questions.map(q => (
            <div key={q.id} style={{padding:10,border:'1px solid #eee',borderRadius:6}}>
              <div style={{fontWeight:600}}>{q.text}</div>
              <div style={{marginTop:8}}>
                <button onClick={() => setAnswer(q.id, true)}>Oui</button>
                <button onClick={() => setAnswer(q.id, false)} style={{marginLeft:8}}>Non</button>
                <span style={{marginLeft:12}}>Reponse actuelle: {String(answers[q.id])}</span>
              </div>
            </div>
          ))}
        </div>

        <div style={{marginTop:12}}>
          <button onClick={askFollowUp}>Poser questions de suivi</button>
          <button onClick={doEvaluate} style={{marginLeft:8}}>Evaluer</button>
        </div>
      </div>

      {verdict && (
        <div style={{marginTop:20,padding:12,border:'1px solid #ddd',borderRadius:6}}>
          <h4>Verdict</h4>
          <div><strong>{verdict.verdict}</strong></div>
          <div>Preuves: {JSON.stringify(verdict.supporting)}</div>
          <div>Manquantes: {JSON.stringify(verdict.missing)}</div>
        </div>
      )}
    </div>
  );
}