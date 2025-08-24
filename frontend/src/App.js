import React, { useState } from 'react';
import { getQuestions, evaluate, getFactsState } from './api';
import Header from './components/Header';
import 'bootstrap/dist/css/bootstrap.min.css';
import './App.css';

function App() {
  const [suspect, setSuspect] = useState('mary');
  const [crime, setCrime] = useState('assassinat');
  const [questions, setQuestions] = useState([]);
  const [answers, setAnswers] = useState({});
  const [result, setResult] = useState(null);
  const [loading, setLoading] = useState(false);

  async function start() {
    setLoading(true);
    try {
      const q = await getQuestions(suspect, crime);
      const factsState = await getFactsState(suspect, crime);

      const factsKnown = factsState.facts_state.map(f => ({
        id: f.id,
        text: f.text,
        value: f.value,
        readonly: true
      }));

      const dynamicQuestions = (q.questions || []).map(f => ({
        ...f,
        readonly: false
      }));

      setQuestions([...factsKnown, ...dynamicQuestions]);

      const initialAnswers = {};
      factsKnown.forEach(f => initialAnswers[f.id] = f.value);
      setAnswers(initialAnswers);
      setResult(null);
    } catch (e) {
      console.error(e);
      alert('Erreur: ' + e.message);
    } finally {
      setLoading(false);
    }
  }

  function toggleAnswer(id, val) {
    setAnswers(a => ({ ...a, [id]: val }));
  }

  async function submit() {
    setLoading(true);
    try {
      const res = await evaluate(suspect, crime, answers);

      // Déterminer la raison (alibi ou pas)
      const alibiFact = questions.find(q => q.id === 'alibi');
      const reason = alibiFact
        ? alibiFact.value
          ? 'Car il/elle a un alibi'
          : "Car il/elle n'a pas d'alibi"
        : '';

      // Déduire Information à disposition et Manquantes (sans doublons)
      const infos = res.preuves ? Array.from(new Set(res.preuves)) : [];
      const manquantes = res.manquantes ? Array.from(new Set(res.manquantes)) : [];

      setResult({
        ...res,
        reason,
        informations: [reason, ...infos], // mettre la raison en première info
        manquantes: manquantes
      });

    } catch (e) {
      console.error(e);
      alert('Erreur: ' + e.message);
    } finally {
      setLoading(false);
    }
  }

  return (
    <div className="container py-4">
      <Header />

      <div className="row g-3 mb-3">
        <div className="col-md-4">
          <label className="form-label">Suspect</label>
          <input className="form-control" value={suspect} onChange={e => setSuspect(e.target.value)} required/>
        </div>
        <div className="col-md-4">
          <label className="form-label">Crime</label>
          <select
            className="form-control"
            value={crime}
            onChange={e => setCrime(e.target.value)}
            required
          >
            <option value="">Sélectionnez un crime</option>
            <option value="assassinat">Assassinat</option>
            <option value="vol">Vol</option>
            <option value="escroquerie">Escroquerie</option>
          </select>
        </div>
        <div className="col-md-4 d-flex align-items-end">
          <button
            className="btn btn-primary me-2"
            onClick={() => {
              if (!crime) {
                alert("Veuillez sélectionner un crime avant de poser les questions !");
                return;
              }
              start();
            }}
            disabled={loading}
          >
            {loading ? 'Chargement…' : 'Poser les questions'}
          </button>

          <button
            className="btn btn-success"
            onClick={() => {
              if (!crime) {
                alert("Veuillez sélectionner un crime avant d'évaluer !");
                return;
              }
              submit();
            }}
            disabled={loading || questions.length === 0}
          >
            Evaluer
          </button>
        </div>

      </div>

      <div className="mb-4">
        <h5>Questions & Faits connus</h5>
        {questions.length === 0 && <div className="text-muted">Aucune question pour le moment.</div>}
        <div className="row row-cols-1 row-cols-md-2 g-3 mt-2">
          {questions.map(q => {
            const borderClass = q.readonly
              ? q.value ? 'border-success' : 'border-danger'
              : 'border-primary';

            return (
              <div className="col" key={q.id + (q.readonly ? '_fact' : '')}>
                <div className={`card border-2 shadow-sm ${borderClass}`}>
                  <div className="card-body">
                    <div className="card-title fw-semibold">{q.text}</div>
                    <div className="mt-3">
                      {!q.readonly ? (
                        <>
                          <button
                            className={`btn btn-sm ${answers[q.id] === true ? 'btn-primary' : 'btn-outline-primary'}`}
                            onClick={() => toggleAnswer(q.id, true)}
                          >
                            Oui
                          </button>
                          <button
                            className={`btn btn-sm ms-2 ${answers[q.id] === false ? 'btn-danger' : 'btn-outline-danger'}`}
                            onClick={() => toggleAnswer(q.id, false)}
                          >
                            Non
                          </button>
                          <span className="ms-3 text-muted">Réponse: {String(answers[q.id])}</span>
                        </>
                      ) : (
                        <span className={`badge ${q.value ? 'bg-success' : 'bg-danger'}`}>
                          {q.value ? 'Vrai' : 'Faux'}
                        </span>
                      )}
                    </div>
                  </div>
                </div>
              </div>
            );
          })}
        </div>
      </div>

      {result && (
        <div className={`card shadow-lg ${result.status === 'Coupable' ? 'border-danger' : 'border-success'} mb-4`}>
          <div className="card-body">
            <h3 className={`fw-bold text-center mb-4 ${result.status.startsWith('Coupable') ? 'text-danger' : 'text-success'}`}>
              {result.status.replace(/\s*\(.*\)/, '').toUpperCase()}
            </h3>

            <div className="d-flex flex-column gap-2 fs-5">
              <div><span className="fw-bold">Suspect: </span>{result.suspect}</div>
              <div><span className="fw-bold">Information à disposition: </span>{result.informations && result.informations.length > 0 ? result.informations.join(', ') : 'Aucune'}</div>
              <div><span className="fw-bold">Crime: </span>{result.crime}</div>
              <div><span className="fw-bold">Manquantes: </span>{result.manquantes && result.manquantes.length > 0 ? result.manquantes.join(', ') : 'Aucune'}</div>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}

export default App;
