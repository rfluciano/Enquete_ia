import React, {useState} from 'react';
import {getQuestions, evaluate} from './api';
import Header from './components/Header';
import 'bootstrap/dist/css/bootstrap.min.css';
import './App.css';

function App() {
  const [suspect,setSuspect]=useState('mary');
  const [crime,setCrime]=useState('assassinat');
  const [questions,setQuestions]=useState([]);
  const [answers,setAnswers]=useState({});
  const [result,setResult]=useState(null);
  const [loading,setLoading]=useState(false);

  async function start(){
    setLoading(true);
    try {
      const q = await getQuestions(suspect, crime);
      setQuestions(q.questions || []);
      setAnswers({});
      setResult(null);
    } catch(e){
      console.error(e);
      alert('Erreur: ' + e.message);
    } finally { setLoading(false); }
  }

  function toggleAnswer(id, val){
    setAnswers(a => ({...a, [id]: val}));
  }

  async function submit(){
    setLoading(true);
    try {
      const res = await evaluate(suspect, crime, answers);
      setResult(res);
    } catch(e){
      console.error(e);
      alert('Erreur: ' + e.message);
    } finally { setLoading(false); }
  }

  return (
    <div className="container py-4">
      <Header />
      <div className="d-flex align-items-center mb-4">
        <h3 className="me-3">Enquête IA</h3>
        <div className="ms-auto text-muted">Local dev</div>
      </div>

      <div className="row g-3 mb-3">
        <div className="col-md-4">
          <label className="form-label">Suspect</label>
          <input className="form-control" value={suspect} onChange={e=>setSuspect(e.target.value)} />
        </div>
        <div className="col-md-4">
          <label className="form-label">Crime</label>
          <input className="form-control" value={crime} onChange={e=>setCrime(e.target.value)} />
        </div>
        <div className="col-md-4 d-flex align-items-end">
          <button className="btn btn-primary me-2" onClick={start} disabled={loading}>
            {loading ? 'Chargement…' : 'Poser les questions'}
          </button>
          <button className="btn btn-success" onClick={submit} disabled={loading || questions.length===0}>
            Evaluer
          </button>
        </div>
      </div>

      <div className="mb-4">
        <h5>Questions</h5>
        {questions.length === 0 && <div className="text-muted">Aucune question pour le moment.</div>}
        <div className="row row-cols-1 row-cols-md-2 g-3 mt-2">
          {questions.map(q => (
            <div className="col" key={q.id}>
              <div className="card">
                <div className="card-body">
                  <div className="card-title fw-semibold">{q.text}</div>
                  <div className="mt-3">
                    <button className={`btn btn-sm ${answers[q.id]===true ? 'btn-primary' : 'btn-outline-primary'}`} onClick={()=>toggleAnswer(q.id, true)}>Oui</button>
                    <button className={`btn btn-sm ms-2 ${answers[q.id]===false ? 'btn-danger' : 'btn-outline-danger'}`} onClick={()=>toggleAnswer(q.id, false)}>Non</button>
                    <span className="ms-3 text-muted">Reponse: {String(answers[q.id])}</span>
                  </div>
                </div>
              </div>
            </div>
          ))}
        </div>
      </div>

      {result && (
        <div className="card border-info">
          <div className="card-body">
            <h5 className="card-title">Verdict</h5>
            <p className="mb-1"><strong>{result.verdict}</strong></p>
            <p className="mb-1"><strong>Preuves:</strong> {JSON.stringify(result.supporting)}</p>
            <p className="mb-0"><strong>Manquantes:</strong> {JSON.stringify(result.missing)}</p>
          </div>
        </div>
      )}
    </div>
  );
}

export default App;
