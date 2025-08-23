import React, { useState } from 'react';
import CrimeCard from './CrimeCard';
import SuspectCard from './SuspectCard';
import './Menu.css';

// liste statique (tu peux synchroniser depuis le backend si tu ajoutes un endpoint)
const CRIMES = [
  { id: 'assassinat', label: 'Assassinat', img: '/images/crimes/assassinat.png' },
  { id: 'vol', label: 'Vol', img: '/images/crimes/vol.png' },
  { id: 'escroquerie', label: 'Escroquerie', img: '/images/crimes/escroquerie.png' },
];

const SUSPECTS = [
  { id: 'mary', name: 'Mary', img: '/images/suspects/mary.jpg' },
  { id: 'john', name: 'John', img: '/images/suspects/john.jpg' },
  { id: 'alice', name: 'Alice', img: '/images/suspects/alice.jpg' },
  { id: 'bob', name: 'Bob', img: '/images/suspects/bob.jpg' },
];

export default function Menu({ mode, onModeChange, selectedCrime, onSelectCrime, selectedSuspect, onSelectSuspect, onStart }) {
  const [openSuspect, setOpenSuspect] = useState(null);

  return (
    <main style={{padding:20}}>
      <section style={{marginBottom:16, display:'flex', gap:12, alignItems:'center'}}>
        <label>Mode d'enquête:</label>
        <select value={mode} onChange={e => onModeChange(e.target.value)}>
          <option value="main_argued">main_argued (argumente)</option>
          <option value="main">main (rapide)</option>
        </select>
        <div style={{marginLeft:'auto'}}>
          <button onClick={onStart} style={{padding:'8px 12px'}}>Demarrer l'enquête</button>
        </div>
      </section>

      <section>
        <h3>Choisir un crime</h3>
        <div className="crime-grid">
          {CRIMES.map(c => (
            <CrimeCard key={c.id} crime={c} selected={selectedCrime===c.id} onSelect={() => onSelectCrime(c.id)} />
          ))}
        </div>
      </section>

      <section style={{marginTop:20}}>
        <h3>Suspects</h3>
        <div className="suspect-grid">
          {SUSPECTS.map(s => (
            <SuspectCard
              key={s.id}
              suspect={s}
              selected={selectedSuspect===s.id}
              onSelect={() => onSelectSuspect(s.id)}
              onOpenDetails={() => setOpenSuspect(s.id)}
            />
          ))}
        </div>
      </section>

      {openSuspect && (
        <div className="modal-backdrop" onClick={() => setOpenSuspect(null)}>
          <div className="modal-card" onClick={e => e.stopPropagation()}>
            <h4>Details: {SUSPECTS.find(x=>x.id===openSuspect).name}</h4>
            <p>Faits connus (localement) :</p>
            <ul>
              {/* afficher quelques faits statiques ; tu peux remplacer par call API si tu veux */}
              <li>has_alibi(...)</li>
              <li>has_motive(...)</li>
              <li>was_near_crime_scene(...)</li>
            </ul>
            <button onClick={() => setOpenSuspect(null)}>Fermer</button>
          </div>
        </div>
      )}
    </main>
  );
}