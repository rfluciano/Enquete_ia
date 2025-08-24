import React, { useState } from 'react';
import SettingsModal from './SettingsModal';

export default function Header() {
  const [showSettings, setShowSettings] = useState(false);

  return (
    <header className="d-flex align-items-center px-3 py-2 border-bottom">
      <div>
        <h4 className="mb-0">Enquête IA</h4>
      </div>

      <nav className="ms-auto d-flex align-items-center gap-2">
        {/* <button type="button" className="btn btn-link text-decoration-none" onClick={()=>alert('Profil')}>
          Profil
        </button> */}
        <button type="button" className="btn btn-link text-decoration-none" onClick={()=>setShowSettings(true)}>
          Parametres
        </button>
        {/* <button type="button" className="btn btn-link text-decoration-none" onClick={()=>alert('Historiques')}>
          Historiques
        </button> */}
      </nav>

      <SettingsModal show={showSettings} onClose={() => setShowSettings(false)} />
    </header>
  );
}