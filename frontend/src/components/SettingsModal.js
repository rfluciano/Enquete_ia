import React, { useState } from "react";
import "./SettingsModal.css";
import SuspectDetails from "./SuspectDetails";

const SUSPECTS = [
  { id: "mary", name: "Mary", img: "/images/suspects/mary.png" },
  { id: "john", name: "John", img: "/images/suspects/john.png" },
  { id: "alice", name: "Alice", img: "/images/suspects/alice.png" },
  { id: "bob", name: "Bob", img: "/images/suspects/bob.png" },
];

export default function SettingsModal({ show, onClose }) {
  const [selectedSuspect, setSelectedSuspect] = useState(null);

  if (!show) return null;

  return (
    <>
      {/* Modal principal : liste des suspects */}
      <div className="modal-backdrop-custom" onClick={onClose}>
        <div className="modal-dialog-custom" onClick={(e) => e.stopPropagation()}>
          <div className="d-flex justify-content-between align-items-center mb-3">
            <h5 className="m-0">Paramètres — Suspects</h5>
            <button className="btn btn-sm btn-outline-secondary" onClick={onClose}>
              Fermer
            </button>
          </div>

          <div className="suspect-grid-modal">
            {SUSPECTS.map((s) => (
              <div
                key={s.id}
                className="suspect-tile"
                onClick={() => setSelectedSuspect(s)}
              >
                <div
                  className="suspect-thumb"
                  style={{ backgroundImage: `url(${s.img})` }}
                />
                <div className="suspect-name">{s.name}</div>
              </div>
            ))}
          </div>
        </div>
      </div>

      {/* Modal secondaire : détails du suspect */}
      {selectedSuspect && (
        <SuspectDetails
          suspect={selectedSuspect}
          onClose={() => setSelectedSuspect(null)}
        />
      )}
    </>
  );
}
