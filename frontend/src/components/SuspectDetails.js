import React, { useEffect, useState } from "react";
import Modal from "react-modal";

export default function SuspectDetails({ suspect, onClose }) {
  const [details, setDetails] = useState(null);
  const [newFact, setNewFact] = useState("");
  const [editingFact, setEditingFact] = useState(null);
  const [editValue, setEditValue] = useState("");

  // Charger les faits
  useEffect(() => {
    if (suspect) {
      refresh();
    } else {
      setDetails(null);
    }
  }, [suspect]);

  const refresh = () => {
    const suspectId = suspect.id || suspect;
    fetch(`/api/suspect?id=${suspectId}`)
      .then((res) => res.json())
      .then((data) => setDetails(data))
      .catch((err) => {
        console.error(err);
        setDetails(null);
      });
  };

  // Ajouter un fait
  const handleAddFact = () => {
    const suspectId = suspect.id || suspect;
    if (!newFact.trim()) return;
    fetch("/add_fact", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ suspect: suspectId, fact: newFact }),
    })
      .then((res) => res.json())
      .then(() => {
        setNewFact("");
        refresh();
      })
      .catch(console.error);
  };

  // Supprimer un fait
  const handleDeleteFact = (fact) => {
    const suspectId = suspect.id || suspect;
    fetch("/delete_fact", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ suspect: suspectId, fact }),
    })
      .then((res) => res.json())
      .then(() => refresh())
      .catch(console.error);
  };

  // Modifier un fait (si tu ajoutes update_fact_handler)
  const handleUpdateFact = (oldFact) => {
    const suspectId = suspect.id || suspect;
    fetch("/update_fact", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ suspect: suspectId, old: oldFact, new: editValue }),
    })
      .then((res) => res.json())
      .then(() => {
        setEditingFact(null);
        setEditValue("");
        refresh();
      })
      .catch(console.error);
  };

  const renderList = (label, items) => (
    <div>
      <b>{label} :</b>{" "}
      {items.length > 0 ? (
        items.map((it, i) => (
          <span key={i} className="me-2">
            {editingFact === it ? (
              <>
                <input
                  className="form-control d-inline w-auto me-2"
                  value={editValue}
                  onChange={(e) => setEditValue(e.target.value)}
                />
                {/* <button
                  className="btn btn-sm btn-success me-1"
                  onClick={() => handleUpdateFact(it)}
                >
                  ✔
                </button> */}
                {/* <button
                  className="btn btn-sm btn-secondary"
                  onClick={() => setEditingFact(null)}
                >
                  ✕
                </button> */}
              </>
            ) : (
              <>
                {it}{" "}
                {/* <button
                  className="btn btn-sm btn-outline-primary me-1"
                  onClick={() => {
                    setEditingFact(it);
                    setEditValue(it);
                  }}
                >
                  ✎
                </button> */}
                {/* <button
                  className="btn btn-sm btn-outline-danger"
                  onClick={() => handleDeleteFact(it)}
                >
                  ✕
                </button> */}
              </>
            )}
          </span>
        ))
      ) : (
        "Aucun"
      )}
    </div>
  );

  return (
    <Modal
      isOpen={!!suspect}
      onRequestClose={onClose}
      contentLabel="Détails suspect"
      className="modal-dialog-custom"
      overlayClassName="modal-backdrop-custom"
    >
      <div className="d-flex justify-content-between align-items-center mb-3">
        <h5 className="m-0">
          Détails — {details?.suspect || suspect?.name || suspect}
        </h5>
        <button className="btn btn-sm btn-outline-secondary" onClick={onClose}>
          Fermer
        </button>
      </div>

      {details ? (
        <div className="suspect-details">
          {renderList("Alibis", details.alibis)}
          {renderList("Motifs", details.motives)}
          {renderList("Près du lieu du crime", details.near_crime_scene)}
          {renderList("Empreintes", details.fingerprints)}
          {renderList("Témoins", details.eyewitness)}
          {renderList("Fausse identité", details.fake_identities)}
          {renderList("Transactions", details.transactions)}

          <hr />
          {/* <div className="d-flex mt-3">
            <input
              className="form-control me-2"
              placeholder="Nouveau fait..."
              value={newFact}
              onChange={(e) => setNewFact(e.target.value)}
            />
            <button className="btn btn-primary" onClick={handleAddFact}>
              Ajouter
            </button>
          </div> */}
        </div>
      ) : (
        <p>Chargement...</p>
      )}
    </Modal>
  );
}
