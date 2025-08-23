import React from 'react';

export default function SuspectCard({ suspect, selected, onSelect, onOpenDetails }) {
  return (
    <div style={{
      display:'flex',
      flexDirection:'column',
      alignItems:'center',
      width:160,
      padding:10,
      border: selected ? '2px solid #27f' : '1px solid #ddd',
      borderRadius:8,
      cursor:'pointer',
      gap:8
    }}>
      <img src={suspect.img} alt={suspect.name} style={{width:100,height:100,objectFit:'cover',borderRadius:50}} />
      <strong>{suspect.name}</strong>
      <div style={{display:'flex', gap:8}}>
        <button onClick={onSelect} style={{padding:'6px 8px'}}>Choisir</button>
        <button onClick={onOpenDetails} style={{padding:'6px 8px'}}>Details</button>
      </div>
    </div>
  );
}