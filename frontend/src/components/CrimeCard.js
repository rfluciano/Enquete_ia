import React from 'react';

export default function CrimeCard({ crime, selected, onSelect }) {
  return (
    <div onClick={onSelect} style={{
      border: selected ? '2px solid #2b7' : '1px solid #ddd',
      borderRadius:8,
      padding:10,
      cursor:'pointer',
      width:160,
      textAlign:'center',
      boxShadow: selected ? '0 2px 8px rgba(43,127,70,0.12)' : 'none'
    }}>
      <img src={crime.img} alt={crime.label} style={{width:80, height:80, objectFit:'cover'}} />
      <div style={{marginTop:8}}>{crime.label}</div>
    </div>
  );
}