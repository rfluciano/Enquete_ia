import React from 'react';
import { createRoot } from 'react-dom/client';
import App from './App';
import 'bootstrap/dist/css/bootstrap.min.css'; // <-- bootstrap
import './index.css';
import Modal from "react-modal";
Modal.setAppElement("#root");

const root = createRoot(document.getElementById('root'));
root.render(<App />);
