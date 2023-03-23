import Navbar from "./components/navbar";
import Home from "./pages_components/home";

import Configuration from "./pages_components/configuration";
import Registration from "./pages_components/registration";
import NSFs from "./pages_components/NSFs";
import './App.css'
import React, { useState, useEffect } from 'react';

function App() {
  const [mode, setMode] = useState(() => {
    // Use local storage to read the current mode, or default to "light"
    return localStorage.getItem('mode') || 'light';
  });
  function toggleMode() {
    setMode(mode === 'light' ? 'dark' : 'light');
  }

  let component 
  switch (window.location.pathname) {
    case '/home':
      component = <Home />
      break;
    case '/registration':
      component = < Registration mode={mode}/>;
      break;
    case '/configuration':
      component = <Configuration mode={mode}/>;
      break;
    case '/NSFs':
      component = <NSFs mode={mode} />;
      break;
  }

  useEffect(() => {
    localStorage.setItem('mode', mode);
    document.body.className = mode;
  }, [mode]);

  return (
    <div className={`App ${mode}`}> 
      <Navbar />
      <button className={mode === 'dark' ? 'dark-toggle' : 'light-toggle'}  onClick={toggleMode}>{mode === 'dark' ? 'Light-Mode' : 'Dark-Mode'}</button>
      <div className={mode}>{component}</div>
    </div>
  )
}

export default App;
