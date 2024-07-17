import React from 'react';
import logo from '../assets/images/logo.png';

function Header() {
  return (
    <header className="App-header">
      <div className="header-content">
        <img src={logo} alt="dsOMOP Logo" className="logo" />
        <div className="title-container">
          <h1>dsOMOP</h1>
          <p className="subtitle">Bridging OMOP CDM and DataSHIELD for Secure Federated Analysis of Standardized Clinical Data</p>
        </div>
      </div>
    </header>
  );
}

export default Header;