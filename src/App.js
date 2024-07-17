import React, { useEffect } from 'react';
import { BrowserRouter as Router, Route, Routes, useLocation } from 'react-router-dom';
import './App.css';
import Header from './components/Header';
import Navbar from './components/Navbar';
import About from './components/About';
import PackageInfo from './components/PackageInfo';
import Authors from './components/Authors';
import License from './components/License';
import Footer from './components/Footer';
import Acknowledgements from './components/Acknowledgements';

function TitleUpdater() {
  const location = useLocation();
  const pageName = location.pathname.slice(1) || 'About';
  React.useEffect(() => {
    document.title = `dsOMOP - ${pageName.charAt(0).toUpperCase() + pageName.slice(1)}`;
  }, [pageName]);
  return null;
}

function App() {
  useEffect(() => {
    const link = document.querySelector("link[rel~='icon']");
    if (link) {
      link.href = process.env.PUBLIC_URL + '/favicon.ico?v=' + new Date().getTime();
    }
  }, []);

  return (
    <Router>
      <TitleUpdater />
      <div className="App">
        <Header />
        <Navbar />
        <main>
          <Routes>
            <Route path="/" element={<About />} />
            <Route path="/packages" element={<PackageInfo />} />
            <Route path="/authors" element={<Authors />} />
            <Route path="/license" element={<License />} />
            <Route path="/acknowledgements" element={<Acknowledgements />} />
          </Routes>
        </main>
        <Footer />
      </div>
    </Router>
  );
}

export default App;