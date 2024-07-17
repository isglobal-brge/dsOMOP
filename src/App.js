import React from 'react';
import { HashRouter as Router, Routes, Route } from 'react-router-dom';
import './App.css';
import Header from './components/Header';
import Navbar from './components/Navbar';
import About from './components/About';
import PackageInfo from './components/PackageInfo';
import Authors from './components/Authors';
import License from './components/License';
import Footer from './components/Footer';
import Acknowledgements from './components/Acknowledgements';
import useScrollToTop from './hooks/useScrollToTop';

function AppContent() {
  useScrollToTop();

  return (
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
  );
}

function App() {
  return (
    <Router>
      <AppContent />
    </Router>
  );
}

export default App;