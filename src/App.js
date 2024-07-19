import React, { useEffect } from 'react';
import { HashRouter as Router, Routes, Route, useLocation } from 'react-router-dom';
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
  const location = useLocation();

  useEffect(() => {
    const pageName = getPageName(location.pathname);
    document.title = `dsOMOP - ${pageName}`;
  }, [location]);

  const getPageName = (pathname) => {
    switch (pathname) {
      case '/':
        return 'About';
      case '/packages':
        return 'Packages';
      case '/authors':
        return 'Authors';
      case '/license':
        return 'License';
      case '/acknowledgements':
        return 'Acknowledgements';
      default:
        return 'Not Found';
    }
  };

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