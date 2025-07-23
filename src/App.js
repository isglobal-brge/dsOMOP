import React, { useEffect, useRef } from 'react';
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
import { FaTrophy, FaExternalLinkAlt, FaBookOpen, FaDatabase } from 'react-icons/fa';

function PublicationParticles() {
  const particlesRef = useRef(null);

  useEffect(() => {
    if (window.particlesJS && particlesRef.current) {
      window.particlesJS(particlesRef.current.id, {
        particles: {
          number: {
            value: 60,
            density: {
              enable: true,
              value_area: 400
            }
          },
          color: {
            value: '#ffffff'
          },
          shape: {
            type: 'circle',
            stroke: {
              width: 0,
              color: '#000000'
            },
          },
          opacity: {
            value: 0.25,
            random: true,
            anim: {
              enable: true,
              speed: 0.5,
              opacity_min: 0.1,
              sync: false
            }
          },
          size: {
            value: 2,
            random: true,
            anim: {
              enable: false,
              speed: 40,
              size_min: 0.1,
              sync: false
            }
          },
          line_linked: {
            enable: false
          },
          move: {
            enable: true,
            speed: 1,
            direction: 'none',
            random: true,
            straight: false,
            out_mode: 'out',
            bounce: false,
            attract: {
              enable: false,
              rotateX: 600,
              rotateY: 1200
            }
          }
        },
        interactivity: {
          detect_on: 'canvas',
          events: {
            onhover: {
              enable: false
            },
            onclick: {
              enable: false
            },
            resize: true
          }
        },
        retina_detect: true
      });
    }
  }, []);

  return (
    <div
      ref={particlesRef}
      id="publication-particles-js"
      className="publication-particles"
    />
  );
}

function PublicationBanner() {
  return (
    <div style={{ padding: '0 0.5rem' }}>
      <div className="publication-highlight">
        <PublicationParticles />
        <div className="publication-icon">
          <FaTrophy />
        </div>
        <div className="publication-content">
          <h4>Our research has been published!</h4>
          <p className="publication-description">
            We are pleased to announce that our work on dsOMOP has been published in the prestigious journal <strong>Bioinformatics</strong>. This publication represents a significant milestone in advancing secure, federated collaborative research with observational health data, empowering medical institutions worldwide to unite in clinical research while safeguarding patient privacy.
          </p>
          <div className="publication-actions">
            <a 
              href="https://doi.org/10.1093/bioinformatics/btaf286" 
              target="_blank" 
              rel="noopener noreferrer"
              className="publication-btn primary"
            >
              <FaExternalLinkAlt /> Read the paper
            </a>
          </div>
        </div>
      </div>
    </div>
  );
}

function AboutWithBanner() {
  return (
    <>
      <PublicationBanner />
      <main>
        <About />
      </main>
    </>
  );
}

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
      <div id="navbar-anchor"></div>
      <Navbar />
      
      <Routes>
        <Route path="/" element={<AboutWithBanner />} />
        <Route path="/packages" element={<main><PackageInfo /></main>} />
        <Route path="/authors" element={<main><Authors /></main>} />
        <Route path="/license" element={<main><License /></main>} />
        <Route path="/acknowledgements" element={<main><Acknowledgements /></main>} />
      </Routes>
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