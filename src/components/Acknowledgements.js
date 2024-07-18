import React, { useState, useEffect } from 'react';
import Confetti from 'react-confetti';

function Acknowledgements() {
  const [showConfetti, setShowConfetti] = useState(true);
  const [windowDimensions, setWindowDimensions] = useState({ width: window.innerWidth, height: document.documentElement.scrollHeight });

  useEffect(() => {
    const timer = setTimeout(() => {
      setShowConfetti(false);
    }, 5000); // The confetti will show for 5 seconds

    const handleResize = () => {
      setWindowDimensions({
        width: window.innerWidth,
        height: document.documentElement.scrollHeight
      });
    };

    window.addEventListener('resize', handleResize);

    return () => {
      clearTimeout(timer);
      window.removeEventListener('resize', handleResize);
    };
  }, []);

  const justifiedTextStyle = {
    textAlign: 'justify',
    textJustify: 'inter-word'
  };

  return (
    <div className="acknowledgements">
      {showConfetti && (
        <div className="confetti-container">
          <Confetti
            width={windowDimensions.width}
            height={windowDimensions.height}
            recycle={false}
            numberOfPieces={200}
          />
        </div>
      )}
      <h2 className="section-title">Acknowledgements</h2>
      <div className="acknowledgements-grid">
        <div className="acknowledgement-card">
          <div className="card-content">
            <p style={justifiedTextStyle}>
              The development of dsOMOP has been supported by the <strong><a href="https://github.com/isglobal-brge/RadGen4COPD" className="highlight-link">RadGen4COPD</a></strong>, <strong><a href="https://www.clinicbarcelona.org/en/projects-and-clinical-assays/detail/p4copd-prediction-prevention-personalized-and-precision-management-of-copd-in-young-adults" className="highlight-link">P4COPD</a></strong>, <strong><a href="https://www.ersnet.org/science-and-research/clinical-research-collaboration-application-programme/cadset-chronic-airway-diseases-early-stratification/" className="highlight-link">CADSET</a></strong>, and <strong><a href="https://datos-cat.github.io/LandingPage" className="highlight-link">DATOS-CAT</a></strong> projects. These collaborations have not only provided essential financial backing but have also affirmed the project's relevance and application in significant research endeavors.
            </p>
          </div>
        </div>
        <div className="acknowledgement-card">
          <div className="card-content">
            <p style={justifiedTextStyle}>
              This project has received funding from the <strong><a href="https://www.ciencia.gob.es/en/" className="highlight-link">Spanish Ministry of Science and Innovation</a></strong> and <strong><a href="https://www.aei.gob.es/en" className="highlight-link">State Research Agency</a></strong> through the <strong>"Centro de Excelencia Severo Ochoa 2019-2023" Program [CEX2018-000806-S]</strong> and <strong><a href="https://www.aei.gob.es/en" className="highlight-link">State Research Agency</a></strong> and <strong><a href="https://ec.europa.eu/regional_policy/funding/erdf_en" className="highlight-link">Fondo Europeo de Desarrollo Regional, UE</a></strong> <strong>(PID2021-122855OB-I00)</strong>, and support from the <strong><a href="https://web.gencat.cat/en/inici/index.html" className="highlight-link">Generalitat de Catalunya</a></strong> through the <strong>CERCA Program</strong> and <strong><a href="https://recercaiuniversitats.gencat.cat/en/inici/" className="highlight-link">Ministry of Research and Universities</a></strong> <strong>(2021 SGR 01563)</strong>.
            </p>
          </div>
        </div>
        <div className="acknowledgement-card">
          <div className="card-content">
            <p style={justifiedTextStyle}>
              This project has received funding from the <strong>"Complementary Plan for Biotechnology Applied to Health"</strong>, coordinated by the <strong><a href="https://ibecbarcelona.eu/" className="highlight-link">Institut de Bioenginyeria de Catalunya (IBEC)</a></strong> within the framework of the <strong>Recovery, Transformation, and Resilience Plan (C17.I1)</strong> - Funded by the <strong><a href="https://european-union.europa.eu/index_en" className="highlight-link">European Union</a></strong> - <strong><a href="https://next-generation-eu.europa.eu/index_en" className="highlight-link">NextGenerationEU</a></strong>.
            </p>
          </div>
        </div>
        <div className="acknowledgement-card">
          <div className="card-content">
            <p style={justifiedTextStyle}>
              Special thanks to <strong><a href="https://github.com/ESCRI11" className="highlight-link">Xavier Escribà Montagut</a></strong> for his <strong>invaluable</strong> support in the development process.
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}

export default Acknowledgements;