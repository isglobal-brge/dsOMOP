import React, { useEffect, useState } from 'react';

function Acknowledgements() {
  const [isMobile, setIsMobile] = useState(window.innerWidth <= 768);

  useEffect(() => {
    const handleResize = () => {
      setIsMobile(window.innerWidth <= 768);
    };

    if (window.confetti) {
      const fireConfetti = () => {
        const width = window.innerWidth;
        const height = window.innerHeight;

        // Adjust particleCount and spread based on screen size
        const particleCount = Math.min(200, Math.max(30, (width * height) / 5000));
        const spread = isMobile 
          ? Math.min(60, Math.max(30, (width + height) / 30)) // Reduced spread for mobile
          : Math.min(120, Math.max(60, (width + height) / 20));

        // Calculate angles and origins based on screen orientation
        let leftAngle, rightAngle, leftOrigin, rightOrigin;

        if (isMobile) {
          // Aim for the middle of the screen on mobile
          leftAngle = Math.atan2(height / 2, width / 2) * (180 / Math.PI);
          rightAngle = 180 - leftAngle;
          leftOrigin = { x: 0, y: 1 };
          rightOrigin = { x: 1, y: 1 };
        } else {
          // Aim for the exact bottom corners on larger screens
          leftAngle = 60;
          rightAngle = 120;
          leftOrigin = { x: 0, y: 1 };
          rightOrigin = { x: 1, y: 1 };
        }

        // Left cannon
        window.confetti({
          particleCount: particleCount,
          angle: leftAngle,
          spread: spread,
          origin: leftOrigin,
          startVelocity: 45,
          zIndex: 10000,
        });

        // Right cannon (delayed)
        setTimeout(() => {
          window.confetti({
            particleCount: particleCount,
            angle: rightAngle,
            spread: spread,
            origin: rightOrigin,
            startVelocity: 45,
            zIndex: 10000,
          });
        }, 250); // 250ms delay
      };

      fireConfetti();
      window.addEventListener('resize', handleResize);

      return () => window.removeEventListener('resize', handleResize);
    }
  }, [isMobile]);

  const justifiedTextStyle = {
    textAlign: 'justify',
    textJustify: 'inter-word'
  };

  return (
    <div className="acknowledgements">
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