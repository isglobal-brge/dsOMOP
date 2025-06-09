import React from 'react';

function Acknowledgements() {
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
              This project has received funding from the <strong><a href="https://www.ciencia.gob.es/en/" className="highlight-link">Spanish Ministry of Education, Innovation and Universities</a></strong>, the <strong><a href="https://www.aei.gob.es/en" className="highlight-link">National Agency for Research</a></strong>, and the <strong><a href="https://ec.europa.eu/regional_policy/funding/erdf_en" className="highlight-link">Fund for Regional Development</a></strong> <strong>(PID2021-122855OB-I00)</strong>. We also acknowledge support from the grant <strong>CEX2023-0001290-S</strong> funded by <strong>MCIN/AEI/10.13039/501100011033</strong>, and support from the <strong><a href="https://web.gencat.cat/en/inici/index.html" className="highlight-link">Generalitat de Catalunya</a></strong> through the <strong><a href="https://cerca.cat/en/" className="highlight-link">CERCA Program</a></strong> and the <strong>Consolidated Group on HEALTH ANALYTICS (2021 SGR 01563)</strong>.
            </p>
          </div>
        </div>
        <div className="acknowledgement-card">
          <div className="card-content">
            <p style={justifiedTextStyle}>
              Additionally, this project has received funding from the <strong><a href="https://www.isciii.es/" className="highlight-link">Instituto de Salud Carlos III (ISCIII)</a></strong> through the project <strong>"PMP21/00090,"</strong> co-funded by the <strong><a href="https://european-union.europa.eu/index_en" className="highlight-link">European Union's</a></strong> <strong>Resilience and Recovery Facility</strong>. It has also been partially funded by the <strong>"Complementary Plan for Biotechnology Applied to Health,"</strong> coordinated by the <strong><a href="https://ibecbarcelona.eu/" className="highlight-link">Institut de Bioenginyeria de Catalunya (IBEC)</a></strong> within the framework of the <strong>Recovery, Transformation, and Resilience Plan (C17.I1)</strong> – Funded by the <strong><a href="https://european-union.europa.eu/index_en" className="highlight-link">European Union</a></strong> – <strong><a href="https://next-generation-eu.europa.eu/index_en" className="highlight-link">NextGenerationEU</a></strong>.
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}

export default Acknowledgements;