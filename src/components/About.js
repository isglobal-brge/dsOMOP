import React from 'react';
import { Link } from 'react-router-dom';
import { FaDatabase, FaShieldAlt, FaLock, FaExchangeAlt, FaPuzzlePiece, FaSearch, FaUser, FaServer, FaFilter, FaChartBar, FaCheckCircle, FaArrowRight, FaCogs, FaUserShield } from 'react-icons/fa';

function About() {
  const handleGetStartedClick = () => {
    const mainElement = document.querySelector('main');
    if (mainElement) {
      mainElement.scrollIntoView({ behavior: 'smooth' });
    }
  };

  return (
    <div className="about">
      <h2 className="section-title">About dsOMOP</h2>
      
      <div className="about-section">
        <h3>What is dsOMOP?</h3>
        <p>
          dsOMOP is an innovative, open-source project designed to bridge the gap between Observational Medical Outcomes Partnership Common Data Model (OMOP CDM) databases and the DataSHIELD environment. It was developed by the <strong><a href="https://brge.isglobal.org/" target="_blank" rel="noopener noreferrer" className="highlight-link">Bioinformatics Research Group in Epidemiology (BRGE)</a></strong> at the <strong><a href="https://www.isglobal.org/" target="_blank" rel="noopener noreferrer" className="highlight-link">Barcelona Institute for Global Health (ISGlobal)</a></strong>.
        </p>
      </div>

      <div className="about-section">
        <h3>What are the OMOP CDM and DataSHIELD?</h3>
        <div className="card-container">
          <div className="card">
            <FaDatabase className="card-icon" />
            <h4>OMOP CDM</h4>
            <p>The Observational Medical Outcomes Partnership Common Data Model is a standardized format for healthcare data, facilitating efficient analysis and collaboration across different institutions.</p>
            <a href="https://www.ohdsi.org/data-standardization/" target="_blank" rel="noopener noreferrer">Learn more</a>
          </div>

          <div className="card">
            <FaShieldAlt className="card-icon" />
            <h4>DataSHIELD</h4>
            <p>A software solution that enables the analysis of sensitive data without physical data transfer, ensuring data privacy and security in collaborative research.</p>
            <a href="https://www.datashield.org/" target="_blank" rel="noopener noreferrer">Learn more</a>
          </div>
        </div>
      </div>

      <div className="about-section">
        <h3>What is the goal of dsOMOP?</h3>
        <p>
          The primary goal of dsOMOP is to enable secure and privacy-preserving analysis of sensitive medical data across multiple institutions without the need for physical data transfer. This approach facilitates collaborative research while maintaining strict data protection standards, a critical requirement in the field of medical research. By integrating OMOP CDM with DataSHIELD, dsOMOP allows researchers to perform standardized analyses on harmonized data from various sources, enhancing the power and scope of epidemiological studies.
        </p>
      </div>

      <div className="about-section how-it-works-section">
        <h3>How does dsOMOP work?</h3>
        <p>
          dsOMOP operates as a middleware between OMOP CDM databases and the DataSHIELD environment, facilitating secure and privacy-preserving analysis of sensitive medical data. The process involves several steps, with the user driving the data selection and tailoring to meet their specific research needs:
        </p>
        <div className="dsomop-workflow-cards">
          <div className="card process-card">
            <FaUser className="card-icon" />
            <h4>1. User Request</h4>
            <p>The user initiates a request through the DataSHIELD client, specifying the data they need and the analysis to be performed.</p>
          </div>
          <div className="card process-card">
            <FaServer className="card-icon" />
            <h4>2. Server Processing</h4>
            <p>The dsOMOP server receives the request and prepares to interact with the OMOP CDM database.</p>
          </div>
          <div className="card process-card">
            <FaDatabase className="card-icon" />
            <h4>3. Database Interaction</h4>
            <p>dsOMOP connects to the OMOP CDM database and executes SQL queries based on the user's specifications.</p>
          </div>
          <div className="card process-card">
            <FaFilter className="card-icon" />
            <h4>4. Data Processing</h4>
            <p>Retrieved data is transformed into a format compatible with DataSHIELD's analytical tools.</p>
          </div>
          <div className="card process-card">
            <FaUserShield className="card-icon" />
            <h4>5. Privacy Checks</h4>
            <p>Disclosure control measures are applied to ensure data privacy and compliance with DataSHIELD's security model.</p>
          </div>
          <div className="card process-card">
            <FaChartBar className="card-icon" />
            <h4>6. Analysis</h4>
            <p>The processed data is integrated into DataSHIELD's server-side environment for analysis. Only aggregate results that pass all privacy checks are returned to the user's client interface.</p>
          </div>
        </div>
      </div>

      <div className="about-section">
        <h3>What are the key features of dsOMOP?</h3>
        <p>
          dsOMOP offers a range of powerful features that make it a valuable tool for researchers working with sensitive medical data across multiple institutions:
        </p>
        <div className="card-container">
          <div className="card feature-card">
            <FaLock className="card-icon" />
            <h4>Enhanced Data Privacy</h4>
            <p>Enables secure analysis of sensitive medical data, adhering to DataSHIELD's strict disclosure control measures and ensuring compliance with data protection regulations.</p>
          </div>
          <div className="card feature-card">
            <FaDatabase className="card-icon" />
            <h4>Multi-Database Compatibility</h4>
            <p>Supports simultaneous analysis across various database management systems and institutions, facilitating standardized and comparable analyses in a distributed manner.</p>
          </div>
          <div className="card feature-card">
            <FaPuzzlePiece className="card-icon" />
            <h4>Extensibility</h4>
            <p>Supports community-driven extensions, allowing for the incorporation of new functionalities and expanding its capabilities to meet evolving research needs.</p>
          </div>
          <div className="card feature-card">
            <FaCogs className="card-icon" />
            <h4>High Customizability</h4>
            <p>Offers flexibility in data selection, dataset formatting, and user experience, allowing researchers to tailor their analyses to specific research requirements while maintaining privacy safeguards.</p>
          </div>
        </div>
      </div>

      <div className="about-section">
        <h3>Ready to get started?</h3>
        <p>If you want to use dsOMOP in your research, you can install our packages now!</p>
        <div className="center-button">
          <Link to="/packages" className="btn-large" onClick={handleGetStartedClick}>
            Get started <FaArrowRight className="btn-icon" />
          </Link>
        </div>
      </div>
    </div>
  );
}

export default About;