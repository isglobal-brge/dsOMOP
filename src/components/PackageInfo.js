import React from 'react';
import { FaGithub, FaBook } from 'react-icons/fa';

function PackageInfo() {
  const packages = [
    {
      name: 'dsOMOP',
      subtitle: 'Server-Side DataSHIELD Integration for OMOP CDM Databases',
      description: 'This package facilitates interaction with remote databases in the OMOP CDM format from a DataSHIELD environment. It is responsible for fetching and transforming data from databases into a user-intelligible table format, integrated into the DataSHIELD workflow to ensure compliance with the DataSHIELD security model.',
      github: 'https://github.com/isglobal-brge/dsOMOP',
    },
    {
      name: 'dsOMOPClient',
      subtitle: 'Client-Side DataSHIELD Integration for OMOP CDM Databases',
      description: 'This package facilitates interaction with remote databases in the OMOP CDM format from a DataSHIELD environment. It enables users to invoke server-side functions that perform fetching and transforming of data from OMOP CDM databases, integrating these operations into the DataSHIELD workflow to maintain adherence to the DataSHIELD security model.',
      github: 'https://github.com/isglobal-brge/dsOMOPClient',
      userGuide: 'https://isglobal-brge.github.io/dsOMOPClient/',
    },
    {
      name: 'dsOMOPHelper',
      subtitle: 'dsOMOP Helper Functions',
      description: 'This package provides a set of functions to help the user to work with the dsOMOPClient package in DataSHIELD. It provides plug-and-play functionalities for data selection and fetching, streamlining interactions with most simple use cases.',
      github: 'https://github.com/isglobal-brge/dsOMOPHelper',
      userGuide: 'https://isglobal-brge.github.io/dsOMOPHelper/',
    },
    {
      name: 'dsOMOP.oracle',
      subtitle: 'dsOMOP Oracle Extension',
      description: 'Extends the functionality of the dsOMOP package to support OMOP CDM databases in Oracle. Requires the oracle.resourcer package.',
      github: 'https://github.com/isglobal-brge/dsOMOP.oracle',
    },
  ];

  return (
    <div className="package-info">
      <h2>Packages</h2>
      <div className="package-grid">
        {packages.map((pkg, index) => (
          <div key={index} className="package-item">
            <h3>{pkg.name}</h3>
            <p className="package-description">{pkg.subtitle}</p>
            <p className="package-long-description">{pkg.description}</p>
            <div className="package-links">
              <a href={pkg.github} target="_blank" rel="noopener noreferrer" className="btn github-btn">
                <FaGithub /> GitHub
              </a>
              {pkg.userGuide && (
                <a href={pkg.userGuide} target="_blank" rel="noopener noreferrer" className="btn pkgdown-btn">
                  <FaBook /> User guide
                </a>
              )}
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}

export default PackageInfo;