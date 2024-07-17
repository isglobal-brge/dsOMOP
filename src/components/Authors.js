import React from 'react';
import { FaGithub, FaLinkedin, FaEnvelope } from 'react-icons/fa';
import { SiOrcid } from 'react-icons/si';
import davidPhoto from '../assets/images/david.jpeg';
import juanrPhoto from '../assets/images/juanr.jpg';
import xaviPhoto from '../assets/images/xavi.jpeg';

function Authors() {
  const authors = [
    {
      name: 'David Sarrat González',
      photo: davidPhoto,
      links: {
        github: 'https://github.com/davidsarratgonzalez',
        linkedin: 'https://www.linkedin.com/in/davidsarratgonzalez/',
        orcid: 'https://orcid.org/0000-0002-9064-3303',
        email: 'david.sarrat@isglobal.org'
      }
    },
    {
      name: 'Juan R González',
      photo: juanrPhoto,
      links: {
        github: 'https://github.com/isglobal-brge',
        linkedin: 'https://www.linkedin.com/in/juan-r-gonzalez-50a808171/',
        orcid: 'https://orcid.org/0000-0003-3267-2146',
        email: 'juanr.gonzalez@isglobal.org'
      }
    },
    {
      name: 'Xavier Escribà-Montagut',
      photo: xaviPhoto,
      links: {
        github: 'https://github.com/ESCRI11',
        linkedin: 'https://www.linkedin.com/in/xavier-escriba-montagut/',
        orcid: 'https://orcid.org/0000-0003-2888-8948'
      }
    }
  ];

  return (
    <div className="authors">
      <h2>Authors</h2>
      <div className="authors-grid">
        {authors.map((author, index) => (
          <div key={index} className="author-card">
            <img src={author.photo} alt={author.name} className="author-photo" />
            <h3>{author.name}</h3>
            <div className="author-links">
              {author.links.github && (
                <a href={author.links.github} target="_blank" rel="noopener noreferrer">
                  <FaGithub />
                </a>
              )}
              {author.links.linkedin && (
                <a href={author.links.linkedin} target="_blank" rel="noopener noreferrer">
                  <FaLinkedin />
                </a>
              )}
              {author.links.orcid && (
                <a href={author.links.orcid} target="_blank" rel="noopener noreferrer">
                  <SiOrcid />
                </a>
              )}
              {author.links.email && (
                <a href={`mailto:${author.links.email}`}>
                  <FaEnvelope />
                </a>
              )}
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}

export default Authors;