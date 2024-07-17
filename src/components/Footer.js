import React from 'react';
import CustomLink from './CustomLink';
import { FaHeart } from 'react-icons/fa';
import brgeLogo from '../assets/images/brge-logo.png';
import isglobalLogo from '../assets/images/isglobal-logo.png';

function Footer() {
  return (
    <footer className="footer">
      <div className="footer-content">
        <div className="footer-section">
          <div className="footer-logos">
            <a href="https://brge.isglobal.org/" target="_blank" rel="noopener noreferrer">
              <img src={brgeLogo} alt="BRGE Logo" className="footer-logo" />
            </a>
            <a href="https://www.isglobal.org/" target="_blank" rel="noopener noreferrer">
              <img src={isglobalLogo} alt="ISGlobal Logo" className="footer-logo" />
            </a>
          </div>
        </div>
        <div className="footer-section footer-made-by">
          <a href="https://github.com/davidsarratgonzalez" target="_blank" rel="noopener noreferrer" className="footer-link">
            Website made with 🧡 by David Sarrat González
          </a>
        </div>
        <div className="footer-section">
          <div className="footer-copyright">
            © {new Date().getFullYear()} dsOMOP.<br />
            This project is licensed under the <CustomLink to="/license" className="license-link"><strong>MIT License</strong></CustomLink>.
          </div>
        </div>
      </div>
    </footer>
  );
}

export default Footer;