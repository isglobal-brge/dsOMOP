import React, { useState, useEffect, useRef } from 'react';
import CustomLink from './CustomLink';
import { FaHome, FaBox, FaUsers, FaFileAlt, FaHandsHelping, FaBars, FaBook } from 'react-icons/fa';

function Navbar() {
  const [isOpen, setIsOpen] = useState(false);
  const menuRef = useRef(null);

  const toggleMenu = () => {
    setIsOpen(!isOpen);
  };

  useEffect(() => {
    const handleClickOutside = (event) => {
      if (menuRef.current && !menuRef.current.contains(event.target)) {
        setIsOpen(false);
      }
    };

    document.addEventListener('mousedown', handleClickOutside);
    return () => {
      document.removeEventListener('mousedown', handleClickOutside);
    };
  }, []);

  return (
    <nav className="navbar" ref={menuRef}>
      <button className={`navbar-toggle ${isOpen ? 'open' : ''}`} onClick={toggleMenu}>
        <FaBars style={{ color: isOpen ? 'orange' : 'white' }} />
      </button>
      <ul className={`navbar-menu ${isOpen ? 'open' : ''}`}>
        <li><CustomLink to="/" setIsOpen={setIsOpen}><FaHome /> About</CustomLink></li>
        <li><CustomLink to="/packages" setIsOpen={setIsOpen}><FaBox /> Packages</CustomLink></li>
        <li><CustomLink to="/authors" setIsOpen={setIsOpen}><FaUsers /> Authors</CustomLink></li>
        <li>
          <a 
            href="https://doi.org/10.1093/bioinformatics/btaf286" 
            target="_blank" 
            rel="noopener noreferrer"
            onClick={() => setIsOpen(false)}
          >
            <FaBook /> Paper
          </a>
        </li>
        <li><CustomLink to="/license" setIsOpen={setIsOpen}><FaFileAlt /> License</CustomLink></li>
        <li><CustomLink to="/acknowledgements" setIsOpen={setIsOpen}><FaHandsHelping /> Acknowledgements</CustomLink></li>
      </ul>
    </nav>
  );
}

export default Navbar;