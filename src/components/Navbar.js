import React, { useState } from 'react';
import { Link } from 'react-router-dom';
import { FaHome, FaBox, FaUsers, FaFileAlt, FaHandsHelping, FaBars } from 'react-icons/fa';

function Navbar() {
  const [isOpen, setIsOpen] = useState(false);

  const toggleMenu = () => {
    setIsOpen(!isOpen);
  };

  return (
    <nav className="navbar">
      <button className="navbar-toggle" onClick={toggleMenu}>
        <FaBars />
      </button>
      <ul className={`navbar-menu ${isOpen ? 'open' : ''}`}>
        <li><Link to="/" onClick={toggleMenu}><FaHome /> About</Link></li>
        <li><Link to="/packages" onClick={toggleMenu}><FaBox /> Packages</Link></li>
        <li><Link to="/authors" onClick={toggleMenu}><FaUsers /> Authors</Link></li>
        <li><Link to="/license" onClick={toggleMenu}><FaFileAlt /> License</Link></li>
        <li><Link to="/acknowledgements" onClick={toggleMenu}><FaHandsHelping /> Acknowledgements</Link></li>
      </ul>
    </nav>
  );
}

export default Navbar;