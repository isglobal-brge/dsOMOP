import React from 'react';
import { Link } from 'react-router-dom';

const CustomLink = ({ children, to, ...props }) => {
  const handleClick = () => {
    if (to.startsWith('/')) {
      const mainElement = document.querySelector('main');
      if (mainElement) {
        mainElement.scrollIntoView({ behavior: 'smooth' });
      }
    }
  };

  if (to.startsWith('http') || to.startsWith('https')) {
    return (
      <a href={to} {...props}>
        {children}
      </a>
    );
  }

  return (
    <Link to={to} onClick={handleClick} {...props}>
      {children}
    </Link>
  );
};

export default CustomLink;