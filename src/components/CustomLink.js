import React from 'react';
import { Link, useLocation } from 'react-router-dom';

const CustomLink = ({ children, to, setIsOpen, ...props }) => {
  const location = useLocation();

  const handleClick = (e) => {
    if (to === location.pathname) {
      e.preventDefault();
    }

    // Perform the scroll action
    const mainElement = document.querySelector('main');
    if (mainElement) {
      mainElement.scrollIntoView({ behavior: 'smooth', block: 'start' });
    }

    // Close the dropdown menu
    if (setIsOpen) {
      setIsOpen(false);
    }

    // Call the original onClick handler if provided
    if (props.onClick) {
      props.onClick(e);
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