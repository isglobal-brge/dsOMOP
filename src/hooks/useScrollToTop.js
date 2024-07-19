import { useEffect, useRef } from 'react';
import { useLocation } from 'react-router-dom';

const useScrollToTop = () => {
  const { pathname } = useLocation();
  const prevPathname = useRef(pathname);

  useEffect(() => {
    if (prevPathname.current !== pathname) {
      const navbarAnchor = document.getElementById('navbar-anchor');
      if (navbarAnchor) {
        window.requestAnimationFrame(() => {
          navbarAnchor.scrollIntoView({ behavior: 'smooth', block: 'start' });
        });
      }
    }
    prevPathname.current = pathname;
  }, [pathname]);
};

export default useScrollToTop;