import { useEffect, useRef } from 'react';
import { useLocation } from 'react-router-dom';

const useScrollToTop = () => {
  const { pathname } = useLocation();
  const prevPathname = useRef(pathname);

  useEffect(() => {
    if (prevPathname.current !== pathname) {
      const mainElement = document.querySelector('main');
      if (mainElement) {
        window.requestAnimationFrame(() => {
          mainElement.scrollIntoView({ behavior: 'smooth', block: 'start' });
        });
      }
    }
    prevPathname.current = pathname;
  }, [pathname]);
};

export default useScrollToTop;