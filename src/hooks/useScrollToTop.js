import { useEffect, useRef } from 'react';
import { useLocation } from 'react-router-dom';

const useScrollToTop = () => {
  const { pathname } = useLocation();
  const isFirstRender = useRef(true);

  useEffect(() => {
    if (isFirstRender.current) {
      isFirstRender.current = false;
      return;
    }

    const mainElement = document.querySelector('main');
    if (mainElement) {
      window.requestAnimationFrame(() => {
        mainElement.scrollIntoView({ behavior: 'smooth', block: 'start' });
      });
    }
  }, [pathname]);
};

export default useScrollToTop;