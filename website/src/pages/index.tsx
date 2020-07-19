import React from 'react';
import { PageProps } from 'gatsby';

import ColorPicker from '../components/ColorPicker';

const Home: React.FC<PageProps> = () => {
  return (
    <main className="main">
      <div className="center main-item">
        <h1 className="title">Palette Generator</h1>
      </div>
      <div className="divider" />
      <p className="main-item explanation">
        A color palette generator machine learning model trained from palettes
        from some of the most renowned artists of all time.
      </p>
      <ColorPicker />
    </main>
  );
};

export default Home;
