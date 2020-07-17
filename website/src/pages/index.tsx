import React from 'react';
import { PageProps } from 'gatsby';

import ColorPicker from '../components/ColorPicker';

const Home: React.FC<PageProps> = () => {
  return (
    <main>
      <div className="center">
        <h1 className="title">Palette Generator</h1>
      </div>
      <p>
        A color palette generator machine learning model trained from palettes
        from some of the most renowned artists of all time.
      </p>
      <p>Select a color and see what palette the model generates from it.</p>
      <ColorPicker />
    </main>
  );
};

export default Home;
