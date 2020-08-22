import React from 'react';
import { PageProps } from 'gatsby';

import ColorPicker from '../components/ColorPicker';

const Home: React.FC<PageProps> = () => {
  return (
    <main className="main">
      <div className="center">
        <h1 className="title">Palette Generator</h1>
      </div>
      <div className="divider" />
      <p className="explanation">
        A machine learning model that generates a color palette, trained on the
        following{' '}
        <a href="https://www.kaggle.com/ikarus777/best-artworks-of-all-time">
          dataset
        </a>
        .
      </p>
      <ColorPicker />
    </main>
  );
};

export default Home;
