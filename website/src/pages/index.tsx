import React from 'react';
import { PageProps } from 'gatsby';
import * as tf from '@tensorflow/tfjs';

import Title from '@/components/Title';
import model from '../model/model.json';

const Home: React.FC<PageProps> = () => {
  tf.loadLayersModel(model.toString()).then((m) => {
    const prediction = m.predict(tf.tensor1d([0.75, 0.5, 0.25], 'float32'));
    console.log(prediction);
  });

  return (
    <main>
      <Title />
      <p>A TypeScript starter for Gatsby. Great for advanced users.</p>
      <p>
        Follow me on Twitter (
        <a href="https://twitter.com/jpedroschmitz">@jpedroschmitz</a>)
      </p>
    </main>
  );
};

export default Home;
