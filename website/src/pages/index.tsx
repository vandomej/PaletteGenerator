import React from 'react';
import { PageProps } from 'gatsby';
import * as tf from '@tensorflow/tfjs';

import Title from '@/components/Title';
import model from '../../public/model.json';

const Home: React.FC<PageProps> = () => {
  tf.loadLayersModel('http://localhost:8000/model.json').then((m) => {
    const prediction = m.predict(tf.tensor([[0.75, 0.5, 0.25]]));
    (prediction as tf.Tensor).print();
  });

  return (
    <main>
      <Title />
      <p>A TypeScript starter for Gatsby. Great for advanced users.</p>
      <p>Follow me on Twitter</p>
    </main>
  );
};

export default Home;
