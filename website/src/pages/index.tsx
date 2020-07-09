import React, { useEffect } from 'react';
import { PageProps, useStaticQuery, graphql } from 'gatsby';

import * as tf from '@tensorflow/tfjs';
import Title from '../components/Title';

const Home: React.FC<PageProps> = () => {
  const modelFile = useStaticQuery(graphql`
    {
      allFile(filter: { name: { eq: "model" } }) {
        edges {
          node {
            name
            publicURL
            relativePath
          }
        }
      }
    }
  `);

  useEffect(() => {
    tf.loadLayersModel(modelFile.allFile.edges[0].node.relativePath)
      .then((m) => {
        const prediction = m.predict(tf.tensor([[0.75, 0.5, 0.25]]));
        (prediction as tf.Tensor).print();
      })
      .catch((e) => console.log(e));
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
