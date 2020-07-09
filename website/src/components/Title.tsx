import React, { useEffect } from 'react';
import { useStaticQuery, graphql } from 'gatsby';

import * as tf from '@tensorflow/tfjs';

const Title: React.FC<{}> = () => {
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

  console.log(modelFile);

  useEffect(() => {
    console.log('test');
    tf.loadLayersModel(modelFile.allFile.edges[0].node.relativePath)
      .then((m) => {
        console.log('Success!');
        const prediction = m.predict(tf.tensor([[0.75, 0.5, 0.25]]));
        (prediction as tf.Tensor).print();
      })
      .catch((e) => console.log(e));
  });

  return <h1>Hello TypeScript!</h1>;
};

export default Title;
