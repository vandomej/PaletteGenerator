import React, { useState, useEffect } from 'react';
import { useStaticQuery, graphql } from 'gatsby';
import { ChromePicker, BlockPicker } from 'react-color';

import * as tf from '@tensorflow/tfjs';

const ColorPicker: React.FC<{}> = () => {
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

  const [model, setModel] = useState(null);
  const [colors, setColors] = useState([
    { r: 50, g: 0, b: 100 },
    { r: 100, g: 50, b: 0 },
    { r: 0, g: 100, b: 50 },
    { r: 100, g: 0, b: 50 },
  ]);

  useEffect(() => {
    if (!model) {
      tf.loadLayersModel(modelFile.allFile.edges[0].node.relativePath)
        .then((m) => {
          console.log('Setting model ');
          setModel(m);
          //   const prediction = m.predict(tf.tensor([[0.75, 0.5, 0.25]]));
          //   (prediction as tf.Tensor).print();
        })
        .catch((e) => console.log(e));
    }
  });

  //   console.log(`colors: ${colors[0].r} ${colors[0].g} ${colors[0].b}`);

  return (
    <div className="colors">
      <div className="selector-area">
        <p>Select a color and see what color palette gets generated from it.</p>
        <ChromePicker
          className="color-picker"
          disableAlpha={true}
          color={colors[0]}
          width="300px"
          onChange={(color, event) => {
            setColors([color.rgb, colors[1], colors[2], colors[3]]);
          }}
          onChangeComplete={(color, event) => {
            if (model) {
              const normalized = [color.rgb.r, color.rgb.g, color.rgb.b];
              const prediction = model.predict(
                tf.tensor([normalized.map((i) => i / 255)]),
              );
              const palette = prediction.mul(tf.scalar(255));

              palette.array().then((p) => {
                setColors([
                  color.rgb,
                  { r: p[0][0], g: p[0][1], b: p[0][2] },
                  { r: p[0][3], g: p[0][4], b: p[0][5] },
                  { r: p[0][6], g: p[0][7], b: p[0][8] },
                ]);
              });
            }
          }}
        />
      </div>
      <div className="color-grid">
        <div className="color-grid-row">
          <BlockPicker
            className="color-primary"
            colors={[]}
            triangle="hide"
            color={colors[0]}
          />
          <BlockPicker
            className="color-secondary"
            colors={[]}
            triangle="hide"
            color={colors[1]}
          />
        </div>
        <div className="color-grid-row">
          <BlockPicker
            className="color-tertiary"
            colors={[]}
            triangle="hide"
            color={colors[2]}
          />
          <BlockPicker
            className="color-quaternary"
            colors={[]}
            triangle="hide"
            color={colors[3]}
          />
        </div>
      </div>
    </div>
  );
};

export default ColorPicker;
