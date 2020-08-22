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
            relativePath
          }
        }
      }
    }
  `);

  const [model, setModel] = useState(null);
  const [colors, setColors] = useState([
    { r: 104, g: 85, b: 170 },
    { r: 132, g: 76, b: 165 },
    { r: 247, g: 246, b: 107 },
    { r: 247, g: 222, b: 107 },
  ]);

  //Loading the machine learning model using tensorflowjs from the website.
  useEffect(() => {
    if (!model) {
      tf.loadLayersModel(modelFile.allFile.edges[0].node.relativePath)
        .then((m) => {
          setModel(m);
        })
        .catch((e) => console.log(e));
    }
  });

  return (
    <div className="colors">
      <div className="color-picker">
        <p>Select a color</p>
        <ChromePicker
          disableAlpha={true}
          color={colors[0]}
          onChange={(color) => {
            setColors([color.rgb, colors[1], colors[2], colors[3]]);
          }}
          onChangeComplete={(color) => {
            if (model) {
              const input = tf
                .tensor([[color.rgb.r, color.rgb.g, color.rgb.b]])
                .div(255);
              const prediction = model.predict(input);
              const palette = prediction.mul(tf.scalar(255));

              // const normalized = [color.rgb.r, color.rgb.g, color.rgb.b];
              // const prediction = model.predict(
              //   tf.tensor([normalized.map((i) => i / 255)]),
              // );
              // const palette = prediction.mul(tf.scalar(255));

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
      <div className="result-area">
        <p>And see the resulting palette</p>
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
    </div>
  );
};

export default ColorPicker;
