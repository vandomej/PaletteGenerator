import os
import json
import matplotlib.pyplot as plt
import numpy as py

import tensorflow as tf
import pandas as pd
import seaborn as sns

from tensorflow import keras
from tensorflow.keras import layers

import tensorflow_docs as tfdocs
import tensorflow_docs.plots
import tensorflow_docs.modeling

with open('data.json', 'r') as file:
    file_data = file.read()

json_data = json.loads(file_data)

# cleaning and preparing the data
json_data = [row[0:4] for row in json_data if len(row) >= 4]

input_json = []
for jd in json_data:

    input_json.append(
        {
            "primaryr":       jd[0]['r'],
            "primaryg":       jd[0]['g'],
            "primaryb":       jd[0]['b'],
            "secondaryr":     jd[1]['r'],
            "secondaryg":     jd[1]['g'],
            "secondaryb":     jd[1]['b'],
            "tertiaryr":      jd[2]['r'],
            "tertiaryg":      jd[2]['g'],
            "tertiaryb":      jd[2]['b'],
            "quaternaryr":    jd[3]['r'],
            "quaternaryg":    jd[3]['g'],
            "quaternaryb":    jd[3]['b']
        }
    )

# print(json.dumps(input_json))

raw_dataset = pd.read_json(json.dumps(input_json), orient='records')

dataset = raw_dataset.copy()

# print(dataset.tail())

train_dataset = dataset.sample(frac=0.8, random_state=0)
test_dataset = dataset.drop(train_dataset.index)

# sns.pairplot(
#     train_dataset[["primaryr", "primaryg", "primaryb"]], diag_kind="kde")
# plt.show()

train_stats = train_dataset.describe()
train_stats = train_stats.drop(
    labels=['secondaryr', 'secondaryg', 'secondaryb', 'tertiaryr', 'tertiaryg', 'tertiaryb', 'quaternaryr', 'quaternaryg', 'quaternaryb'], axis='columns')
train_stats = train_stats.transpose()

train_label_stats = train_dataset.describe()
train_label_stats = train_label_stats.drop(
    labels=['primaryr', 'primaryg', 'primaryb'], axis='columns')
train_label_stats = train_label_stats.transpose()

# print(train_label_stats)

train_labels = train_dataset.drop(
    labels=['primaryr', 'primaryg', 'primaryb'], axis='columns')
train_dataset = train_dataset.drop(
    labels=['secondaryr', 'secondaryg', 'secondaryb', 'tertiaryr', 'tertiaryg', 'tertiaryb', 'quaternaryr', 'quaternaryg', 'quaternaryb'], axis='columns')

test_labels = test_dataset.drop(
    labels=['primaryr', 'primaryg', 'primaryb'], axis='columns')
test_dataset = test_dataset.drop(
    labels=['secondaryr', 'secondaryg', 'secondaryb', 'tertiaryr', 'tertiaryg', 'tertiaryb', 'quaternaryr', 'quaternaryg', 'quaternaryb'], axis='columns')

# print(test_labels.tail())
# print(test_dataset.tail())


def norm_data(x):
    return x / 255


def norm_labels(x):
    return x / 255


normed_train_data = norm_data(train_dataset)
normed_test_data = norm_data(test_dataset)

normed_train_labels = norm_labels(train_labels)
normed_test_labels = norm_labels(test_labels)

# print(normed_train_labels.tail())

# Model results
# Layers        | best val_mae
# 64 | 9            .1537
# 24 | 9            .1535
# 16 | 9            .1536
# 8  | 9            .1548
# 12 | 9            .1536
# 10 | 9            .1543
# 11 | 9            .1547
# 12 | 12 | 9       .1533
# 12 | 6 | 9        .1530
# 12 | 24 | 9       .1534
# 12 | 9 | 9        .1535
# 12 | 10 | 9       .1529
# 24 | 10 | 9       .1533
# 16 | 10 | 9       .1528
# 18 | 10 | 9       .1530
# 16 | 10 | 10 | 9  .1528
# 16 | 10 | 20 | 9  .1536
# 16 | 10 | 5 | 9   .1532
# 16 | 10 | 8 | 9   .1531
# 16 | 8 | 10 | 9   .1529
# 16 | 16 | 10 | 9  .1529
# 24 | 10 | 10 | 9  .1520 !!
# 24 | 16 | 10 | 9  .1530
# 24 | 10 | 10 | 9  .1525


def build_model():
    model = keras.Sequential([
        layers.Dense(24, activation='relu', input_shape=[
                     len(train_dataset.keys())], use_bias=True),
        layers.Dense(10, use_bias=True),
        layers.Dense(10, use_bias=True),
        layers.Dense(len(train_labels.keys()), use_bias=True)
    ])

    lr_schedule = tf.keras.optimizers.schedules.InverseTimeDecay(
        0.001,
        decay_steps=(len(normed_train_data) / 32)*1000,
        decay_rate=1,
        staircase=False)

    model.compile(loss='mse',
                  # tf.keras.optimizers.RMSprop(lr_schedule),
                  optimizer='rmsprop',
                  metrics=['mae', 'mape'])
    return model


model = build_model()

model.summary()

# example_batch = normed_train_data[:10]
# example_result = model.predict(example_batch)
# print(example_result)

checkpoint_path = "models/cp.ckpt"
checkpoint_dir = os.path.dirname(checkpoint_path)

# Create a callback that saves the model's weights
cp_callback = tf.keras.callbacks.ModelCheckpoint(filepath=checkpoint_path,
                                                 save_weights_only=True,
                                                 verbose=1,
                                                 period=5)

EPOCHS = 400

history = model.fit(
    normed_train_data, normed_train_labels,
    epochs=EPOCHS, validation_data=(
        normed_test_data, normed_test_labels), verbose=0,
    shuffle=True,
    callbacks=[
        tfdocs.modeling.EpochDots(),
        tf.keras.callbacks.EarlyStopping(monitor='val_mae', patience=100),
        cp_callback
    ])

hist = pd.DataFrame(history.history)
hist['epoch'] = history.epoch
print(hist.tail())

plotter = tfdocs.plots.HistoryPlotter(smoothing_std=2)
plotter.plot({'Basic': history}, metric="mae")
plt.ylim([0.15, 0.18])
plt.ylabel('MAE [RGB]')
plt.savefig("results.png")


loss, mae, mape = model.evaluate(
    normed_test_data, normed_test_labels, verbose=2)

print("Testing set Mean Abs Error: {:5.2f}".format(mae))
print("Testing set MAPE Error: {}".format(mape))

test_predictions = model.predict(normed_test_data).flatten()

model.save('models/final.h5')


#
# Copyright (c) 2017 François Chollet
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
# THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.
