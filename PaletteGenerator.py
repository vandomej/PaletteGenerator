import json
import matplotlib.pyplot as plt
import numpy as py

import tensorflow as tf
import pandas as pd
import seaborn as sns

from tensorflow import keras
from tensorflow.keras import layers

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

print(raw_dataset.tail())
