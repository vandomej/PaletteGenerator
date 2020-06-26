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

# print(json.dumps(json_data, indent=4))