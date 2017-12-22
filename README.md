# py
py
import pandas as pd
import numpy  as np
import matplotlib.pyplot as plt
from sklearn.cross_validation import train_test_split
from sklearn.metrics import accuracy_score
from sklearn.metrics import roc_auc_score
from sklearn.metrics import roc_curve
from sklearn.metrics import classification_report


data = pd.read_csv(Location);
print(data.head)

print(data['student'])

def encode_target(data):
    data['student'] = [0 if student=='No' else 1 for student in data.student]
    data['default'] = [0 if default=='No' else 1 for default in data.default]
    return
