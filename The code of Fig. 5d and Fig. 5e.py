#!/usr/bin/env python
# -*- coding:utf-8 -*-
# Author:Mayako
import csv
import numpy as np
from sklearn import metrics
import matplotlib.pyplot as plt
import os


dic={}
with open('all_slide.csv') as f:
    f_csv=csv.reader(f)
    for line in f_csv:
        if int(line[1]) !=0 and int(line[2]) !=0:
            dic[line[0][:15].replace('-','.')]=int(line[1])/(int(line[2])+int(line[1]))

real={}
with open('TCGA-PAM TOP 50-signatures.csv') as rea:
    rea_csv=csv.reader(rea)
    for line in rea_csv:
        real[line[0]]=line[1]

print(dic)
p=0
f=0
y_label=[]
x_score=[]
for i in dic:
    result='WDL'
    if i in real:
        if dic[i]>0.66:
            result='PDL'
        if result==real[i]:
            p+=1
        else:
            f+=1

        if result=='PDL':
            x_score.append(dic[i])
            y_label.append(0)
        else:
            x_score.append((dic[i]))
            y_label.append(1)
def get_curve(file='index.csv'):
    scores = []
    labels = []
    for i in range(len(x_score)):
        scores.append(x_score[i])
        labels.append(y_label[i])
    return np.array(labels),np.array(scores)

def draw_cure(pos_label=0):
    labels, scores=get_curve()
    fpr, tpr, thresholds = metrics.roc_curve(labels, scores, pos_label)
    auc = metrics.auc(fpr, tpr)
    plt.figure()
    lw = 2
    plt.plot(fpr, tpr, color='darkorange',
             lw=lw, label='ROC curve (area = %0.2f)' % auc)
    plt.plot([0, 1], [0, 1], color='navy', lw=lw, linestyle='--')
    plt.xlim([0.0, 1.0])
    plt.ylim([0.0, 1.0])
    plt.xlabel('False Positive Rate')
    plt.ylabel('True Positive Rate')
    plt.title('Receiver operating characteristic example')
    plt.legend(loc="lower right")
    plt.show()

draw_cure()