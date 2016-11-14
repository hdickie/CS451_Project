# -*- coding: utf-8 -*-
"""
Created on Fri Oct 28 03:54:52 2016

@author: John
"""
import pandas as pd
import arff 

import numpy as np
import matplotlib.pyplot as plt
import sklearn as sk
from sklearn import preprocessing
import scipy as sp
from scipy.io.arff import loadarff
from sklearn.ensemble import RandomForestClassifier
from sklearn import svm, linear_model
from sklearn.cross_validation import train_test_split
from sklearn.metrics import roc_curve, auc, confusion_matrix, accuracy_score, classification_report
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import classification_report



file= "C:/Users/John/Documents/5 Semester/Data Mining/Project/bio_trainT.csv"
'''
dataset, meta=loadarff(open(file,'r'))
datasetC=np.asarray(dataset.tolist(), dtype=np.float32)
'''
name=range(0,77)
data = pd.read_csv(file, header=0, names=name)
#print(len(dataset))

#print(data.head(n=10))

train, test = train_test_split(data, test_size = 0.3)
target = train[2]
testtarget=test[2]
training=train.drop(2, axis=1)
testing= test.drop(2, axis=1)


''' Random forest --------------------------------------------------------------------------------'''
'''
print("-------------------------------Random Forest-------------------------------------------")
clf=RandomForestClassifier(n_estimators=76)
RNNfit=clf.fit(X=training, y=target,sample_weight=None)
print("Feature importence Array: \n",RNNfit.feature_importances_)
RNNpred= clf.predict(testing)
print()
'''
'''Trainging Accuracy Random Forest----'''
'''
RNNpred= clf.predict(training)
print()

accuracy = accuracy_score(target, RNNpred)
print("accuracy: ", accuracy)
cm = confusion_matrix(target, RNNpred)
print ("confusion matrix: \n", cm)



fpr, tpr, thresholds = roc_curve(target, RNNpred)
roc_auc = auc(fpr, tpr)
print(roc_auc)
'''

''' Test Accuracy Random Forest--------'''  
'''
RNNpred= clf.predict(testing)
print("Performance on test set \n")

accuracy = accuracy_score(testtarget, RNNpred)
print("accuracy: ", accuracy)
cm = confusion_matrix(testtarget, RNNpred)
print ("confusion matrix: \n", cm)

fpr, tpr, thresholds = roc_curve(testtarget, RNNpred)
roc_auc = auc(fpr, tpr)
print(roc_auc)

plt.figure()
plt.plot(fpr, tpr, label='ROC curve (area = %0.2f)' % roc_auc, lw=4 )
plt.show()
'''

'''--------Random Forest with Upsample---------------------------'''

to_add=data.loc[data[2] == 1]
newtrain=train
for i in range(0, 40):
    newtrain.append(to_add)

newtarget = newtrain[2]
newtraining=newtrain.drop(2, axis=1)

clf=RandomForestClassifier(n_estimators=76)
RNNfit=clf.fit(X=newtraining, y=newtarget,sample_weight=None)
print("Feature importence Array: \n",RNNfit.feature_importances_)
RNNpred= clf.predict(testing)
print()


'''Testing--------------------------------'''

RNNpred= clf.predict(testing)

#for imp in range(0, len(importances)):
 #   print(importances)

print("Performance on test set \n")




accuracy = accuracy_score(testtarget, RNNpred)
print("accuracy: ", accuracy)
cm = confusion_matrix(testtarget, RNNpred)
print ("confusion matrix: \n", cm)

fpr, tpr, thresholds = roc_curve(testtarget, RNNpred)
roc_auc = auc(fpr, tpr)
print(roc_auc)

plt.figure()
plt.plot(fpr, tpr, label='ROC curve (area = %0.2f)' % roc_auc, lw=4 )
plt.show()


'''SVM------------------------------------------------------------------------------------------------------------'''
'''
print("----------------------------SVM--------------------------------------------")
print("---------------------RBF-------------")

C_range = [1e-2, 1]                 
gamma_range = [1e-1]               
param_grid = dict(gamma=gamma_range, C=C_range)      # possible parameters to be searched through
cv = 5                                               #cross validation folds
svc_rbf = GridSearchCV(svm.SVC(), param_grid=param_grid, cv=cv)
svc_rbf.fit(training, target)

print("The best parameters are %s with a score of %0.2f"
      % (svc_rbf.best_params_, svc_rbf.best_score_))

#rbf_pred=svc_rbf.predict(training)
rbf_predTest=svc_rbf.predict(testing)
#y_pred = svc_rbf.decision_function(testing)
'''
'''Trainging Accuracy RBF----'''
'''
accuracy = accuracy_score(target, rbf_pred)
print("accuracy: ", accuracy)
cm = confusion_matrix(target, rbf_pred)
print ("confusion matrix: \n", cm)
fpr, tpr, thresholds = roc_curve(target, rbf_pred)
roc_auc = auc(fpr, tpr)
print(roc_auc)
'''
''' Test Accuracy RBF--------'''
'''
print("Performance on test set \n")
accuracy = accuracy_score(testtarget, rbf_predTest)
print("accuracy: ", accuracy)
cm = confusion_matrix(testtarget, rbf_predTest)
print ("confusion matrix: \n", cm)
fpr, tpr, thresholds = roc_curve(testtarget, rbf_predTest)
roc_auc = auc(fpr, tpr)
print("Area under ROC: ", roc_auc)

plt.figure()
plt.plot(fpr, tpr, label='ROC curve (area = %0.2f)' % roc_auc, lw=4 )
plt.show()
'''

