# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import os
import pandas as pd

os.chdir("//ahmct-065/teams/PMRF/Amir/bin")
df=pd.read_csv("training1.csv")
test=pd.read_csv("testing1.csv")

x=pd.DataFrame(data=df)
x=x.drop(columns=['CASE_ID'])

y=pd.DataFrame(data=df, columns=['CASE_ID']) 

test_col=[col for col in test.columns if col in df.columns]
x_test=pd.DataFrame(data=test, columns=test_col)
x_test=x_test.drop(columns=['CASE_ID'])

y_test=pd.DataFrame(data=test, columns=['CASE_ID']) 


from sklearn.feature_selection import RFE
from sklearn.linear_model import LogisticRegression

logreg = LogisticRegression(solver='lbfgs', max_iter=1000, n_jobs=7)
rfe = RFE(logreg, 10)
rfe = rfe.fit(x, y.values.ravel())
print(rfe.support_)
print(rfe.ranking_)
rfe.get_support(indices=True)

high_score=0
score_list =[]
nof=0
for n in range(2, 38):
    model = LogisticRegression(solver='lbfgs', max_iter=1000, n_jobs=7)
    rfe = RFE(model, n)
    train = rfe.fit_transform(x, y.values.ravel())
    x_test_rfe = rfe.transform(x_test)
    model.fit(train,y)
    score = model.score(x_test_rfe,y_test)
    score_list.append(score)
    if(score>high_score):
        high_score = score
        nof = n

import matplotlib.pyplot as plt
plt.plot(score_list)
