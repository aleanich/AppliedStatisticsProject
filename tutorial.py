# -*- coding: utf-8 -*-
"""
Created on Wed Mar 15 14:07:18 2023

@author: matte
"""
# fristly import the libraries
import pandas as pd
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
import pandas as pd
import datetime as dt
import os
import numpy as np
import matplotlib as mpl
from sklearn.decomposition import PCA
from sklearn.model_selection import GridSearchCV, KFold
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import r2_score
from sklearn.metrics import mean_squared_error

#import the data
data=pd.read_pickle('ml_data.pkl')
data=data.astype(float)
#select the year
anno=19
#fetch the data for a given year
data=data[data.index.strftime('%y')==str(anno)]
#compute the correlation between data  
correlation=data.corr()
#identify the features(note the first column (column 0 ) represrents the prices)
features=data.columns[1:]
#divide the dataset in train and split
training_data,testing_data=train_test_split(data,test_size=0.2,random_state=0)
#create the features training matrix
X_multi=np.matrix(training_data[features])
#create the feature testing matrix
X_test_multi=np.matrix(testing_data[features])
#standardize the data with the standard scaler->it removes the mean and scaling to unit variance
X_multi_standardized = StandardScaler().fit_transform(X_multi)
X_test_multi_standardized=StandardScaler().fit_transform(X_test_multi)
#define the DAM prices as a series
Y_training=training_data.loc[:,'dam']
Y_testing=testing_data.loc[:,'dam']
# import the ML model
r_f_r=GradientBoostingRegressor(loss='squared_error',n_estimators=1000,learning_rate=0.1,random_state=0,max_depth=5,min_samples_split=6,min_samples_leaf=1)
#fit the model
r_f_r.fit(X_multi_standardized,Y_training)
#predict the results of the test dataset with the trained model
y_predicted=r_f_r.predict(X_test_multi_standardized)
#define a score
score=r2_score(y_predicted,Y_testing)
mse=mean_squared_error(y_predicted,Y_testing)

print(" The accuracy for the gradient boosting regressor is "+str(score*100)+' %\n'\
      " The MSE is "+str(mse)+"\n The RMSE is "+str(np.sqrt(mse)))
# create three plots
fig,dam_prediction=plt.subplots()
dam_prediction.scatter(Y_testing,y_predicted,label="actual dam")
dam_prediction.plot(np.linspace(0,100,100),np.linspace(0,100,100),color='red')
#dam_prediction.scatter(testing_data.index,y_predicted,label='Random Forest')
dam_prediction.set_title('Gradient boositng Features: '+str(features))
dam_prediction.set_xlabel('Real value')
dam_prediction.set_ylabel('Forecasted value')


fig,dam_prediction_10=plt.subplots()
dam_prediction_10.scatter(testing_data.index[10:50],Y_testing[10:50],label="actual dam")
dam_prediction_10.scatter(testing_data.index[10:50],y_predicted[10:50],label='Grandient boositg')
dam_prediction_10.set_title('Random forest; Features: '+str(features))


fig,one_day=plt.subplots(figsize=(20,20))
month=np.random.randint(1,13,1)[0]
day=np.random.randint(1,31,1)[0]
year=np.random.randint(2000+anno,2000+anno+1,1)[0]
random_day=dt.datetime(year,month,day)
random_data=data[data.index.strftime('%y-%m-%d')==random_day.strftime('%y-%m-%d')]
random_data_standardized=StandardScaler().fit_transform(random_data.iloc[:,1:])
y_predicted_random=r_f_r.predict(random_data_standardized)
one_day.tick_params(axis='both',labelsize=30)
one_day.plot(random_data.index,random_data['dam'],label='DAM values',linewidth=10)
one_day.plot(random_data.index,y_predicted_random,label='Forecasted DAM value',linewidth=10)
one_day.fill_between(random_data.index, y_predicted_random - np.sqrt(mse), y_predicted_random + np.sqrt(mse), alpha=0.2,label='MSE confidence interval')
one_day.legend(prop={'size':30})
plt.setp(one_day,xticks=random_data.index, xticklabels=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24])

