import pandas as pd
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error
import datetime as dt
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
import numpy as np

# Carica il dataset
dataset = pd.read_csv('ml_data.csv')
dataset["date"] = pd.to_datetime(dataset["date"]) # Converting into datetime format
start_date = pd.to_datetime("2019-01-01")
end_date = pd.to_datetime("2020-01-01") 
df = dataset[ dataset["date"] <= end_date] # Extracting the portion of data from 2018 to 2019 (included)
df = df[ df["date"] >= start_date]
date_time = pd.to_datetime(df.pop('date'), format='%Y.%m.%d %H:%M:%S')


# Prepara le variabili di input
X = df.drop(['dam'], axis=1).values

# Prepara l'etichetta di output (prezzi dell'elettricità per ogni ora)
y = df[['dam']].values

# Suddivide il dataset in training set e test set
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)

X_train = StandardScaler().fit_transform(X_train)
X_test=StandardScaler().fit_transform(X_test)

# Inizializza il modello GBRT
#gbrt = GradientBoostingRegressor(n_estimators=100, learning_rate=0.1, random_state=42)
gbrt = GradientBoostingRegressor(loss='squared_error',n_estimators=1000,learning_rate=0.05,random_state=0,max_depth=5,min_samples_split=6,min_samples_leaf=1)


# Addestra il modello sul training set
gbrt.fit(X_train, y_train)

# Effettua le previsioni sul test set
y_pred = gbrt.predict(X_test)

# Valuta le prestazioni del modello (ad esempio, l'errore quadratico medio per ogni ora)
mse = mean_squared_error(y_test, y_pred, multioutput='raw_values')
print("Mean Squared Error per ogni ora:")
print(mse)

fig,one_day=plt.subplots(figsize=(20,20))
month=np.random.randint(1,13,1)[0]
day=np.random.randint(1,31,1)[0]
year=np.random.randint(2019,2020,1)[0]
random_day=dt.datetime(year,month,day)
random_data=df[df.index.strftime('%y-%m-%d')==random_day.strftime('%y-%m-%d')]
random_data_standardized=StandardScaler().fit_transform(random_data.iloc[:,1:])
y_predicted_random=gbrt.predict(random_data_standardized)
one_day.tick_params(axis='both',labelsize=30)
one_day.plot(random_data.index,random_data['dam'],label='DAM values',linewidth=10)
one_day.plot(random_data.index,y_predicted_random,label='Forecasted DAM value',linewidth=10)
one_day.fill_between(random_data.index, y_predicted_random - np.sqrt(mse), y_predicted_random + np.sqrt(mse), alpha=0.2,label='MSE confidence interval')
one_day.legend(prop={'size':30})
plt.setp(one_day,xticks=random_data.index, xticklabels=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24])
plt.show()
