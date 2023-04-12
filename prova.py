import os
import datetime
import numpy as np
import pandas as pd
import tensorflow as tf
import matplotlib as mpl
import matplotlib.pyplot as plt


mpl.rcParams['figure.figsize'] = (8, 6)
mpl.rcParams['axes.grid'] = False
df = pd.read_csv("AppliedStatisticsProject\ml_data.csv")

df["date"] = pd.to_datetime(df["date"]) # Converting into datetime format
end_date = pd.to_datetime("2020-01-01") 
df = df[df["date"] <= end_date] # Extracting the portion of data from 2018 to 2019 (inlcluded)
date_time = pd.to_datetime(df.pop('date'), format='%Y.%m.%d %H:%M:%S')

# print(df.head())

# Printing the price overtime
# plot_cols = ['dam']
# plot_features = df[plot_cols]
# plot_features.index = date_time
# plot_features.plot
# plt.plot(plot_features)
# plt.show()
# print(df.describe().transpose())

# Emphasizing the periodicity of the date
timestamp_s = date_time.map(pd.Timestamp.timestamp)
day = 24*60*60
year = (365.2425)*day
df['Day sin'] = np.sin(timestamp_s * (2 * np.pi / day))
df['Day cos'] = np.cos(timestamp_s * (2 * np.pi / day))
df['Year sin'] = np.sin(timestamp_s * (2 * np.pi / year))
df['Year cos'] = np.cos(timestamp_s * (2 * np.pi / year))

# Checking that the periodicity is daily and yearly

# fft = tf.signal.rfft(df['dam'])
# f_per_dataset = np.arange(0, len(fft))

# n_samples_h = len(df['dam'])
# hours_per_year = 24*365.2524
# years_per_dataset = n_samples_h/(hours_per_year)

# f_per_year = f_per_dataset/years_per_dataset
# plt.step(f_per_year, np.abs(fft))
# plt.xscale('log')
# plt.ylim(0, 400000)
# plt.xlim([0.1, max(plt.xlim())])
# plt.xticks([1, 365.2524], labels=['1/Year', '1/day'])
# _ = plt.xlabel('Frequency (log scale)')
# plt.show()

column_indices = {name: i for i, name in enumerate(df.columns)}

n = len(df)
train_df = df[0:int(n*0.7)]
val_df = df[int(n*0.7):int(n*0.9)]
test_df = df[int(n*0.9):]

num_features = df.shape[1]


train_mean = train_df.mean()
train_std = train_df.std()

train_df = (train_df - train_mean) / train_std
val_df = (val_df - train_mean) / train_std
test_df = (test_df - train_mean) / train_std

df_std = (df - train_mean) / train_std
df_std = df_std.melt(var_name='Column', value_name='Normalized')
plt.figure(figsize=(12, 6))
ax = sns.violinplot(x='Column', y='Normalized', data=df_std)
_ = ax.set_xticklabels(df.keys(), rotation=90)