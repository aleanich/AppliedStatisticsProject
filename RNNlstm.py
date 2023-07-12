import os
import datetime
import numpy as np
import pandas as pd
import tensorflow as tf
import matplotlib as mpl
import matplotlib.pyplot as plt
import seaborn as sns
import IPython
import IPython.display

mpl.rcParams['figure.figsize'] = (8, 6)
mpl.rcParams['axes.grid'] = False
df = pd.read_csv("data_w_temp.csv")



### Eliminating -999 from temperatures sobstituing the previous hour value
count = 0

while -999 in df['Medio'].values and count < len(df):
    for i in range(1, len(df)):
        if df.loc[i, 'Medio'] == -999:
            df.loc[i, 'Medio'] = df.loc[i-1, 'Medio']
    count += 1


df["date"] = pd.to_datetime(df["date"]) # Converting into datetime format
## Adding a the 'year' feature

df["year"] = df["date"].dt.year
start_date = pd.to_datetime("2022-03-01") 
# df = df[df["date"] >= start_date] # Extracting the portion of data from uk-rus war start
date_time = pd.to_datetime(df.pop('date'), format='%Y.%m.%d %H:%M:%S')

datawd = {'weekday': [1, 2, 3, 4, 5, 6, 7]}

## Add a weekend feature which is 1 if it's saturday or sunday, 0 otherwise

# Define a function to check if a day is a weekend
def is_weekend(day):
    if day in [5, 6]:  # Saturday and Sunday
        return 1
    else:
        return 0

# Apply the function to create the 'weekend' column

df['weekend'] = df['weekday'].apply(is_weekend)

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

df = df.drop('day', axis=1)


df = df.drop('Swiss_import', axis = 1)
df = df.drop('Swiss_export', axis = 1)
df = df.drop('France_import', axis = 1)
df = df.drop('France_export', axis = 1)
df = df.drop('Austria_import', axis = 1)
df = df.drop('Austria_export', axis = 1)
df = df.drop('Slovenia_import', axis = 1)
df = df.drop('Slovenia_export', axis = 1)

print(df.head())
# Creating a new variable "cluster" which is 0 in cluster 1 and 1 in cluster 2.

df['cluster'] = 0

df.loc[33120:,'cluster'] = 1


## Checking that the periodicity is daily and yearly

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

## Creating a dictionary for the column indexes e.g column_indices['column_name'] returns the index of the column "column_name"
column_indices = {name: i for i, name in enumerate(df.columns)}

### Diding in train-validation-test (85/10/5)

n = len(df)
train_df = df[0:int(n*0.95)]
val_df = df[int(n*0.95):int(n*0.975)]
test_df = df[int(n*0.975):]

num_features = df.shape[1]
print(num_features)

# Standardizing the features using only the training set

train_mean = train_df.mean()
train_std = train_df.std()

# Saving the mean and std for future predicitons

train_mean.to_csv('mean.csv')
train_std.to_csv('std.csv')

# Standardizing the features

# train_df = (train_df - train_mean) / train_std
# val_df = (val_df - train_mean) / train_std
# test_df = (test_df - train_mean) / train_std


## Plotting the standardized variables, in this case we can observe that some features such as gas price, thermal and the imports have
## heavy tails  (some outliers are observed)

# df_std = (df - train_mean) / train_std
# df_std = df_std.melt(var_name='Column', value_name='Normalized')
# plt.figure(figsize=(12, 6))
# ax = sns.violinplot(x='Column', y='Normalized', data=df_std)
# _ = ax.set_xticklabels(df.keys(), rotation=90)
# plt.show()

## Defining how to create windows of datas

class WindowGenerator():
  def __init__(self, input_width, label_width, shift,
               train_df=train_df, val_df=val_df, test_df=test_df,
               label_columns=None):
    # Store the raw data.
    self.train_df = train_df
    self.val_df = val_df
    self.test_df = test_df

    # Work out the label column indices.
    self.label_columns = label_columns
    if label_columns is not None:
      self.label_columns_indices = {name: i for i, name in
                                    enumerate(label_columns)}
    self.column_indices = {name: i for i, name in
                           enumerate(train_df.columns)}

    # Work out the window parameters.
    self.input_width = input_width
    self.label_width = label_width
    self.shift = shift

    self.total_window_size = input_width + shift
    self.input_slice = slice(0, input_width)
    self.input_indices = np.arange(self.total_window_size)[self.input_slice]

    self.label_start = self.total_window_size - self.label_width
    self.labels_slice = slice(self.label_start, None)
    self.label_indices = np.arange(self.total_window_size)[self.labels_slice]

  def __repr__(self):
    return '\n'.join([
        f'Total window size: {self.total_window_size}',
        f'Input indices: {self.input_indices}',
        f'Label indices: {self.label_indices}',
        f'Label column name(s): {self.label_columns}'])
  
  
## split_window allows to stack different windows

def split_window(self, features):

  # All shapes are: (batch, time, features)
  inputs = features[:, self.input_slice, :]
  labels = features[:, self.labels_slice, :]
  if self.label_columns is not None:
    labels = tf.stack(
        [labels[:, :, self.column_indices[name]] for name in self.label_columns],
        axis=-1)

  # Slicing doesn't preserve static shape information, so set the shapes
  # manually. This way the `tf.data.Datasets` are easier to inspect.

  # None means any size
  inputs.set_shape([None, self.input_width, None])
  labels.set_shape([None, self.label_width, None])

  return inputs, labels

# Assigning the split_window function to the split_window attribute of the WindowGenerator class
# Allowing instances of WindowGenerator to call this method
WindowGenerator.split_window = split_window

## Defining a plot method to visualize the windows

def plot(self, model=None, plot_col='dam', max_subplots=3):
  inputs, labels = self.example
  plt.figure(figsize=(12, 8))
  plot_col_index = self.column_indices[plot_col]
  max_n = min(max_subplots, len(inputs))
  for n in range(max_n):
    plt.subplot(max_n, 1, n+1)
    plt.ylabel(f'{plot_col} [normed]')
    plt.plot(self.input_indices, inputs[n, :, plot_col_index],
             label='Inputs', marker='.', zorder=-10)

    if self.label_columns:
      label_col_index = self.label_columns_indices.get(plot_col, None)
    else:
      label_col_index = plot_col_index

    if label_col_index is None:
      continue

    plt.scatter(self.label_indices, labels[n, :, label_col_index],
                edgecolors='k', label='Labels', c='#2ca02c', s=64)
    if model is not None:
      predictions = model(inputs)
      plt.scatter(self.label_indices, predictions[n, :, label_col_index],
                  marker='X', edgecolors='k', label='Predictions',
                  c='#ff7f0e', s=64)

    if n == 0:
      plt.legend()

  plt.xlabel('Time [h]')

WindowGenerator.plot = plot



## This make_dataset method will take a time series DataFrame and convert it to a tf.data.Dataset
##  of (input_window, label_window) pairs using the tf.keras.utils.timeseries_dataset_from_array function


def make_dataset(self, data):
  data = np.array(data, dtype=np.float32)
  ds = tf.keras.utils.timeseries_dataset_from_array(
      data=data,
      targets=None,
      sequence_length=self.total_window_size,
      sequence_stride=1,
      shuffle=True,
      batch_size=1,)

  ds = ds.map(self.split_window)

  return ds

WindowGenerator.make_dataset = make_dataset

@property
def train(self):
  return self.make_dataset(self.train_df)

@property
def val(self):
  return self.make_dataset(self.val_df)

@property
def test(self):
  return self.make_dataset(self.test_df)

@property
def example(self):
  result = getattr(self, '_example', None)
  if result is None:
    # No example batch was found, so get one from the `.test` dataset
    result = next(iter(self.test))
    # And cache it for next time
    self._example = result
  return result

WindowGenerator.train = train
WindowGenerator.val = val
WindowGenerator.test = test
WindowGenerator.example = example

################# SINGLE STEP METHODS #################

# Creating variables containing the performances

val_performance = {}
performance = {}

# Defining the window parameters

wide_window = WindowGenerator(
    input_width=24, label_width=24, shift=24,
    label_columns=['dam'])

single_step_window = WindowGenerator(
    input_width=24, label_width=1, shift=24,
    label_columns=['dam'])

######## LINEAR MODEL #########

linear = tf.keras.Sequential([
    tf.keras.layers.Dense(units=1)])

MAX_EPOCHS = 10

def compile_and_fit(model, window, patience=2):
  early_stopping = tf.keras.callbacks.EarlyStopping(monitor='val_loss',
                                                    patience=patience,
                                                    mode='min')

  model.compile(loss=tf.losses.MeanSquaredError(),
                optimizer=tf.keras.optimizers.Adam(),
                metrics=[tf.keras.metrics.MeanAbsoluteError()])

  history = model.fit(window.train, epochs=MAX_EPOCHS,
                      validation_data=window.val,
                      callbacks=[early_stopping])
  return history

# history = compile_and_fit(linear, single_step_window)

# val_performance['Linear'] = linear.evaluate(single_step_window.val)
# performance['Linear'] = linear.evaluate(single_step_window.test, verbose=0)

# wide_window.plot(linear)
# plt.show()

# lstm_model = tf.keras.models.Sequential([
#     # Shape [batch, time, features] => [batch, time, lstm_units]
#     tf.keras.layers.LSTM(32, return_sequences=True),
#     # Shape => [batch, time, features]
#     tf.keras.layers.Dense(units=1)
# ])

# history = compile_and_fit(lstm_model, wide_window)

# IPython.display.clear_output()
# val_performance['LSTM'] = lstm_model.evaluate(wide_window.val)
# performance['LSTM'] = lstm_model.evaluate(wide_window.test, verbose=1)
# # lstm_model.save_weights('lstm_weights.h5')
# wide_window.plot(lstm_model)
# plt.show()

######### MULTI-OUTPUT MODELS #########




class ResidualWrapper(tf.keras.Model):
  def __init__(self, model):
    super().__init__()
    self.model = model

  def call(self, inputs, *args, **kwargs):
    delta = self.model(inputs, *args, **kwargs)

    # The prediction for each time step is the input
    # from the previous time step plus the delta
    # calculated by the model.
    return inputs + delta
  

residual_lstm = ResidualWrapper(
    tf.keras.Sequential([
    tf.keras.layers.LSTM(12, return_sequences=True,activation = 'relu'),
    tf.keras.layers.Dense(
        num_features,
        # The predicted deltas should start small.
        # Therefore, initialize the output layer with zeros.
        kernel_initializer=tf.initializers.zeros())
]))

print('Input shape:', wide_window.example[0].shape)


history = compile_and_fit(residual_lstm, wide_window)

IPython.display.clear_output()
val_performance['Residual LSTM'] = residual_lstm.evaluate(wide_window.val)
performance['Residual LSTM'] = residual_lstm.evaluate(wide_window.test, verbose=0)
wide_window.plot(residual_lstm)
plt.show()
residual_lstm.save_weights('residual_lstm_weights.h5')



