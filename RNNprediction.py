
import tensorflow as tf
import pandas as pd
import numpy as np
num_features = 26

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
  

model = ResidualWrapper(
    tf.keras.Sequential([
    tf.keras.layers.LSTM(12, return_sequences=False,activation = 'relu'),
    tf.keras.layers.Dense(
        num_features,
        # The predicted deltas should start small.num_
        # Therefore, initialize the output layer with zeros.
        kernel_initializer=tf.initializers.zeros())
]))







### CODICE PER CARICARE L'INPUT (NON LEGGERE...)


newdata = pd.read_csv("data_w_temp.csv")
count = 0

while -999 in newdata['Medio'].values and count < len(newdata):
    for i in range(1, len(newdata)):
        if newdata.loc[i, 'Medio'] == -999:
            newdata.loc[i, 'Medio'] = newdata.loc[i-1, 'Medio']
    count += 1


newdata["date"] = pd.to_datetime(newdata["date"]) # Converting into datetime format
## Adding a the 'year' feature

newdata["year"] = newdata["date"].dt.year
start_date = pd.to_datetime("2022-03-01") 
# newdata = newdata[newdata["date"] >= start_date] # Extracting the portion of data from uk-rus war start
date_time = pd.to_datetime(newdata.pop('date'), format='%Y.%m.%d %H:%M:%S')



## Add a weekend feature which is 1 if it's saturday or sunday, 0 otherwise

# Define a function to check if a day is a weekend
def is_weekend(day):
    if day in [5, 6]:  # Saturday and Sunday
        return 1
    else:
        return 0

# Apply the function to create the 'weekend' column

newdata['weekend'] = newdata['weekday'].apply(is_weekend)

timestamp_s = date_time.map(pd.Timestamp.timestamp)
day = 24*60*60
year = (365.2425)*day
newdata['Day sin'] = np.sin(timestamp_s * (2 * np.pi / day))
newdata['Day cos'] = np.cos(timestamp_s * (2 * np.pi / day))

newdata = newdata.drop('day', axis=1)


newdata = newdata.drop('Swiss_import', axis = 1)
newdata = newdata.drop('Swiss_export', axis = 1)
newdata = newdata.drop('France_import', axis = 1)
newdata = newdata.drop('France_export', axis = 1)
newdata = newdata.drop('Austria_import', axis = 1)
newdata = newdata.drop('Austria_export', axis = 1)
newdata = newdata.drop('Slovenia_import', axis = 1)
newdata = newdata.drop('Slovenia_export', axis = 1)

print(newdata.head())
# Creating a new variable "cluster" which is 0 in cluster 1 and 1 in cluster 2.

newdata['cluster'] = 0

newdata.loc[33120:,'cluster'] = 1

### HO CARICATO IL DATASET E VOGLIO PRENDERE LE ULTIME 24 RIGHE

last_24_rows = newdata.tail(24)

# Convert the last 24 rows into a NumPy array
last_24_array = last_24_rows.to_numpy()

# Convert the NumPy array into a TensorFlow tensor
tensor = tf.convert_to_tensor(last_24_array)

# Reshape the tensor 
reshaped_tensor = tf.reshape(tensor, (1, 24, 26))
model(reshaped_tensor)
model.load_weights('residual_lstm_weights.h5')
pred = model.predict(reshaped_tensor)
print(pred)
