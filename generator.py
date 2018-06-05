import numpy as np

# `data`: The original array of floating point data, which we just normalized in the code snippet above.
# `lookback`: How many timesteps back should our input data go.
# `delay`: How many timesteps in the future should our target be.
# `min_index` and `max_index`: Indices in the `data` array that delimit which timesteps to draw from. This is useful for keeping a segment of the data for validation and another one for testing.
# `shuffle`: Whether to shuffle our samples or draw them in chronological order.
# `batch_size`: The number of samples per batch.
# `step`: The period, in timesteps, at which we sample data. We will set it 6 in order to draw one data point every hour.

def generator(data, lookback, min_index = 0, max_index = None, delay = 0,
              shuffle = False, batch_size = 128, step = 1):
    if max_index is None:
        max_index = len(data) - delay
    # + 1 since include row of inputs for which to predict
    i = min_index + lookback + 1
    # while true
    while 1:
        if shuffle:
            # Pull batch_size random row indices between given index range
            rows = np.random.randint(
                min_index + lookback + 1, max_index, size=batch_size)
            # No i-counter since shuffling, so can repeat same rows
        else:
            if i + batch_size >= max_index:
                i = min_index + lookback + 1
            # Define the row indices (observations) in batch
            rows = np.arange(i, min(i + batch_size, max_index))
            # Increment timeseries index counter
            i += len(rows)

        # Samples size = (batch_size, lookback_lags+1, num_features)
        ## +1 since we include the last set of inputs, i.e. those for which we want to predict
        samples = np.zeros((len(rows),
                           lookback+1 // step,
                           data.shape[-1]-1))
        # Outputs size = (batch_size)
        targets = np.zeros((len(rows),))
        # Iterate over row in rows (= j in batch)
        for j, row in enumerate(rows):
            # Indices = index of row j - lookback, to that row, by step
                # Gets row indices of lookback lags + "current" row j
            indices = range(rows[j] - lookback, rows[j]+1, step)
            # Get those ordered data observations
            # Each sample in batch has shape: (lookbacks, features)
            samples[j] = data[indices, 1:]
            # Corresponding targets are output [1] of row j + delay (24h)
                # In this case, outputs are in column [1]
            targets[j] = data[rows[j] + delay][0]
        # Sample shape: (batch_size, lookback_lags, num_features)
        # Targets shape: (batch_size,)
        yield samples, targets