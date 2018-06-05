import numpy as np

def multi_generator(market, grib, vwp, lookback_market, lookback_grib, min_index = 0, 
                    max_index = None, shuffle = False, batch_size = 128):
    # Generator breaks if input datasets' dimensions do not match
    assert market.shape[0] == grib.shape[0] == vwp.shape[0]
    # Find which lookback is longest, since that determines the timeline lower bound
    max_lb = max(lookback_market, lookback_grib)
    if max_index is None:
        max_index = len(vwp)
    i = min_index + max_lb
    while True:
        if shuffle:
            # The randomly drawn rows that make up each observation in the batch is the same for both sets
            rows = np.random.randint(min_index + max_lb, max_index, size=batch_size)
        else:
            if i + batch_size >= max_index:
                i = min_index + max_lb
            rows = np.arange(i, min(i + batch_size, max_index))
            i += len(rows)

        # Samples size = (batch_size, lookback_lags+1, num_features)
        ## +1 since we include the last set of inputs, i.e. those for which we want to predict
        inputs_market = np.zeros((len(rows), lookback_market+1, market.shape[-1]))
        inputs_grib = np.zeros((len(rows), lookback_grib+1, grib.shape[1], grib.shape[2], grib.shape[3]))
        outputs_vwp = np.zeros((len(rows),))
        
        # Iterate over row in rows (= j in batch)
        for j, row in enumerate(rows):
            # Row indices of lookback lags + "current" row j (+1 since range is not inclusive right)
            ## Then the corresponding data
            indices_market = range(rows[j] - lookback_market, rows[j]+1)
            indices_grib = range(rows[j] - lookback_grib, rows[j]+1)
            inputs_market[j] = market[indices_market]
            inputs_grib[j] = grib[indices_grib]
            outputs_vwp[j] = vwp[rows[j]]
        yield [inputs_market, inputs_grib], outputs_vwp