"""This file preprocess the data in the same way as preprocess.py but it is has been
adapted to work with daily data.

This part is more sentitive to the structure of the data, that is why this version
is kept in a different folder. The rest of the program should handle the preprocessed
data well (with a few variable changes) weather it is recorded daily or every 15 minutes."""

import os

from d_checkGaps import d_checkGaps
from d_normalizer import d_normalizer
from d_joiner import d_joiner
from d_mfilterer import d_mfilterer


# Define the data we want to study
files = [f for f in os.listdir("PreprocessorDaily/Database") if os.path.isfile(os.path.join("PreprocessorDaily/Database", f))]

varNames = [i[0:-4] for i in files] # extract the names of the variables

# Define the time dram we want to use (a: months, b: weeks, c: days)
timeFrame = 'b'

if __name__ == '__main__':

    for varName in varNames:

        # Fill in the gaps in the time series
        d_checkGaps(File=f'{varName}.csv')
        print('[INFO] checkGaps() DONE')

        # Normalize the data. See normalizer.py for details
        d_normalizer(File=f'{varName}_full.csv')
        print('[INFO] normalizer() DONE')
    
    # Join the normalized databases
    d_joiner()
    print('[INFO] joiner() DONE')
    
    # Filter out those months or weeks or days (depending on the desired
    # time unit) with too many NaN in several variables and iterate on the rest
    d_mfilterer(File='data_joi.csv', timeframe=timeFrame)
    print('[INFO] filterer() DONE')
