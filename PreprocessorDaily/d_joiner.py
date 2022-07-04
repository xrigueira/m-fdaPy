import os
import numpy as np
import pandas as pd

"""This function joins several csv files which contain time series of the
same lenght into a unique csv file."""

# NEEDSWORK: this function is too dependent on the file names. This can be improved I think

def d_joiner():
    
    # Set the names of the files which contain the data
    caudal = "Caudal_nor.csv"
    pluviometria = "Pluviometria_nor.csv"

    # Read the files and store them as a pandas database
    df = pd.read_csv(f'PreprocessorDaily/Database/{caudal}', delimiter=';', parse_dates=['date'], index_col=['date'])
    dfPluviometria = pd.read_csv(f'PreprocessorDaily/Database/{pluviometria}', delimiter=';', parse_dates=['date'], index_col=['date'])



    # Extract the desired columns
    pluviometria = dfPluviometria['value']

    # Insert the new columns in the database
    df.insert(1, "Pluviometria", pluviometria, True)

    cols = list(df.columns.values.tolist())
    cols = [i.replace("value", "Caudal") for i in cols]
    df.to_csv(f'PreprocessorDaily/Database/data_joi.csv', sep=';', encoding='utf-8', index=True, header=cols)
