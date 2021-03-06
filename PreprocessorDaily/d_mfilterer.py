
import os
import pandas as pd

"""This function deletes those time spans across several variables 
with too many empty values, and iterates on the rest"""

def d_mfilterer(File, timeframe):
        
    fileName, fileExtension = os.path.splitext(File)
    df = pd.read_csv(f'PreprocessorDaily/Database/{fileName}.csv', delimiter=';')

    years = list(dict.fromkeys(df['year'].tolist()))

    months = list(dict.fromkeys(df['month'].tolist()))
    months.sort()

    weeks = list(dict.fromkeys(df['week'].tolist()))
    weekOrder = list(dict.fromkeys(df['weekOrder'].tolist()))

    startDate = list(df['startDate'])
    endDate = list(df['endDate'])

    days = list(dict.fromkeys(df['day'].tolist()))

    cols = list(df.columns.values.tolist())[1:8]

    if timeframe == 'a':
        
        indexInit, indexEnd = [], []
        numNaN, consecNaN = [], []
        for i in years:

            df = df.loc[df['year'] == i]
            
            for j in months:
                
                df = df.loc[df['month'] == j]

                # Get total number of NaN and the max consecutive NaNs
                for k in cols:

                    numNaN.append(df[k].isnull().sum())
                    consecNaN.append(max(df[k].isnull().astype(int).groupby(df[k].notnull().astype(int).cumsum()).sum()))

                # Count the number of NaN higher than 480 and the number of consecNaN higher than 192
                count_numNaN = sum(map(lambda x: x >= 8, numNaN))
                count_consecNaN = sum(map(lambda x: x >= 5, consecNaN))
                
                # Get the first and last index of those months with too many empty (or consecutive) values 
                # in several variables (NaN in this case)
                if count_numNaN >= 2 or count_consecNaN >= 2:
                    indexInit.append(df.index[0])
                    indexEnd.append(df.index[-1])

                # Clean numNaN and consecNaN
                numNaN, consecNaN = [], []

                df = pd.read_csv(f'PreprocessorDaily/Database/{fileName}.csv', delimiter=';')
                
                if j == 12:
                    df = df.loc[df['year'] == (i+1)]
                else:
                    df = df.loc[df['year'] == i]

        # Delete those parts of the data frame between the appended indices
        df = pd.read_csv(f'PreprocessorDaily/Database/{fileName}.csv', delimiter=';')

        counter = 0
        lenMonth = 31
        for i,j in zip(indexInit, indexEnd):

            df = df.drop(df.index[int(i-counter*lenMonth):int(j-counter*lenMonth+1)], inplace=False)
            counter += 1
        
        # Interpolate the remaining empty values
        df = (df.interpolate(method='polynomial', order=1)).round(2)
        
        # Save the data frame 
        cols = list(df.columns.values.tolist())
        df.to_csv(f'PreprocessorDaily/Database/{fileName[0:-4]}_pro.csv', sep=';', encoding='utf-8', index=False, header=cols)

    elif timeframe == 'b':
        
        weeks = [i for i in weeks if i != 0] # Remove the 0

        indexInit, indexEnd = [], []
        numNaN, consecNaN = [], []
        for i in weeks:
            
            df = df.loc[df['week'] == i]
            
            # Get total number of NaN and the max consecutive NaNs
            for k in cols:
                
                numNaN.append(df[k].isnull().sum())
                consecNaN.append(max(df[k].isnull().astype(int).groupby(df[k].notnull().astype(int).cumsum()).sum()))
            
            # Count the number of NaN higher than 192 and the number of consecNaN higher than 24
            count_numNaN = sum(map(lambda x: x >= 3, numNaN))
            count_consecNaN = sum(map(lambda x: x >= 3, consecNaN))
            
            # Get the first and last index of those months with too many empty (or consecutive) values 
            # in several variables (NaN in this case)
            if count_numNaN >= 2 or count_consecNaN >= 2:
                indexInit.append(df.index[0])
                indexEnd.append(df.index[-1])
            
            # Clean numNaN and consecNaN
            numNaN, consecNaN = [], []
            
            df = pd.read_csv(f'PreprocessorDaily/Database/{fileName}.csv', delimiter=';')
            
        # Delete those parts of the data frame between the appended indices
        df = pd.read_csv(f'PreprocessorDaily/Database/{fileName}.csv', delimiter=';')
        
        counter = 0
        lenWeek = 7
        for i,j in zip(indexInit, indexEnd):
            
            df = df.drop(df.index[int(i-counter*lenWeek):int(j-counter*lenWeek+1)], inplace=False)
            counter += 1
        
        # Interpolate the remaining empty values
        df = (df.interpolate(method='polynomial', order=1)).round(2)

        # Save the data frame
        cols = list(df.columns.values.tolist())
        df.to_csv(f'PreprocessorDaily/Database/{fileName[0:-4]}_pro.csv', sep=';', encoding='utf-8', index=False, header=cols)
        
    # THIS LAST OPTION DOES NOT MAKE SENSE WITH DAILY DATA. lEFT UNTOUCHED        
    # elif timeframe == 'c':

    #     indexInit, indexEnd = [], []
    #     numNaN, consecNaN = [], []
    #     for i in years:

    #         df = df.loc[df['year'] == i]

    #         for j in months:

    #             df = df.loc[df['month'] == j]

    #             for k in days:

    #                 df = df.loc[df['day'] == k]
                    
    #                 # Get total number of NaN and the max consecutive NaNs
    #                 for l in cols:
                        
    #                     numNaN.append(df[l].isnull().sum())
    #                     consecNaN.append(max(df[l].isnull().astype(int).groupby(df[l].notnull().astype(int).cumsum()).sum()))

    #                 # Count the number of NaN higher than 20 and the number of consecNaN higher than 8
    #                 count_numNaN = sum(map(lambda x: x >= 20, numNaN))
    #                 count_consecNaN = sum(map(lambda x: x >= 8, consecNaN))
                    
    #                 print(k, j, i)
    #                 print(numNaN, consecNaN)
    #                 print(count_numNaN, count_consecNaN)
                    
    #                 # Get the first and last index of those months with too many empty (or consecutive) values 
    #                 # in several variables (NaN in this case)
    #                 if count_numNaN >= 3 or count_consecNaN >= 3:
    #                     indexInit.append(df.index[0])
    #                     indexEnd.append(df.index[-1])
                    
    #                 # Clean numNaN and consecNaN
    #                 numNaN, consecNaN = [], []

    #                 df = pd.read_csv(f'Database/{fileName}.csv', delimiter=';')

    #                 if j == 12 and k == 31:
    #                     df = df.loc[df['year'] == (i+1)]
    #                     df = df.loc[df['month'] == 1]
                    
    #                 elif j <= 12:
    #                     df = df.loc[df['year'] == i]
                        
    #                     if k < 31:
    #                         df = df.loc[df['month'] == j]
                        
    #                     elif k == 31:
    #                         df = df.loc[df['month'] == (j+1)]

    #     # Delete those parts of the data frame between the appended indices
    #     df = pd.read_csv(f'Database/{fileName}.csv', delimiter=';')

    #     counter = 0
    #     lenDay = 96
    #     for i,j in zip(indexInit, indexEnd):

    #         df = df.drop(df.index[int(i-counter*lenDay):int(j-counter*lenDay+1)], inplace=False)
    #         counter += 1

    #     # Interpolate the remaining empty values
    #     df = (df.interpolate(method='polynomial', order=1)).round(2)

    #     # Save the data frame
    #     cols = list(df.columns.values.tolist())
    #     df.to_csv(f'Database/{fileName[0:-4]}_pro.csv', sep=';', encoding='utf-8', index=False, header=cols)
