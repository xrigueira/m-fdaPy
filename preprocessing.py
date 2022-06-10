
from checkGaps import checkGaps
from normalizer import normalizer
from filterer import filterer


# Define the data we want to study
varName = 'Temperatura'
timeFrame = 'b'


if __name__ == '__main__':
        
    # Fill in the gaps in the time series
    checkGaps(File=f'{varName}.txt')
    print('[INFO] checkGaps() DONE')
    
    # Normalize the data. See normalizer.py for details
    normalizer(File=f'{varName}_full.csv')
    print('[INFO] normalizer() DONE')
    
    # Filter out those months or weeks or days (depending on the desired
    # time unit) with too many NaN and iterate on the rest
    filterer(File=f'{varName}_nor.csv', timeframe=timeFrame)
    print('[INFO] filterer() DONE')