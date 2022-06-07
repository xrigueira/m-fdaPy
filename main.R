# TODO: this file will contain the implementation of both algorithms
# from Lopez-Oriona 2021

# Include the needed libraries
library(xts)
library(dplyr)
library(mlmts)
library(lubridate)

# Read the csv file
df_csv <- read.csv("Database/argentina_test.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

# Convert the df to ts
# format <- "%d/%m/%Y %H:%M" # Set the format
# df_csv$date <- as.POSIXct(strptime(df_csv$date, format), tz = "") # Change the data type of the time column
# df_ts <- xts(df_csv[, -1], order.by = as.POSIXct(df_csv$date)) # Convert to xts PROBLEM -> converts to char too...


# # Subsetting option 1
# subset_1 <- subset(df_csv, df_csv$week == 2, select = c(so2, no))
# # print(subset_1)
# # print(str(matrix(subset_1)))

# Option 2
subset_2 <- select(filter(df_csv, week == 2), c(so2, no))
print(subset_2)
names(subset_2) <- NULL
print(str(data.matrix(subset_2)))
print(class(data.matrix(subset_2)))

# Testing a matrix
mat <- matrix(rnorm(36), nrow = 6)
print(str(mat))
print(class(mat))

# print(str(SyntheticData1$data[[1]]))
# print(class(SyntheticData1$data[[1]]))