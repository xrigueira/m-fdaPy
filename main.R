# TODO: this file will contain the implementation of both algorithms
# from Lopez-Oriona 2021

# Include the needed libraries
library(xts)
library(dplyr)
library(mlmts)
library(lubridate)

# Read the csv file
df_csv <- read.csv("Database/argentina_test.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

# Subsetting in the data.frame
subset_test <- unname(data.matrix(select(filter(df_csv, week == 2), c(so2, no))))

# Define matrices
mat1 <- matrix(rnorm(16), nrow = 4)
mat2 <- matrix(rnorm(16), nrow = 4)

empty_list <- list()

empty_list[[1]] <- mat1
empty_list[[2]] <- mat2
print(str(empty_list))

# Create loop, see ideas -> https://www.statology.org/create-empty-list-in-r/
# This introduces rbind() -> https://www.projectpro.io/recipes/append-output-from-for-loop-dataframe-r

# print(str(SyntheticData1))
# print(SyntheticData1$data[[1]])
# print(class(SyntheticData1$data[[1]]))
# print(str(SyntheticData1$data[[1]]))