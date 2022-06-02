# TODO: this file will contain the implementation of both algorithms
# from Lopez-Oriona 2021

library(xts)
library(dplyr)

# Read the csv file
df_csv <- read.csv("Database/argentina_pro2.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
print(head(df_csv))