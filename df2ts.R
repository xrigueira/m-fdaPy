# Convert the df to ts
# format <- "%d/%m/%Y %H:%M" # Set the format
# df_csv$date <- as.POSIXct(strptime(df_csv$date, format), tz = "") # Change the data type of the time column
# df_ts <- xts(df_csv[, -1], order.by = as.POSIXct(df_csv$date)) # Convert to xts PROBLEM -> converts to char too...

# Subsetting in the data.frame (version easier to read)
# subset_test <- select(filter(df_csv, week == 2), c(so2, no))
# subset_test <- unname(data.matrix(subset_test))