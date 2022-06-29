# Explain what this file does here

# Load libraries
library(mlmts)
library(tidyverse)

# This function is a sorter
bully <- function(list, order) {

    sorted_list <- c()
    counter <- 1

    for (i in order) {

        value <- list[i]

        sorted_list[[counter]] <- value

        counter <- counter + 1
    }

    return(unlist(sorted_list))
}

# Shape outlier detection
outliers_shape <- outlier_detection(mts$data)

# # Apply the outlying order to the time stamps
ordered_dates <- bully(list = mts$time, order = outliers_shape$Indexes)

# print(outliers_shape$Indexes)
print(outliers_shape$Depths)
print(ordered_dates)