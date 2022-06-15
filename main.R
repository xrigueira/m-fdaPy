# TODO: this file will contain the implementation of both algorithms
# from Lopez-Oriona 2021

# Include the needed libraries
library(dplyr)
library(mlmts)

# Read the csv file
df <- read.csv("Database/argentina_test.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)


# Get the numbers of the weeks
weeks <- c(df$week)[!duplicated(c(df$week))]

# Subsetting the data.frame to create the list of matrices
mts <- list()
counter <- 1

for (i in weeks) {
    if (i != 0) {
        mat <- unname(data.matrix(select(filter(df, week == i), c(so2, no, no2, co, pm10, o3, pm2.5, ben))))

        if (length(mat) == 56) {
            # Add a new row which contains the mean of every column
            mat <- rbind(mat, unname(round(colMeans(mat), digits = 2)))
            mts[[counter]] <- mat
        }

        counter <- counter + 1
    }
}

for (i in mts) {
    print(length(i))
}

# Test if it works
outliers <- outlier_detection(mts)

print(outliers$Indexes)
print(outliers$Depths)