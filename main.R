# TODO: this file will contain the implementation of both algorithms
# from Lopez-Oriona 2021

# Include the needed libraries
library(dplyr)
library(mlmts)

# Read the csv file
df <- read.csv("Database/argentina_test.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)


# Get the numbers of the weeks
weeks <- c(df$weekOrder)[!duplicated(c(df$weekOrder))]

# Subsetting in the data.frame to create the list of matrices
mts <- list()
counter <- 1

for (i in weeks) {
    if (i != 0) {
        mat <- unname(data.matrix(select(filter(df, week == i), c(so2, no, no2, co, pm10, o3, pm2.5, ben))))
        mts[[counter]] <- mat

        counter <- counter + 1
    }
}

mts_test <- list()
# mat1 <- matrix(rnorm(16), nrow = 4)
# mat2 <- matrix(rnorm(16), nrow = 4)
# mat3 <- matrix(rnorm(16), nrow = 4)
# mat4 <- matrix(rnorm(16), nrow = 4)

# mat1 <- matrix(rnorm(56), nrow = 8)
# mat2 <- matrix(rnorm(56), nrow = 8)
# mat3 <- matrix(rnorm(56), nrow = 8)
# mat4 <- matrix(rnorm(56), nrow = 8)

mat1 <- matrix(rnorm(672), nrow = 96)
print(mat1)
mat2 <- matrix(rnorm(672), nrow = 96)
mat3 <- matrix(rnorm(672), nrow = 96)
mat4 <- matrix(rnorm(672), nrow = 96)

mts_test[[1]] <- mat1
mts_test[[2]] <- mat2
mts_test[[3]] <- mat3
mts_test[[4]] <- mat4


outliers <- outlier_detection(mts_test)

# print(str(SyntheticData1))
# print(SyntheticData1$data[[1]])
# print(class(SyntheticData1$data[[1]]))
# print(str(SyntheticData1$data[[1]]))