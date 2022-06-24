library(mlmts)

mts <- list()
counter <- 1

while (counter <= 20) {

    if ((counter %% 10) == 0) {

        mat <- matrix(rnorm(n = 1344, mean = 100, sd = 100), nrow = 672)
        mts[[counter]] <- mat

    } else {

        mat <- matrix(rnorm(n = 1344, mean = 0, sd = 1), nrow = 672)
        mts[[counter]] <- mat

    }

    counter <- counter + 1

}

matplot(mts[[1]], type = "l")
matplot(mts[[10]], type = "l")

# outliers <- outlier_detection(mts)


# print(outliers$Depths)
