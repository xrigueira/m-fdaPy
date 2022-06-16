# TODO: this file will contain the implementation of both algorithms
# from Lopez-Oriona 2021

# Include the needed libraries
library(dplyr)
library(mlmts)

# Read the csv file
df <- read.csv("Database/argentina_test.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

# Define the variables for the desired time units
time_frame <- "b" # "a" for months, "b" for weeks, "c" for days
span <- "a" # This variable is to select different combinations later

# Get the number of years in the database
years <- c(df$year)[!duplicated(c(df$year))]

# Get the number the months in the database
months <- c(df$month)[!duplicated(c(df$month))]

# Get the numbers of the weeks
weeks <- c(df$week)[!duplicated(c(df$week))]

# Get the number of the days available in the database
days <- c(df$day)[!duplicated(c(df$day))]


# Subsetting the data.frame to create the list of matrices
mts <- list()
time_stamps <- list()
counter <- 1

if (time_frame == "a") {

    if (span == "a") {

        for (i in years) {

            for (j in months) {

                mat <- unname(data.matrix(select(filter(df, year == i & month == j), c(so2, no, no2, co, pm10, o3, pm2.5, ben))))

                if ((nrow(mat) %% 2) == 1) {

                    # Add a new row which contains the mean of every column
                    mat <- rbind(mat, unname(round(colMeans(mat), digits = 2)))

                    if (nrow(mat) == 32) { # 32 because it is the number of rows in a month after fixing the matrix

                        mts[[counter]] <- mat

                        # Add the time stamps
                        time_stamps[[counter]] <- c(j, i)

                    }

                } else if ((nrow(mat) %% 2) == 0) {

                    if (nrow(mat) == 32) {

                        mts[[counter]] <- mat

                        time_stamps[[counter]] <- c(j, i)

                    }

                }

                counter <- counter + 1

            }

        }

    } else if (span == "b") {
        # TODO
    }

} else if (time_frame == "b") {

    if (span == "a") {

        for (i in weeks) {

            if (i != 0) {

                mat <- unname(data.matrix(select(filter(df, week == i), c(so2, no, no2, co, pm10, o3, pm2.5, ben))))

                if ((nrow(mat) %% 2) == 1) {

                    # Add a new row which contains the mean of every column
                    mat <- rbind(mat, unname(round(colMeans(mat), digits = 2)))

                    if (nrow(mat) == 8) { # 8 because it is the number of rows in a week after fixing the matrix

                        mts[[counter]] <- mat

                        # Add the time stamps
                        time_stamps[[counter]] <- c(i)

                    }

                } else if ((nrow(mat) %% 2) == 0) {

                    if (nrow(mat) == 8) {

                        mts[[counter]] <- mat

                        # Add the time stamps
                        time_stamps[[counter]] <- c(i)

                    }

                }

                counter <- counter + 1

            }

        }

    } else if (span == "b") {
        
        # TODO
    }

} else if (time_frame == "c") {

    if (span == "a") {

        for (i in years) {

            for (j in months) {

                for (k in days) {

                    mat <- unname(data.matrix(select(filter(df, year == i & month == j & day == k), c(so2, no, no2, co, pm10, o3, pm2.5, ben))))

                    if ((nrow(mat) %% 2) == 1) {

                        # Add a new row which contains the mean of every column
                        mat <- rbind(mat, unname(round(colMeans(mat), digits = 2)))

                        if (nrow(mat) == 1) { # 1 because it is the number of rows in a month after fixing the matrix

                            mts[[counter]] <- mat

                            # Add the time stamps
                            time_stamps[[counter]] <- c(k, j, i)

                        }

                    } else if ((nrow(mat) %% 2) == 0) {

                        if (nrow(mat) == 32) {

                            mts[[counter]] <- mat

                            time_stamps[[counter]] <- c(j, i)

                        }

                    }

                    counter <- counter + 1

                }

            }

        }

    } else if (span == "b") {

        # TODO
    }

}


# for (i in mts) {
#     print(length(i))
# }

# Test if it works
outliers <- outlier_detection(mts)


print(outliers$Indexes)
print(outliers$Depths)
print(time_stamps[order(outliers$Indexes)])