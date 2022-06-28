# TODO: this file will contain the implementation of both algorithms
# from Lopez-Oriona 2021

# Include the necessary libraries
# library(dplyr)
library(mlmts)
library(plotly)
library(tidyverse)

source("plotter.R")

# Function to get time data from the user
time_getter <- function() {

    total_timeunit <- as.integer(readline(prompt = "Enter the desired number of time units: "))

    x <- 0
    number_timeunit <- vector()

    while (x < total_timeunit) {

        data <- readline(prompt = "Enter each of the time units: ")

        number_timeunit <- c(number_timeunit, data)

        x <- x + 1
    }

    return(as.integer(number_timeunit))
}

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

# Read the csv file
df <- read.csv("Database/argentina_test.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

# Define the variables for the desired time units
time_frame <- "a" # "a" for months, "b" for weeks, "c" for days
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

    if (span == "a") { # All days

        for (i in years) {

            for (j in months) {

                mat <- (data.matrix(select(filter(df, year == i & month == j), c(so2, no, no2, co, pm10, o3, pm2.5, ben))))

                if ((nrow(mat) %% 2) == 1) {

                    # Add a new row which contains the mean of every column
                    mat <- rbind(mat, (round(colMeans(mat), digits = 2)))

                    if ((nrow(mat) == 32) & (nrow(mat) != 0)) { # 32 because it is the number of rows in a month after fixing the matrix

                        mts$data[[counter]] <- mat

                        # Add the time stamps
                        mts$time[[counter]] <- c(j, i)
                        # time_stamps[[counter]] <- c(j, i)

                        counter <- counter + 1

                    }

                } else if ((nrow(mat) %% 2) == 0) {

                    if ((nrow(mat) == 32) & (nrow(mat) != 0)) {

                        mts$data[[counter]] <- mat

                        mts$time[[counter]] <- c(j, i)
                        # time_stamps[[counter]] <- c(j, i)

                        counter <- counter + 1

                    }

                }

            }

        }

    } else if (span == "b") { # All days of one or several years

        number_years <- time_getter()

        for (i in number_years) {

            for (j in months) {

                mat <- (data.matrix(select(filter(df, year == i & month == j), c(so2, no, no2, co, pm10, o3, pm2.5, ben))))

                if ((nrow(mat) %% 2) == 1) {

                    # Add a new row which contains the mean of every column
                    mat <- rbind(mat, (round(colMeans(mat), digits = 2)))

                    if ((nrow(mat) == 2976) & (nrow(mat) != 0)) {

                        mts$data[[counter]] <- mat

                        # Add the time stamps
                        mts$time[[counter]] <- c(j, i)

                        counter <- counter + 1

                    }

                } else if ((nrow(mat) %% 2) == 0) {

                    if ((nrow(mat) == 2976) & (nrow(mat) != 0)) {

                        mts$data[[counter]] <- mat

                        mts$time[[counter]] <- c(j, i)

                        counter <- counter + 1

                    }

                }

            }

        }

    } else if (span == "c") { # A specific month in every year

        number_months <- time_getter()

        for (i in years) {

            for (j in number_months) {

                mat <- (data.matrix(select(filter(df, year == i & month == j), c(so2, no, no2, co, pm10, o3, pm2.5, ben))))

                if ((nrow(mat) %% 2) == 1) {

                    # Add a new row which contains the mean of every column
                    mat <- rbind(mat, (round(colMeans(mat), digits = 2)))

                    if ((nrow(mat) == 2976) & (nrow(mat) != 0)) {

                        mts$data[[counter]] <- mat

                        # Add the time stamps
                        mts$time[[counter]] <- c(j, i)

                        counter <- counter + 1

                    }

                } else if ((nrow(mat) %% 2) == 0) {

                    if ((nrow(mat) == 2976) & (nrow(mat) != 0)) {

                        mts$data[[counter]] <- mat

                        mts$time[[counter]] <- c(j, i)

                        counter <- counter + 1

                    }

                }

            }

        }

    } else if (span == "d") { # A combination of desired years and months

        number_years <- time_getter()
        number_months <- time_getter()

        for (i in number_years) {

            for (j in number_months) {

                mat <- (data.matrix(select(filter(df, year == i & month == j), c(so2, no, no2, co, pm10, o3, pm2.5, ben))))

                if ((nrow(mat) %% 2) == 1) {

                    # Add a new row which contains the mean of every column
                    mat <- rbind(mat, (round(colMeans(mat), digits = 2)))

                    if ((nrow(mat) == 2976) & (nrow(mat) != 0)) { # 32 because it is the number of rows in a month after fixing the matrix

                        mts$data[[counter]] <- mat

                        # Add the time stamps
                        mts$time[[counter]] <- c(j, i)

                        counter <- counter + 1

                    }

                } else if ((nrow(mat) %% 2) == 0) {

                    if ((nrow(mat) == 2976) & (nrow(mat) != 0)) {

                        mts$data[[counter]] <- mat

                        mts$time[[counter]] <- c(j, i)

                        counter <- counter + 1

                    }

                }

            }

        }

    } else if (span == "e") { # A range of months

        year_start <- as.integer(readline(prompt = "Enter the starting year: "))
        month_start <- as.integer(readline(prompt = "Enter the starting month: "))
        year_end <- as.integer((readline(prompt = "Enter the ending year: ")))
        month_end <- as.integer(readline(prompt = "Enter the ending month: "))

        for (i in years) {

            if (i >= year_start) {

                for (j in months) {

                    if (j >= month_start & i == year_start) {

                        mat <- (data.matrix(select(filter(df, year == i & month == j), c(so2, no, no2, co, pm10, o3, pm2.5, ben))))

                        if ((nrow(mat) %% 2) == 1) {

                            # Add a new row which contains the mean of every column
                            mat <- rbind(mat, (round(colMeans(mat), digits = 2)))

                            if ((nrow(mat) == 2976) & (nrow(mat) != 0)) { # 32 because it is the number of rows in a month after fixing the matrix

                                mts$data[[counter]] <- mat
                                # Add the time stamps
                                mts$time[[counter]] <- c(j, i)

                                counter <- counter + 1
                            }

                        } else if ((nrow(mat) %% 2) == 0) {

                            if ((nrow(mat) == 2976) & (nrow(mat) != 0)) {

                                mts$data[[counter]] <- mat
                                mts$time[[counter]] <- c(j, i)

                                counter <- counter + 1
                            }

                        }

                    } else if (i > year_start & i < year_end) {

                        mat <- (data.matrix(select(filter(df, year == i & month == j), c(so2, no, no2, co, pm10, o3, pm2.5, ben))))

                        if ((nrow(mat) %% 2) == 1) {

                            # Add a new row which contains the mean of every column
                            mat <- rbind(mat, (round(colMeans(mat), digits = 2)))

                            if ((nrow(mat) == 2976) & (nrow(mat) != 0)) { # 32 because it is the number of rows in a month after fixing the matrix

                                mts$data[[counter]] <- mat
                                # Add the time stamps
                                mts$time[[counter]] <- c(j, i)

                                counter <- counter + 1
                            }

                        } else if ((nrow(mat) %% 2) == 0) {

                            if ((nrow(mat) == 2976) & (nrow(mat) != 0)) {

                                mts$data[[counter]] <- mat
                                mts$time[[counter]] <- c(j, i)

                                counter <- counter + 1
                            }

                        }

                    } else if (j <= month_end & i == year_end) {

                        mat <- (data.matrix(select(filter(df, year == i & month == j), c(so2, no, no2, co, pm10, o3, pm2.5, ben))))

                        if ((nrow(mat) %% 2) == 1) {

                            # Add a new row which contains the mean of every column
                            mat <- rbind(mat, (round(colMeans(mat), digits = 2)))

                            if ((nrow(mat) == 2976) & (nrow(mat) != 0)) { # 32 because it is the number of rows in a month after fixing the matrix

                                mts$data[[counter]] <- mat
                                # Add the time stamps
                                mts$time[[counter]] <- c(j, i)

                                counter <- counter + 1
                            }

                        } else if ((nrow(mat) %% 2) == 0) {

                            if ((nrow(mat) == 2976) & (nrow(mat) != 0)) {

                                mts$data[[counter]] <- mat
                                mts$time[[counter]] <- c(j, i)

                                counter <- counter + 1
                            }

                        }

                    }
                }
            }
        }
    }

} else if (time_frame == "b") { # A range of weeks in several or all years

    if (span == "a") { # All weeks

        for (i in weeks) {

            if (i != 0) {

                mat <- (data.matrix(select(filter(df, week == i), c(so2, no, no2, co, pm10, o3, pm2.5, ben))))

                if ((nrow(mat) %% 2) == 1) {

                    # Add a new row which contains the mean of every column
                    mat <- rbind(mat, (round(colMeans(mat), digits = 2)))

                    if ((nrow(mat) == 672) & (nrow(mat) != 0)) { # 8 because it is the number of rows in a week after fixing the matrix

                        mts$data[[counter]] <- mat

                        # Add the time stamps
                        mts$time[[counter]] <- c(i)

                        counter <- counter + 1

                    }

                } else if ((nrow(mat) == 672) & (nrow(mat) != 0)) {

                    if (nrow(mat) == 8) {

                        mts$data[[counter]] <- mat

                        # Add the time stamps
                        mts$time[[counter]] <- c(i)

                        counter <- counter + 1

                    }

                }

            }

        }

    } else if (span == "b") { # 1st/2nd/3rd/4th week of each month

        week_number <- as.integer(readline(prompt = "Enter the week number: "))

        for (j in week_number) {

            df_sub <- filter(df, weekOrder == j)

            # Get the updated numbers of the weeks
            weeks <- c(df_sub$week)[!duplicated(c(df_sub$week))]

            for (k in weeks) {

                mat <- (data.matrix(select(filter(df_sub, weekOrder == j & week == k), c(so2, no, no2, co, pm10, o3, pm2.5, ben))))

                # Select the first 7 rows of the corresponding range for the columns startDate and endDate
                dates <- select(filter(df, weekOrder == j & week == k), c(startDate, endDate))[1:7, ]

                # Remove the dashes, unname it and turn into char
                dates <- as.character(unname(subset(dates, startDate != "-" & endDate != "-")))

                if ((nrow(mat) %% 2) == 1) {

                    # Add a new row which contains the mean of every column
                    mat <- rbind(mat, (round(colMeans(mat), digits = 2)))

                    if ((nrow(mat) == 672) & (nrow(mat) != 0)) {

                        mts$data[[counter]] <- mat

                        # Add the time stamps
                        mts$time[[counter]] <- dates

                        counter <- counter + 1

                    }

                } else if ((nrow(mat) %% 2) == 0) {

                    if ((nrow(mat) == 672) & (nrow(mat) != 0)) {

                        mts$data[[counter]] <- mat

                        mts$time[[counter]] <- dates

                        counter <- counter + 1

                    }

                }

            }
        }

    } else if (span == "d") { # Range of weeks

        year_begin <- as.integer(readline(prompt = "Enter the first year desired: "))
        month_begin <- as.integer(readline(prompt = "Enter the first month desired: "))
        order_begin <- as.integer(readline(prompt = "Enter the fist week number of the month desired: "))
        year_end <- as.integer(readline(prompt = "Enter the last year desired: "))
        month_end <- as.integer(readline(prompt = "Enter the last month desired: "))
        order_end <- as.integer(readline(prompt = "Enter the last week number of the month desired: "))

        # Get the indices to subset the data frame
        index_startDate <- as.numeric(rownames(df[df$year == year_begin & df$month == month_begin & df$weekOrder == order_begin, ])[1])
        index_endDate <- as.numeric(tail(rownames(df[df$year == year_end & df$month == month_end & df$weekOrder == order_end, ]), n = 1))

        df_sub <- df[index_startDate:index_endDate, ]

        # Get the updated numbers of the weeks
        weeks <- c(df_sub$week)[!duplicated(c(df_sub$week))]

        for (k in weeks) {

            mat <- (data.matrix(select(filter(df_sub, week == k), c(so2, no, no2, co, pm10, o3, pm2.5, ben))))

            # Select the first 7 rows of the corresponding range for the columns startDate and endDate
            dates <- select(filter(df_sub, week == k), c(startDate, endDate))[1:7, ]

            # Remove the dashes, unname it and turn into char
            dates <- as.character(unname(subset(dates, startDate != "-" & endDate != "-")))

            if ((nrow(mat) %% 2) == 1) {

                # Add a new row which contains the mean of every column
                mat <- rbind(mat, (round(colMeans(mat), digits = 2)))

                if ((nrow(mat) == 672) & (nrow(mat) != 0)) {

                    mts$data[[counter]] <- mat

                    # Add the time stamps
                    mts$time[[counter]] <- dates

                    counter <- counter + 1

                }

            } else if ((nrow(mat) %% 2) == 0) {

                if ((nrow(mat) == 672) & (nrow(mat) != 0)) {

                    mts$data[[counter]] <- mat

                    mts$time[[counter]] <- dates

                    counter <- counter + 1

                }

            }

        }

    } else if (span == "c") { # Range of weeks in several or all years
        
        # year_begin_init <- as.integer(readline(prompt = "Enter the first year desired: "))
        # month_begin <- as.integer(readline(prompt = "Enter the first month desired: "))
        # order_begin <- as.integer(readline(prompt = "Enter the fist week number of the month desired: "))
        # year_end <- as.integer(readline(prompt = "Enter the last year desired: "))
        # month_end <- as.integer(readline(prompt = "Enter the last month desired: "))
        # order_end <- as.integer(readline(prompt = "Enter the last week number of the month desired: "))

        year_begin_init <- 2014
        month_begin <- 1
        order_begin <- 1
        year_end <- 2021
        month_end <- 4
        order_end <- 4

        total_years <- year_end - year_begin_init

        for (i in seq(from = 0, to = total_years, by = 1)) {
            
            year_begin <- year_begin_init + i

            # Get the indices to subset the data frame
            index_startDate <- as.numeric(rownames(df[df$year == year_begin & df$month == month_begin & df$weekOrder == order_begin, ])[1])
            index_endDate <- as.numeric(tail(rownames(df[df$year == year_begin & df$month == month_end & df$weekOrder == order_end, ]), n = 1))

            df_sub <- df[index_startDate:index_endDate, ]

            # Get the updated numbers of the weeks
            weeks <- c(df_sub$week)[!duplicated(c(df_sub$week))]

            for (k in weeks) {

                mat <- (data.matrix(select(filter(df_sub, week == k), c(so2, no, no2, co, pm10, o3, pm2.5, ben))))

                # Select the first 7 rows of the corresponding range for the columns startDate and endDate
                dates <- select(filter(df_sub, week == k), c(startDate, endDate))[1:7, ]

                # Remove the dashes, unname it and turn into char
                dates <- as.character(unname(subset(dates, startDate != "-" & endDate != "-")))

                if ((nrow(mat) == 672)) { # This is a 7 when dealing with daily data

                    if ((nrow(mat) %% 2) == 1) {

                        # Add a new row which contains the mean of every column
                        mat <- rbind(mat, (round(colMeans(mat), digits = 2)))

                        if ((nrow(mat) == 672) & (nrow(mat) != 0)) {

                            mts$data[[counter]] <- mat
                            # Add the time stamps
                            mts$time[[counter]] <- dates

                            counter <- counter + 1

                        }

                    } else if ((nrow(mat) %% 2) == 0) {

                        if ((nrow(mat) == 672) & (nrow(mat) != 0)) {

                            mts$data[[counter]] <- mat
                            mts$time[[counter]] <- dates

                            counter <- counter + 1

                        }

                    }

                }

            }

        }

    }

} else if (time_frame == "c") {

    if (span == "a") {

        for (i in years) {

            for (j in months) {

                for (k in days) {

                    mat <- (data.matrix(select(filter(df, year == i & month == j & day == k), c(so2, no, no2, co, pm10, o3, pm2.5, ben))))

                    if ((nrow(mat) %% 2) == 1) {

                        # Add a new row which contains the mean of every column
                        mat <- rbind(mat, (round(colMeans(mat), digits = 2)))

                        if ((nrow(mat) == 96) & (nrow(mat) != 0)) { # 96 because it is the number of rows in a day after fixing the matrix

                            mts$data[[counter]] <- mat
                            mts$time[[counter]] <- c(k, j, i)

                            counter <- counter + 1

                        }

                    } else if ((nrow(mat) %% 2) == 0) {

                        if ((nrow(mat) == 96) & (nrow(mat) != 0)) {

                            mts$data[[counter]] <- mat
                            mts$time[[counter]] <- c(k, j, i)

                            counter <- counter + 1

                        }

                    }

                }

            }

        }

    } else if (span == "b") { # All days of one or several years

        number_years <- time_getter()

        for (i in number_years) {

            for (j in months) {

                for (k in days) {

                    mat <- (data.matrix(select(filter(df, year == i & month == j & day == k), c(so2, no, no2, co, pm10, o3, pm2.5, ben))))

                    if ((nrow(mat) %% 2) == 1) {

                        # Add a new row which contains the mean of every column
                        mat <- rbind(mat, (round(colMeans(mat), digits = 2)))

                        if ((nrow(mat) == 96) & (nrow(mat) != 0)) { # 32 because it is the number of rows in a month after fixing the matrix

                            mts$data[[counter]] <- mat
                            mts$time[[counter]] <- c(k, j, i)

                            counter <- counter + 1

                        }

                    } else if ((nrow(mat) %% 2) == 0) {

                        if ((nrow(mat) == 96) & (nrow(mat) != 0)) {

                            mts$data[[counter]] <- mat
                            mts$time[[counter]] <- c(k, j, i)

                            counter <- counter + 1

                        }

                    }

                }

            }

        }

    } else if (span == "c") { # All days of one or several months
        
        number_months <- time_getter()

        for (i in years) {

            for (j in number_months) {

                for (k in days) {

                    mat <- (data.matrix(select(filter(df, year == i & month == j & day == k), c(so2, no, no2, co, pm10, o3, pm2.5, ben))))

                    if ((nrow(mat) %% 2) == 1) {

                        # Add a new row which contains the mean of every column
                        mat <- rbind(mat, (round(colMeans(mat), digits = 2)))

                        if ((nrow(mat) == 96) & (nrow(mat) != 0)) {

                            mts$data[[counter]] <- mat
                            mts$time[[counter]] <- c(k, j, i)

                            counter <- counter + 1

                        }
                    } else if ((nrow(mat) %% 2) == 0) {

                        if ((nrow(mat) == 96) & (nrow(mat) != 0)) {

                            mts$data[[counter]] <- mat
                            mts$time[[counter]] <- c(k, j, i)

                            counter <- counter + 1

                        }

                    }

                }

            }

        }

    } else if (span == "d") { # One or a range of days in every month of every year

        number_days <- time_getter()

        for (i in years) {

            for (j in months) {

                for (k in number_days) {

                    mat <- (data.matrix(select(filter(df, year == i & month == j & day == k), c(so2, no, no2, co, pm10, o3, pm2.5, ben))))

                    if ((nrow(mat) %% 2) == 1) {

                        # Add a new row which contains the mean of every column
                        mat <- rbind(mat, (round(colMeans(mat), digits = 2)))

                        if ((nrow(mat) == 96) & (nrow(mat) != 0)) {

                            mts$data[[counter]] <- mat
                            mts$time[[counter]] <- c(k, j, i)

                            counter <- counter + 1

                        }

                    } else if ((nrow(mat) %% 2) == 0) {

                        if ((nrow(mat) == 96) & (nrow(mat) != 0)) {

                            mts$data[[counter]] <- mat
                            mts$time[[counter]] <- c(k, j, i)

                            counter <- counter + 1

                        }

                    }

                }

            }

        }

    } else if (span == "e") { # One or a range of days in several years

        number_years <- time_getter()
        number_days <- time_getter()

        for (i in number_years) {

            for (j in months) {

                for (k in number_days) {

                    mat <- (data.matrix(select(filter(df, year == i & month == j & day == k), c(so2, no, no2, co, pm10, o3, pm2.5, ben))))

                    if ((nrow(mat) %% 2) == 1) {

                        # Add a new row which contains the mean of every column
                        mat <- rbind(mat, (round(colMeans(mat), digits = 2)))

                        if ((nrow(mat) == 96) & (nrow(mat) != 0)) {

                            mts$data[[counter]] <- mat
                            mts$time[[counter]] <- c(k, j, i)

                            counter <- counter + 1

                        }

                    } else if ((nrow(mat) %% 2) == 0) {

                        if ((nrow(mat) == 96) & (nrow(mat) != 0)) {

                            mts$data[[counter]] <- mat
                            mts$time[[counter]] <- c(k, j, i)

                            counter <- counter + 1

                        }

                    }

                }

            }

        }

    } else if (span == "f") { # One or a range of days in several months in several years

        number_years <- time_getter()
        number_months <- time_getter()
        number_days <- time_getter()

        for (i in number_years) {

            for (j in number_months) {

                for (k in number_days) {

                    mat <- (data.matrix(select(filter(df, year == i & month == j & day == k), c(so2, no, no2, co, pm10, o3, pm2.5, ben))))

                    if ((nrow(mat) %% 2) == 1) {

                        # Add a new row which contains the mean of every column
                        mat <- rbind(mat, (round(colMeans(mat), digits = 2)))

                        if ((nrow(mat) == 96) & (nrow(mat) != 0)) {

                            mts$data[[counter]] <- mat
                            mts$time[[counter]] <- c(k, j, i)

                            counter <- counter + 1

                        }

                    } else if ((nrow(mat) %% 2) == 0) {

                        if ((nrow(mat) == 96) & (nrow(mat) != 0)) {

                            mts$data[[counter]] <- mat
                            mts$time[[counter]] <- c(k, j, i)

                            counter <- counter + 1

                        }

                    }

                }

            }

        }

    } else if (span == "g") { # A range of days

        year_begin <- as.integer(readline(prompt = "Enter the first year desired: "))
        month_begin <- as.integer(readline(prompt = "Enter the first month desired: "))
        day_begin <- as.integer(readline(prompt = "Enter the fist day desired: "))
        year_end <- as.integer(readline(prompt = "Enter the last year desired: "))
        month_end <- as.integer(readline(prompt = "Enter the last month desired: "))
        day_end <- as.integer(readline(prompt = "Enter the last day desired: "))

        # Get the indices to subset the data frame
        index_startDate <- as.numeric(rownames(df[df$year == year_begin & df$month == month_begin & df$day == day_begin, ])[1])
        index_endDate <- as.numeric(tail(rownames(df[df$year == year_end & df$month == month_end & df$day == day_end, ]), n = 1))

        df_sub <- df[index_startDate:index_endDate, ]

        # Get updated date info of the cropped database
        years <- c(df$year)[!duplicated(c(df$year))]
        months <- c(df$month)[!duplicated(c(df$month))]
        days <- c(df$day)[!duplicated(c(df$day))]

        for (i in years) {

            for (j in months) {

                for (k in days) {

                    mat <- (data.matrix(select(filter(df, year == i & month == j & day == k), c(so2, no, no2, co, pm10, o3, pm2.5, ben))))

                    if ((nrow(mat) %% 2) == 1) {

                        # Add a new row which contains the mean of every column
                        mat <- rbind(mat, (round(colMeans(mat), digits = 2)))

                        if ((nrow(mat) == 96) & (nrow(mat) != 0)) {

                            mts$data[[counter]] <- mat
                            mts$time[[counter]] <- c(k, j, i)

                            counter <- counter + 1

                        }

                    } else if ((nrow(mat) %% 2) == 0) {

                        if ((nrow(mat) == 96) & (nrow(mat) != 0)) {

                            mts$data[[counter]] <- mat
                            mts$time[[counter]] <- c(k, j, i)

                            counter <- counter + 1

                        }

                    }

                }

            }

        }

    }

}


# Shape outlier detection
outliers <- outlier_detection(mts$data)

# # Apply the outlying order to the time stamps
ordered_dates <- bully(list = mts$time, order = outliers$Indexes)

# print(outliers$Indexes)
print(outliers$Depths)
print(ordered_dates)

# # Plotting
grafic <- plotter(mts, variable = 1)

# Magnitude outlier detection
# I am texting stuff in test.R
