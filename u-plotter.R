# The second function plots all functions of a specific
# variable

plot_autofeeder <- function(i, data, outliers, series, plot_object) {

    if (colnames(data)[i] %in% names(outliers)) {

        plot_object <- plot_object +
            geom_line(aes(x = time, y = data[, i],  group = series[i]), color = "red")


    } else {

        plot_object <- plot_object +
            geom_line(aes(x = time, y = data[, i],  group = series[i]), color = "black")

    }

    return(plot_object)

}

u_plotter <- function(mts, outliers, variable) {

    # Get the data of each desired variable and put it in a matrix (data)
    data <- matrix(ncol = length(mts$data), nrow = nrow(mts$data[[1]]))
    counter <- 1
    for (i in mts$data) {

        data[, counter] <- i[, variable] # This variable here is the number we have to change
        # depending on which variable we want, co2, no2 etc are in different columsn and
        # this number is the column we want

        counter <- counter + 1

    }

    colnames(data) <- mts$time
    data <- cbind(data, time = seq_len(nrow(mts$data[[1]])))
    data <- as.data.frame(data)
    series <- colnames(data)

    # Define the base plot
    plot_object <- ggplot(data) +
        ggtitle(glue("Functional S02 monthly data")) +
        xlab("Time (days)") +
        ylab("Value" ~ (mu*g/m^3)) # plotly does not take this kind of "math mode" label

    # Add all the remaining curves with a loop
    for (i in seq_len(ncol(data) - 1)) {

        plot_object <- plot_autofeeder(i, data, outliers, series, plot_object)

    }

    return(plot_object)

}
