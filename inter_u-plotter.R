# The second function plots all functions of a specific
# variable in an interactive way

i_plot_autofeeder <- function(i, data, outliers, series, inter_plot_object) {

    if (colnames(data)[i] %in% names(outliers)) {

        inter_plot_object <- inter_plot_object %>%
            add_lines(x = ~time, y = ~data[, i], name = series[i], color = I("red"))

    } else {

        inter_plot_object <- inter_plot_object %>%
            add_lines(x = ~time, y = ~data[, i], name = series[i], color = I("black"))

    }

    return(inter_plot_object)

}

inter_u_plotter <- function(mts, outliers, variable) {

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
    inter_plot_object <- plot_ly(data) %>%
        layout(title = "Functional S02 monthly data", plot_bgcolor = "ebe7d8", xaxis = list(title = "Time (days)"), 
        yaxis = list(title = "Value"), legend = list(title=list(text='<b> Dates </b>')))
        # ggtitle(glue("Functional S02 monthly data")) +
        # xlab("Time (days)") +
        # ylab("Value" ~ (mu*g/m^3)) # plotly does not take this kind of "math mode" label

    # Add all the remaining curves with a loop
    for (i in seq_len(ncol(data) - 1)) {

        inter_plot_object <- i_plot_autofeeder(i, data, outliers, series, inter_plot_object)

    }

    return(inter_plot_object)

}
