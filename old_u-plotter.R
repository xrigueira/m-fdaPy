# This function plots all functions of a specific variable
# in an interactive way

u_plotter <- function(mts, variable) {

    data <- matrix(ncol = length(mts$data), nrow = nrow(mts$data[[1]]))
    counter <- 1

    for (i in mts$data) {

        data[, counter] <- i[, variable] # This variable here is the number we have to change
        # depending on which variable we want, co2, no2 etc are in different columsn and
        # this number is the column we want

        counter <- counter + 1

    }

    # Insert column names
    colnames(data) <- mts$time
    data <- cbind(data, time = seq_len(nrow(mts$data[[1]])))

    # Convert to data.frame
    data <- as.data.frame(data)
    data <- melt(data, id.vars = "time", variable.name = "series")

    # Create binary list with outliers as 1s
    binary_list <- c()
    counter <- 1
    for (i in data$series) {

        if (i %in% names(outliers)) {

            binary_list[counter] <- 1

        } else {

            binary_list[counter] <- 0

        }

        counter <- counter + 1
    }

    data <- cbind(data, binary = binary_list)

    # Plot on same grid, each series colored differently
    plot_object <- add_lines(plot_ly(data, x = ~time, y = ~value, color = ~series, colors = "Paired"))

    # With ggplot -> it can be converted to plotly with "ggplotly"
    plot_object <- ggplot(data, aes(time, value)) +
        geom_line(aes(colour = series), size = 1)
        labs(title = glue("Functional S02 monthly data"), x = "Time (days)", y = "Value" ~ (mu*g/m^3)) +
        theme(legend.position = "none")

    return(plot_object)

}