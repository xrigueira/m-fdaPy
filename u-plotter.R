# This function plots all functions of a specific variable
# in an interactive way

u_plotter <- function(mts, variable) {

    output <- matrix(ncol = length(mts$data), nrow = nrow(mts$data[[1]]))
    counter <- 1

    for (i in mts$data) {

        output[, counter] <- i[, variable] # This variable here is the number we have to change
        # depending on which variable we want, co2, no2 etc are in different columsn and
        # this number is the column we want

        counter <- counter + 1

    }

    # Insert column names
    colnames(output) <- mts$time
    output <- cbind(output, time = seq_len(nrow(mts$data[[1]])))

    # Convert to data.frame
    output <- as.data.frame(output)

    # Plotting
    data_frame <- melt(output, id.vars = "time", variable.name = "series")

    # plot on same grid, each series colored differently -- 
    # good if the series have same scale
    plot_object <- add_lines(plot_ly(data_frame, x = ~time, y = ~value, color = ~series, colors = "Paired"))

    # With ggplot -> it can be converted to plotly with "ggplotly"
    # plot_object <- ggplot(data_frame, aes(time, value)) + geom_line(aes(colour = series))

    return(plot_object)

}