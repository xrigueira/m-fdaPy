library(ggplot2) 

# This file, which has to evolve into a function
# plots all functions/variables of a specific time frame

data <- mts$data[[1]]

data <- cbind(data, time = seq_len(nrow(mts$data[[1]])))

data <- as.data.frame(data)

data <- melt(data, id.vars = "time", variable.name = "series")

plot_object <- ggplot(data, aes(time, value)) + geom_line(aes(colour = series), size = 1)