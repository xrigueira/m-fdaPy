# This file, which has to evolve into a function
# plots all functions/variables of a specific time frame

library(glue)
library(fda.usc)

time_unit <- 1

data <- mts$data[[time_unit]]

data <- cbind(data, time = seq_len(nrow(mts$data[[1]])))

data <- as.data.frame(data)

data <- melt(data, id.vars = "time", variable.name = "series")

plot_object <- ggplot(data, aes(time, value)) +
    geom_line(aes(colour = series), size = 1) +
    labs(title = glue("Multivariate plot of the time interval #", time_unit), x = "Time (days)", y = "Value" ~ (mu*g/m^3))


functional_data <- fdata2fd(fdata(mts$data[[time_unit]][, c(4, 8)]), type.basis = "fourier", nbasis = 30)
