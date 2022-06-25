library(tidyverse)
library(ggplot2)
library(reshape2)

output <- matrix(ncol = length(mts$data), nrow = 32)
counter <- 1

for (i in mts$data) {

    output[, counter] <- i[, 1] # This 1 here is the number we have to change
    # depending on which variable we want (co2, no2 etc are in different columsn and
    # this number is the column we want

    counter <- counter + 1

}

# Insert column names
colnames(output) <- mts$time
output <- cbind(output, time = c(1:32))

# Converto to tibble
output <- as.data.frame(output)

# Plotting
df <- melt(output, id.vars = "time", variable.name = "series")

# plot on same grid, each series colored differently -- 
# good if the series have same scale
plot1 <- ggplot(df, aes(time, value)) + geom_line(aes(colour = series))
