library(tidyverse)
library(plotly)
library(mlmts)
library(gapminder)

plot1 <- ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy))

plot2 <- ggplot(data = mpg) +
    geom_point(mapping = aes(x = hwy, y = cyl))

plot3 <- ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy, color = class, shape = class))

plot4 <- ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy)) +
    facet_wrap(~ class, nrow = 3)

plot5 <- ggplot(data = mpg, (mapping = aes(x = displ, y = hwy))) +
    geom_point(mapping = aes(color = class)) +
    geom_smooth()

a <- matrix(rnorm(100), c(10, 10))
sa <- stack(as.data.frame(a))
sa$x <- rep(seq_len(nrow(a)), ncol(a))
plot6 <- qplot(x, values, data = sa, group = ind, colour = ind, geom = "line")

plot7 <- gapminder %>%
    filter(year == 1977) %>%
    ggplot(aes(gdpPercap, lifeExp, size = pop, color = continent)) +
    geom_point()

plot(plot6)

ggplotly(plot7) # I still have to run this in the terminal

