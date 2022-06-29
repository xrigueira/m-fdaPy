# This file contains the implementation of the ms-outdec
# algorithm, which is a magnitude and shape outlier detector.
# The shape component is based on the CCR-periodogram proposed
# by Lopez-Oriona 2021, and the magnitude component is obtained
# with a weak multivariate version of the Fraiman-Muñiz depth.

# Load the libraries
library(tidyverse)
library(mlmts)
library(fda.usc)

# Load the files
source("builder.R")
source("sha-outdec.R")
source("mag-outdec.R")
source("u-plotter.R")
source("m-plotter.R")

# Define the variables for the desired time units
time_frame <- "a" # "a" for months, "b" for weeks, "c" for days
span <- "a" # This variable is to select different combinations later

# Call the functions to get the results
mts <- builder(time_frame = time_frame, span = span)

# Meter la función de los detectores de outliers
# Son funciones que aún hay que terminar. Falta
# darle nombre, return etc.

uni_grafic <- u_plotter(mts, variable = 1)

# Aquí iría la fución de plot multivariable