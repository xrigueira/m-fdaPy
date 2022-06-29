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
# source("u-plotter.R")
# source("m-plotter.R")

# Define the variables for the desired time units
time_frame <- "b" # "a" for months, "b" for weeks, "c" for days
span <- "c" # This variable is to select different combinations later

# Call the functions to get the results
mts <- builder(time_frame = time_frame, span = span)

# Shape outlier detection
shape_outliers <- shape_outdec(mts)

# Magnitude outlier detection
magnitude_outliers <- magnitude_outdec(mts)

# CONTINUAR PASANDO LO DE GLOB-OUTDEC A UNA FUNCIÓN Y METERLA AQUÍ

# uni_grafic <- u_plotter(mts, variable = 1)

# Aquí iría la fución de plot multivariable