# The "vars" have to be assigned with "global_depth"
# after every run of main.R for the desired variables

# Get the names
names <- sort(names(var1))

# Get the indix of each name in each variable
indexes_var1 <- c()
counter <- 1
for (i in names) { # TURN THIS BLOCKS OF CODE INTO A FUNCTION

    indexes_var1[counter] <- match(var1[i], var1)

    counter <- counter + 1

}

indexes_var2 <- c()
counter <- 1
for (i in names) {

    indexes_var2[counter] <- match(var2[i], var2)

    counter <- counter + 1

}

indexes_varn <- c()
counter <- 1
for (i in names) {

    indexes_varn[counter] <- match(varn[i], varn)

    counter <- counter + 1

}

# Create de df
df <- data.frame(indexes_var1, indexes_var2, indexes_varn)
rownames(df) <- names # Inser the row names

# Calculate the variance of the indexes
df$row_var <- apply(df[, -1], 1, var)

# Get the mean of the variance
outliers_corr <- mean(sort(df$row_var))
print(outliers_corr)