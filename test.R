mat <- matrix(data = c(3, 8, 1, 5, 4, 6, 2, 1, 9, 10, 7, 5), nrow = 3, ncol = 4)

print(mat)

col_means <- round(colMeans(mat), digit = 0)

mat <- rbind(mat, unname(col_means))

print(class(mat))
