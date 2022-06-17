getter <- function() {

    total_years <- as.integer(readline(prompt = "Enter the desired number years: "))

    x <- 0
    number_years <- vector()

    while (x < total_years) {

        data <- readline(prompt = "Enter each of the years: ")

        number_years <- c(number_years, data)

        x <- x + 1
    }

    return(as.integer(number_years))
}

number_years <- getter()

for (i in number_years) {
    print(i)
}