# This file calculates the final depth, which is a combination
# of the magnitud depth and the shape depth

global_outdec <- function(mts) {
    
}

nombres <- as.character(mts$time)

ms_depth <- c()
counter <- 1

for (i in nombres) {

    sha_depth <- shape_outliers[[i]]
    mag_depth <- magnitude_outliers[[i]]

    # Here is where we can make a few tweaks to change the way of each depth
    tot_depth <- mag_depth + sha_depth

    ms_depth[[counter]] <- tot_depth

    counter <- counter + 1

}

names(ms_depth) <- nombres
ms_depth <- sort(unlist(ms_depth))
