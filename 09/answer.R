read_matrix <- function(.path) {
    readLines(.path) |>
        lapply(strsplit, "") |>
        lapply(`[[`, 1) |>
        lapply(as.numeric) |>
        (\(x) do.call(rbind, x))()
}

risk_level <- function(.mat) {
    low_points <- find_low_points(.mat)
    sum(low_points[!is.na(low_points)] + 1)
}

find_low_points <- function(.mat) {
    low_points <- matrix(NA_real_, nrow(.mat), ncol(.mat))
    # Find the low points, iterating over rows and columns
    for (row in seq_len(nrow(.mat))) {
        for (col in seq_len(ncol(.mat))) {
            low <- is_low_point(.mat, row, col)
            if (low) low_points[row, col] <- .mat[row, col]
        }
    }
    low_points
}

is_low_point <- function(.mat, row, col) {
    value <- .mat[row, col]
    if (value >= 9) return(FALSE)

    above <- if (row == 1) NA_real_ else .mat[row - 1, col]
    below <- if (row == nrow(.mat)) NA_real_ else .mat[row + 1, col]
    left <- if (col == 1) NA_real_ else .mat[row, col - 1]
    right <- if (col == ncol(.mat)) NA_real_ else .mat[row, col + 1]

    others <- c(above, below, left, right)
    others <- others[!is.na(others)]

    all(value <= others)
}

"9/input.txt" |>
    read_matrix() |>
    risk_level()


top_basins <- function(.mat, n = 3) {
    r <- raster::raster(.mat)
    rc <- raster::clump(r, directions = 4)

    clump_counts <- sort(table(as.vector(rc)), decreasing = TRUE)
    prod(clump_counts[1:n])
}


mat <- read_matrix("9/input.txt")
top_basins(mat != 9)



# -------------------------------------------------------------------------

find_basins <- function(.mat) {
    # This doesn't work for the input, but I'm not sure why!
    basins <- matrix(FALSE, nrow(.mat), ncol(.mat))
    tracker_mat <- .mat
    complete <- FALSE
    i <- 1
    rasters <- NULL
    while (!complete) {
        lows <- find_low_points(tracker_mat)
        low_points <- which(!is.na(lows), arr.ind = TRUE)
        if (nrow(low_points) == 0) {
            complete <- TRUE
            return(basins)
        }
        basins[low_points] <- TRUE
        tracker_mat[low_points] <- 9
        rasters[[i]] <- raster(basins)
        i <- i + 1
    }
    return(basins)
}

mat |>
    find_basins() |>
    top_basins()

tst_mat <- mat[1:10, 1:10]
me <- find_basins(tst_mat)
them <- tst_mat < 9

plot(raster(me))
plot(raster(them))

plot(me[[2]][[1]])
plot(me[[2]][[2]])
plot(me[[2]][[3]])
plot(me[[2]][[4]])
plot(me[[2]][[5]])
plot(me[[2]][[6]])
plot(me[[2]][[7]])
plot(me[[2]][[8]])
plot(me[[2]][[9]])

# Ok, it's because I don't handle <=, only <
