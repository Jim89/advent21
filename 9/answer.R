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

    above <- if (row == 1) NA_real_ else .mat[row - 1, col]
    below <- if (row == nrow(.mat)) NA_real_ else .mat[row + 1, col]
    left <- if (col == 1) NA_real_ else .mat[row, col - 1]
    right <- if (col == ncol(.mat)) NA_real_ else .mat[row, col + 1]

    others <- c(above, below, left, right)
    others <- others[!is.na(others)]

    all(value < others)
}

mat <- read_matrix("9/sample.txt")
risk_level(mat)

orig_mat <- read_matrix("9/sample.txt")
mat <- read_matrix("9/sample.txt")
basins <- matrix(FALSE, nrow(mat), ncol(mat))

lows <- find_low_points(mat)
low_points <- which(!is.na(lows), arr.ind = TRUE)

basin[low_points] <- TRUE

mat[low_points] <- 9

find_basins <- function(.mat) {
    basins <- matrix(FALSE, nrow(mat), ncol(mat))
    tracker_mat <- .mat
    complete <- FALSE
    while (!complete) {
        lows <- find_low_points(tracker_mat)
        low_points <- which(!is.na(lows), arr.ind = TRUE)
        if (nrow(low_points) == 0) {
            complete <- TRUE
            return(basins)
        }
        basins[low_points] <- TRUE
        tracker_mat[low_points] <- 9
    }
}

basins <- find_basins(orig_mat)
