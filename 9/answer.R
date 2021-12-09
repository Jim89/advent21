read_matrix <- function(.path) {
    readLines(.path) |>
        lapply(strsplit, "") |>
        lapply(`[[`, 1) |>
        lapply(as.numeric) |>
        (\(x) do.call(rbind, x))()
}

mat <- read_matrix("9/sample.txt")


# Find the low points, iterating over rows and columns
# Score the low points
sum(low_points[!is.na(low_points)] + 1)

risk_level <- function(.mat) {
    low_points <- matrix(NA_real_, nrow(.mat), ncol(.mat))

    for (row in seq_len(nrow(.mat))) {
        for (col in seq_len(ncol(.mat))) {
            value <- .mat[row, col]

            above <- if (row == 1) NA_real_ else .mat[row - 1, col]
            below <- if (row == nrow(.mat)) NA_real_ else .mat[row + 1, col]
            left <- if (col == 1) NA_real_ else .mat[row, col - 1]
            right <- if (col == ncol(.mat)) NA_real_ else .mat[row, col + 1]

            others <- c(above, below, left, right)
            others <- others[!is.na(others)]
            low <- all(value < others)
            if (low) low_points[row, col] <- value
        }
    }
}