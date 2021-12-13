read_dots <- function(.path, ..., .split = "") {
    readLines(.path, ...) |>
        lapply(strsplit, .split) |>
        lapply(`[[`, 1) |>
        lapply(as.numeric) |>
        (\(x) do.call(rbind, x))()
}

read_matrix <- function(.path, ..., .split = "") {
    dots <- read_dots(.path, ..., .split = .split)
    x <- dots[, 2] + 1 # Because R is 1- not 0-indexed
    y <- dots[, 1] + 1 # Because R is 1- not 0-indexed

    dots_at <- cbind(x, y)

    extent_x <- max(x)
    extent_y <- max(y)
    .m <- matrix(FALSE, extent_x, extent_y)
    .m[dots_at] <- TRUE
    .m
}

read_folds <- function(.path, ...) {
    raw_lines <- readLines(.path, ...)
    mat_lines <- sapply(raw_lines, \(.line) "," %in% strsplit(.line, "")[[1]])
    fold_lines <- raw_lines[!mat_lines]
    fold_lines <- fold_lines[!fold_lines == ""]
    fold_lines |>
        lapply(strsplit, "=") |>
        lapply(`[[`, 1)
}


fold_up <- function(.matrix, .row) {

    top_half <- .matrix[1:.row-1, , drop = FALSE]

    folded_bottom_half <- .matrix[nrow(.matrix):(.row+1), , drop = FALSE]

    top_half | folded_bottom_half

}

fold_left <- function(.matrix, .col) {
    left_half <- .matrix[, 1:(.col-1), drop = FALSE]
    right_half_folded <- .matrix[, ncol(.matrix):(.col+1), drop = FALSE]

    if (ncol(left_half) != ncol(right_half_folded)) {
        filler <- matrix(FALSE, nrow(left_half), ncol = abs(ncol(left_half) - ncol(right_half_folded)))
        if (ncol(left_half) < ncol(right_half_folded)) {
            left_half <- cbind(filler, left_half)
        } else {
            right_half_folded <- cbind(filler, right_half_folded)
        }
    }

    left_half | right_half_folded
}


fold <- function(.matrix, .fold_instructions) {

    .fold_value <- as.integer(.fold_instructions[[2]]) + 1

    .fold_function <- if (stringr::str_detect(.fold_instructions[[1]], "y")) fold_up else fold_left


    .fold_function(.matrix, .fold_value)
}



sample_mat <- read_matrix("13/sample.txt", 18, .split = ",")
sample_folds <- read_folds("13/sample.txt")

sample_mat |>
    fold(sample_folds[[1]]) |>
    fold(sample_folds[[2]])


mat <- read_matrix("13/input.txt", n = 866, .split = ",")
folds <- read_folds("13/input.txt")
mat |>
    fold(folds[[1]]) |>
    sum()
