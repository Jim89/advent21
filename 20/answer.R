get_algorithm <- function(inpath) {
    raw <- scan(file = inpath, nmax = 1, sep = "", what = character())
    cleanup(raw)
}

get_input <- function(inpath) {
    lines <- readLines(inpath)[-c(1, 2)]
    lines |>
        lapply(cleanup) |>
        (\(x) do.call(rbind, x))()
}

cleanup <- function(line) {
    clean <- line |>
        stringr::str_replace_all("\\.", "0") |>
        stringr::str_replace_all("\\#", "1") |>
        strsplit("")

    as.numeric(clean[[1]])
}

extend_image <- function(image, i = 1) {
    .extend_image <- function(image) {
        new_row <- rep(0, ncol(image))
        new_col <- rep(0, nrow(image) + 2)

        .out <- cbind(new_col, rbind(new_row, image, new_row), new_col)
        rownames(.out) <- colnames(.out) <- NULL
        .out
    }

    for (j in seq_len(i)) {
        image <- .extend_image(image)
    }
    return(image)
}






enhance_once <- function(image, algo, debug = F) {
    out <- matrix(0, nrow(image), ncol(image))
    for (row in seq(2, nrow(image)  - 1)) {
        for (col in seq(2, ncol(image) - 1)) {
            above <- image[row - 1, c(col - 1, col, col + 1), drop = TRUE]
            containing <- image[row, c(col - 1, col, col + 1), drop = TRUE]
            below <- image[row + 1, c(col - 1, col, col + 1), drop = TRUE]

            number_str <- paste0(c(above, containing, below), collapse = "")
            number <- strtoi(number_str, 2)

            value <- algo[number + 1] # assumes 0-indexed, but not in R!

            out[row, col] <- value
        }
    }
    out
}

enhance <- function(image, algo, times = 1) {
    for (iter in seq_len(times)) {
        if (iter == 1) extended <- extend_image(image, 2) else extended <- extend_image(image)
        enhanced <- enhance_once(extended, algo)
        image <- enhanced
    }
    image
}


# Part 1
sample_algo <- get_algorithm("20/sample.txt")
sample_input <- get_input("20/sample.txt")

sample_input |>
    enhance(sample_algo, 2) |>
    sum()

