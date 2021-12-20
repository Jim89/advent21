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

extend_image <- function(image, i = 1, pad_with = 0) {
    .extend_image <- function(image) {
        new_row <- rep(pad_with, ncol(image))
        new_col <- rep(pad_with, nrow(image) + 2)

        .out <- cbind(new_col, rbind(new_row, image, new_row), new_col)
        rownames(.out) <- colnames(.out) <- NULL
        .out
    }

    for (j in seq_len(i)) {
        image <- .extend_image(image)
    }
    return(image)
}






enhance_once <- function(image, algo, pad_with = 0) {
    image <- extend_image(image, i = 3, pad_with = pad_with)

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
    out[-c(1, nrow(out)), -c(1, ncol(out))]
}

enhance <- function(image, algo, times = 1) {
    for (iter in seq_len(times)) {
        pad_with <- if (iter %% 2 == 0) 1 else 0
        # Something around filling in with the first character of the algo,
        # depending on whether the iteration is 1st, 2nd, 3rd, (even) etc
        enhanced <- enhance_once(image, algo, pad_with = pad_with)
        image <- enhanced
    }
    image
}


# Part 1
sample_algo <- get_algorithm("20/sample.txt")
sample_input <- get_input("20/sample.txt")

sample_input |> enhance(sample_algo, 2) |> sum() # 35


algo <- get_algorithm("20/input.txt")
input <- get_input("20/input.txt")
input |> enhance(algo, 2) |> sum() # 5483 is correct for me

# Part 2
sample_input |> enhance(sample_algo, 50) |> sum() # 3351

input |> enhance(algo, 50) |> sum() # Current answer: 20330, but this is wrong





