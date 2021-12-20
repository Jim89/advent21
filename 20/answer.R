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

extend_image <- function(image, pad_with = 0) {
    .extend_image <- function(image) {
        new_row <- rep(pad_with, ncol(image))
        new_col <- rep(pad_with, nrow(image) + 2)

        .out <- cbind(new_col, rbind(new_row, image, new_row), new_col)
        rownames(.out) <- colnames(.out) <- NULL
        .out
    }

    # Extend it twice, to make it easier to grab the 9 cells around & including
    # a given point
    image |>
        .extend_image() |>
        .extend_image()
}

enhance_once <- function(image, algo, pad_with = 0) {
    image <- extend_image(image, pad_with = pad_with)

    out <- matrix(0, nrow(image), ncol(image))
    # Only loop from 2nd row/col, to ignore the outer edge where finding the
    # 9-cell region is annoying
    for (row in seq(2, nrow(image)  - 1)) {
        for (col in seq(2, ncol(image) - 1)) {
            col_indices <- c(col - 1, col, col + 1)
            above <- image[row - 1, col_indices, drop = TRUE]
            containing <- image[row, col_indices, drop = TRUE]
            below <- image[row + 1, col_indices, drop = TRUE]

            number_str <- paste0(c(above, containing, below), collapse = "")
            number <- strtoi(number_str, 2)

            value <- algo[number + 1] # assumes 0-indexed, but not in R, so add 1!

            out[row, col] <- value
        }
    }
    # Chop the outer border that we didn't update/don't care about
    out[-c(1, nrow(out)), -c(1, ncol(out))]
}

enhance <- function(image, algo, times = 1) {
    for (iter in seq_len(times)) {
        if (algo[1] == 0) {
            # If all ./0's stay "off", then can always pad with 0
            pad_with <- 0
        } else {
            # Otherwise, the infinite expanse switches on/off on each subsequent step
            pad_with <- if (iter %% 2 == 0) 1 else 0
        }
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

input |> enhance(algo, 50) |> sum() # 18732





