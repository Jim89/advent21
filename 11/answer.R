read_matrix <- function(.path) {
    readLines(.path) |>
        lapply(strsplit, "") |>
        lapply(`[[`, 1) |>
        lapply(as.numeric) |>
        (\(x) do.call(rbind, x))()
}

pad_mat <- function(mat) {
    rbind(NA, cbind(NA, mat, NA), NA)
}

neighbours <- function(indices) {
    N <- c(indices["row"] - 1, indices["col"])
    S <- c(indices["row"] + 1, indices["col"])
    E <- c(indices["row"], indices["col"] + 1)
    W <- c(indices["row"], indices["col"] - 1)

    NE <- c(indices["row"] - 1, indices["col"] + 1)
    NW <- c(indices["row"] - 1, indices["col"] - 1)
    SE <- c(indices["row"] + 1, indices["col"] + 1)
    SW <- c(indices["row"] + 1, indices["col"] - 1)

    rbind(N, S, E, W, NE, SE, SW, NW)
}

sampmat <- "11/sample.txt" |> read_matrix() |> pad_mat()
sampmat_x <- sampmat

# Set up if that cell has flashed this go round
flashed <- !sampmat_x >= 0

# A - Increase energy by 1
sampmat_x <- sampmat_x + 1

# Figure out which octopi have flashed
flash_mask <- sampmat_x > 9

# Find where they are, and mark them as having flashed
has_flashed <- which(flash_mask, arr.ind = TRUE)
flashed[has_flashed] <- TRUE

for ( row_id in seq_len(nrow(has_flashed)) ) {
    flashpoint <- has_flashed[row_id, ]
    updated <- spread_flash_from_point(flashpoint, flashed, sampmat_x, has_flashed)
    flashed <- updated$flashes
    sampmat_x <- updated$octopi
    has_flashed <- updated$other_flashers
}




count_flashes <- function(input, steps, part2 = FALSE) {
    x <- input
    count <- 0
    for (i in seq_len(steps)) {
        x <- step1(x)
        nflashes <- sum(x == 0)
        if (part2 & nflashes == prod(dim(x))) {
            return(i)
        }
        count <- count + nflashes
    }
    count
}

step1 <- function(x) {
    x <- x + 1
    flashing <- flashed <- x == 10
    while (any(flashing)) {
        to_increase <- increase_by(flashing)
        x <- x + to_increase
        flashing <- x > 9 & !flashed
        flashed <- flashing | flashed
    }
    x[x > 9] <- 0
    x
}

increase_by <- function(.flashing) {
    out <- matrix(0, nrow(.flashing), ncol(flashing))
    for (row in seq_len(nrow(.flashing))) {
        for (col in seq_len(ncol(.flashing))) {
            N <- if (row == 1) NA_real_ else .flashing[row - 1, col]
            S <- if (row == nrow(.flashing)) NA_real_ else .flashing[row + 1, col]
            E <- if (col == ncol(.flashing)) NA_real_ else .flashing[row, col + 1]
            W <- if (col == 1) NA_real_ else .flashing[row, col - 1]

            NE <- if (row == 1) NA_real_ else if(col == ncol(.flashing)) NA_real_ else .flashing[row - 1, col + 1]
            SE <- if (row == nrow(.flashing)) NA_real_ else if(col == ncol(.flashing)) NA_real_ else .flashing[row + 1, col + 1]

            NW <- if (row == 1) NA_real_ else if(col == 1) NA_real_ else .flashing[row - 1, col - 1]
            SW <- if (row == nrow(.flashing)) NA_real_ else if(col == 1) NA_real_ else .flashing[row - 1, col - 1]

            all <- c(N, S, E, W, NE, SE, SW, NW)
            out[row, col] <- sum(all, na.rm = TRUE)
        }
    }
    return(out)
}



x <- read_matrix("11/sample.txt")
count_flashes(x, 100, part2 = FALSE)

x <- read_matrix("11/input.txt")
count_flashes(x, 100, FALSE)

x <- read_matrix("11/sample.txt")
x <- x + 2
flashing <- x > 9




