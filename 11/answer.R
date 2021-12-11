read_matrix <- function(.path) {
    readLines(.path) |>
        lapply(strsplit, "") |>
        lapply(`[[`, 1) |>
        lapply(as.numeric) |>
        (\(x) do.call(rbind, x))()
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




