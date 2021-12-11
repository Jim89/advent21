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


spread_flash_from_point <- function(.flash_point, .flash_mask, .octopi, .other_flashers) {
    # Get the area around it
    neighbourhood <- neighbours(.flash_point)

    # That you haven't already been to
    flashed_already <- neighbourhood |>
        apply(1, \(neighbour) apply(.other_flashers, 1, \(row) all(row == neighbour))) |>
        apply(2, any)

    if (all(flashed_already)) {
        .out <- list(
            flashes = .flash_mask,
            octopi = .octopi,
            other_flashers = .other_flashers
        )
        return(.out)
    }

    neighbourhood <- neighbourhood[!flashed_already, ]

    # Increase the energy in the neighbourhood
    .octopi[neighbourhood] <- .octopi[neighbourhood] + 1

    # Find new flashing points
    neighbourhood <- neighbourhood[!is.na(.octopi[neighbourhood]), ] # Deals with NA padding around the edge
    new_flashes <- neighbourhood[.octopi[neighbourhood] > 9, , drop = FALSE]

    if (nrow(new_flashes) > 0) {
        .flash_mask[new_flashes] <- TRUE
        .other_flashers <- rbind(.other_flashers, new_flashes)
        for (row_id in seq_len(nrow(new_flashes)) ) {
            .new_point <- new_flashes[row_id, ]
            .out <- spread_flash_from_point(.new_point, .flash_mask, .octopi, .other_flashers)
        }
    } else {
        .out <- list(
            flashes = .flash_mask,
            octopi = .octopi,
            other_flashers = .other_flashers
        )
        return(.out)
    }
    return(.out)
}



sampmat


x <- read_matrix( "11/sample.txt")
x <- x + 1
flashing <- flashed <- x == 10


add_neigh <- function(.x) {
    I <- nrow(.x)
    J <- ncol(.x)
    cbind(.x[, -1], 0) +
    rbind(.x[-1, ], 0) +
    cbind(0, .x[, -J]) +
    rbind(0, .x[-I, ]) +
    rbind(cbind(.x[-1, -1], 0), 0) +
    rbind(0, cbind(.x[-I, -1], 0)) +
    rbind(cbind(0, .x[-1, -J]), 0) +
    rbind(0, cbind(0, .x[-I, -J]))
}


