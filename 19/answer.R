get_beacons <- function(fp) {
    # Scan the file
    input_lines <- fp |>
        readLines() |>
        stringr::str_trim()

    scanner_positions <- which(sapply(input_lines, \(l) substr(l, 1, 3) == "---"))
    scanners <- stringr::str_extract(input_lines[scanner_positions], "\\d+")

    gap_positions <- c(0, which(input_lines == ""))

    scanners |>
        seq_along() |>
        lapply(\(i) {
            scanner <- scanners[[i]]
            skip <- gap_positions[[i]] + 1
            # Figure out how many rows to read per scanner
            if (i == length(scanners)) nrows <- -1 else nrows <- gap_positions[[(i + 1)]] - gap_positions[[i]] - 2
            beacons <- read.csv(fp, header = FALSE, skip = gap_positions[i] + 1, nrows = nrows, col.names = c("x", "y", "z"))
            as.matrix(beacons)
    })
}

apply_rotations <- function(beacons) {
    rotations <- expand.grid(x = c(1, -1), y = c(1, -1), z = c(1, -1))

    rotate <- function(what) {
        apply(rotations, 1, \(row) t(row * t(what)), simplify = FALSE)
    }

    orientations <- list(
        c("x", "y", "z"),
        c("x", "z", "y"),
        c("y", "x", "z"),
        c("y", "z", "x"),
        c("z", "y", "x"),
        c("z", "x", "y")
    )

    all_orientations <- orientations |>
        lapply(\(orientation) rotate(beacons[, orientation])) |>
        purrr::flatten()

    lapply(all_orientations, \(m){ colnames(m) <- orientations[[1]]; m})
}

count_beacons <- function(scanners, debug = FALSE, beacons = TRUE) {
    if (debug) browser()

    #take fist scanner data as start point
    beacon_locations <- scanners[[1]][[1]]

    # build up scan indexes (don't scan the first against itself)
    to_scan <- seq(2, length(scanners))

    # Hold scanner locations
    scanner_locations <- c(0, 0, 0)

    while (length(to_scan) > 0) {

        # For every other scanner's set of rotations
        for (i in to_scan) {
            #print(paste("Processing scanner: ", i-1))
            k <- length(scanners[[i]])

            # Compare against the other scanners
            for (j in seq_along(scanners[[i]])) {
                distances <- apply(scanners[[i]][[j]], 1, \(x) {
                    m <- sweep(beacon_locations, 2, x)
                    paste(m[, 1], m[, 2], m[, 3])
                })
                distances_comb <- table(as.vector(distances))
                if (any(distances_comb >= 12)) break
            }


            # If we didn't find any overlap then move on to next scanner data
            if (j == length(scanners[[i]]) && !any(distances_comb >= 12)) next

            # Otherwise, we've found the overlaps for that scanner, so chuck it
            to_scan <- setdiff(to_scan, i)


            # Find scanner + (new) beacon locations
            new_scanner_location <- as.numeric(strsplit(names(distances_comb[distances_comb >= 12]), " ")[[1]])

            scanner_locations <- rbind(scanner_locations, new_scanner_location)

            beacon_locations2 <- rbind(beacon_locations, sweep(scanners[[i]][[j]], 2, new_scanner_location, "+"))

            beacon_locations <- beacon_locations2[!duplicated(beacon_locations2), ]
        }
    }

    if (beacons) beacon_locations else (scanner_locations)
}

# Part 1
get_beacons("19/sample.txt") |>
    lapply(apply_rotations) |>
    count_beacons() |>
    nrow()


get_beacons("19/input.txt") |>
    lapply(apply_rotations) |>
    count_beacons() |>
    nrow()


# Part 2
locations <- get_beacons("19/sample.txt") |>
    lapply(apply_rotations) |>
    count_beacons(beacons = F)

max(apply(locations, 1, \(row1) apply(locations, 1, \(row2) sum(abs(row1 - row2)))))

locations <- get_beacons("19/input.txt") |>
    lapply(apply_rotations) |>
    count_beacons(beacons = F)

max(apply(locations, 1, \(row1) apply(locations, 1, \(row2) sum(abs(row1 - row2)))))
