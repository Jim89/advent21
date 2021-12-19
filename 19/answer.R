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
    }) |>
        setNames(paste("scanner", scanners, sep = "_"))
}

apply_rotations <- function(beacons) {
    rotations <- expand.grid(x = c(-1, 1), y = c(-1, 1), z = c(-1, 1))

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

sample_beacons <- get_beacons("19/sample.txt")
rotated_beacons <- lapply(sample_beacons, apply_rotations)
