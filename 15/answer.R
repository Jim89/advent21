library(igraph)
read_matrix <- function(.path, ..., .split = "") {
    readLines(.path, ...) |>
        lapply(strsplit, .split) |>
        lapply(`[[`, 1) |>
        lapply(as.numeric) |>
        (\(x) do.call(rbind, x))()
}

neighbours <- function(.row, .col, .mat) {
    .cant_go <- NULL
    above <- if (.row == 1) .cant_go else c(.row - 1, .col)
    below <- if (.row == nrow(.mat)) .cant_go else c(.row + 1, .col)
    left <- if (.col == 1) .cant_go else c(.row, .col - 1)
    right <- if (.col == ncol(.mat)) .cant_go else c(.row, .col + 1)
    .coords <- rbind(above, below, left, right)
    .coords
}

build_edges <- function(.input) {
    .out <- vector("list", prod(dim(.input)))
    i <- 1
    for (.row in seq_len(nrow(.input))) {
        for (.col in seq_len(nrow(.input))) {

            .node <- paste0(.row, ",", .col)

            .position_neighbours <- neighbours(.row, .col, .input)
            .weights <- .input[.position_neighbours]

            .coords <- apply(.position_neighbours, 1, \(x) paste0(x[1], ",", x[2]))

            .edgelist <- cbind(.node, .coords, .weights)
            .out[[i]] <- .edgelist
            i <- i + 1

        }
    }
    .out <- do.call(rbind, .out)
    .out <- data.frame(.out)
    rownames(.out) <- NULL
    colnames(.out) <- c("from", "to", "weight")
    .out[["weight"]] <- as.integer(.out[["weight"]])
    .out
}


repeat_matrix <- function(.in_mat, cols = 5, rows = 5) {
    # First do across columns
    .cols <- list()
    for (i in seq_len(cols)) {
        if (i == 1) {
            .cols[[i]] <- .in_mat
        } else {
            .prev_mat <- .cols[[(i - 1)]]
            .new_mat <- .prev_mat + 1
            .new_mat[.new_mat > 9] <- 1
            .cols[[i]] <- .new_mat
        }
    }
    .first_row <- do.call(cbind, .cols)
    # Then do down the rows
    .rows <- list()
    for (i in seq_len(rows)) {
        if (i == 1) {
            .rows[[i]] <- .first_row
        } else {
            .prev_row <- .rows[[(i-1)]]
            .new_row <- .prev_row + 1
            .new_row[.new_row > 9] <- 1
            .rows[[i]] <- .new_row
        }
    }
    do.call(rbind, .rows)
}

find_path_cost <- function(.input, part2 = FALSE) {
    .mat <- read_matrix(.input)
    if (part2) {
        .expanded_mat <- repeat_matrix(.mat, 5, 5)
        .mat <- .expanded_mat
    }
    .edges <- build_edges(.mat)
    .graph <- graph_from_data_frame(.edges)
    .path <- shortest_paths(
        .graph,
        "1,1",
        paste0(nrow(.mat), ",", ncol(.mat)),
        weights = NULL,
        output = "both",
        mode = "out"
    )
    sum(edge_attr(.graph, "weight", .path$epath[[1]]))
}



find_path_cost("15/sample.txt") # Part 1: 40
find_path_cost("15/input.txt") # Part 1: 714


find_path_cost("15/sample.txt", TRUE) # Part 2: 315
find_path_cost("15/input.txt", TRUE) # Part 2: 2948