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
    .out <- list()
    for (.row in seq_len(nrow(.input))) {
        for (.col in seq_len(nrow(.input))) {

            .node <- paste0(.row, ",", .col)

            .position_neighbours <- neighbours(.row, .col, .input)
            .weights <- .input[.position_neighbours]

            .coords <- apply(.position_neighbours, 1, \(x) paste0(x[1], ",", x[2]))

            .edgelist <- cbind(.node, .coords, .weights)
            .out[[.node]] <- .edgelist

        }
    }
    .out <- do.call(rbind, .out)
    .out <- data.frame(.out)
    rownames(.out) <- NULL
    colnames(.out) <- c("from", "to", "weight")
    .out[["weight"]] <- as.integer(.out[["weight"]])
    .out
}

sample_nodes_and_edges <- read_matrix("15/sample.txt")
sample_edges <- build_edges(sample_nodes_and_edges)
sample_graph <- graph_from_data_frame(sample_edges)
sample_path <- shortest_paths(sample_graph, "1,1", "10,10", weights = NULL, output = "both")
sum(edge_attr(sample_graph, "weight", sample_path$epath[[1]]))

nodes_and_edges <- read_matrix("15/input.txt")
edges <- build_edges(nodes_and_edges)
graph <- graph_from_data_frame(edges)
path <- shortest_paths(
    graph,
    "1,1",
    paste0(dim(nodes_and_edges)[1], ",", dim(nodes_and_edges)[2]),
    weights = NULL,
    output = "both"
)
sum(edge_attr(graph, "weight", path$epath[[1]]))




