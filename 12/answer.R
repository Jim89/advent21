library(igraph)
read_edgelist <- function(.path) {
    readLines("12/smallest.txt") |>
        strsplit("-") |>
        lapply(rbind) |>
        (\(x) do.call(rbind, x))()
}

build_adjaceny_mat <- function(.el, directed = FALSE) {
    .el |>
        graph_from_edgelist(directed) |>
        as_adjacency_matrix() |>
        as.matrix()
}

is_lower <- function(.letters) all(strsplit(.letters, "")[[1]] %in% letters)
is_upper <- function(.letters) all(strsplit(.letters, "")[[1]] %in% LETTERS)

sel <- read_edgelist("12/smallest.txt")
am <- build_adjaceny_mat(sel)


test <- find_all_paths(g, "start", "end")

# 1. From source, visit neighbours
# 1. Track those visited
# 1. For each neighbour, make that source and repeat
    # 1. But don't _revist_ lowercase neighbours
# 1. If neighbour == end, stop, found a path

find_paths <- function(adj_mat, src = "start", tgt = "end") {
    visited <- NULL
}

neighbours <- function(.am, .n) {
    row <- .am[.n, ]
    names(which(row == 1))
}
have_visited <- function(node, .visited) node %in% .visited


# If we haven't visited it, do so
# Otherwise, is it lowercase? If it is, stop, if it isn't, can visit again
find_paths <- function(.graph, .src = "start", .tgt = "end", .path = vector(mode = "character")) {
    if (.tgt %in% .path) {
    } else {
        # Visit the source node ("start" to begin with)
        .path <- append(.path, .src)
    }

    if (.src == .tgt) return(.path)

    .all_paths <- list()

    # Find the neighbours
    .neighbours <- neighbours(.graph, .src)

    for (.neighbour in .neighbours) {
        if (!have_visited(.neighbour, .path) | is_upper(.neighbour) ) {
            .other_paths <- find_paths(.graph, .neighbour, .tgt, .path)
            .all_paths <- append(.all_paths, list(.other_paths))
        }
    }

    return(.all_paths)
}

out <- find_paths(am, "start", "end")
