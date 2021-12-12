library(igraph)
read_edgelist <- function(.path) {
    readLines(.path) |>
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

is_upper <- function(.letters) all(strsplit(.letters, "")[[1]] %in% LETTERS)
is_lower <- function(.letters) !is_upper(.letters)


neighbours <- function(.am, .n) {
    row <- .am[.n, ]
    names(which(row == 1))
}
have_visited <- function(node, .visited, part2 = FALSE) {
    if (node == "start" | node == "end") {
        return(node %in% .visited)
    }
    if (part2) {
        if (is_upper(node)) {
            FALSE
        } else {
            sum(node == .visited) >= 2
        }
    } else {
        node %in% .visited
    }
}

find_paths <- function(.graph, .src = "start", .tgt = "end", .path = vector(mode = "character"), part2 = FALSE) {
    .path <- append(.path, .src)

    if (.src == .tgt) return(.path)

    .all_paths <- list()

    # Find the neighbours
    .neighbours <- neighbours(.graph, .src)

    for (.neighbour in .neighbours) {
        if (part2) {
            .visits <- table(.path[sapply(.path, is_lower)])
            .visited_twice <- .visits > 1
            if (any(.visited_twice)) {
                if (!have_visited(.neighbour, .path) | is_upper(.neighbour) ) {
                    .other_paths <- find_paths(.graph, .neighbour, .tgt, .path)
                    .all_paths <- append(.all_paths, .other_paths)
                }
            } else {
                if (!have_visited(.neighbour, .path, part2 = TRUE) | is_upper(.neighbour) ) {
                    .other_paths <- find_paths(.graph, .neighbour, .tgt, .path, part2 = TRUE)
                    .all_paths <- append(.all_paths, .other_paths)
                }

            }
        } else {
            if (!have_visited(.neighbour, .path) | is_upper(.neighbour) ) {
                .other_paths <- find_paths(.graph, .neighbour, .tgt, .path)
                .all_paths <- append(.all_paths, .other_paths)
            }
        }
    }
    # unlist() all paths?
    return(.all_paths)
}



n_paths <- function(.input, part2 = FALSE) {
    el <- read_edgelist(.input)
    am <- build_adjaceny_mat(el)
    out <- find_paths(am, "start", "end", part2 = part2)
    paths <- purrr::map2(
        which(out == "start"),
        which(out == "end"),
        \(x, y) unlist(out[x:y])
    )
    length(paths)
}

n_paths("12/smallest.txt") # Part 1: 10
n_paths("12/larger.txt") # Part 1: 19
n_paths("12/largest.txt") # Part 1: 226
n_paths("12/input.txt") # Part 1: 3450


n_paths("12/smallest.txt", TRUE) # Part 2: 36
n_paths("12/larger.txt", TRUE) # Part 2: 103
n_paths("12/largest.txt", TRUE) # Part 2: 3509
n_paths("12/input.txt", TRUE) # Part 2: 96528


