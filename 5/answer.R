library(purrr)
library(stringr)

parse_coords <- function(.line) {
    .line |>
        str_replace_all(" -> ", ",") |>
        str_split(",") |>
        unlist() |>
        as.integer() |>
        matrix(nrow = 1, ncol = 4, dimnames = list("", c("x1", "y1", "x2", "y2")))
}

get_coords <- function(.input = "5/sample.txt") {
    .input |>
        readLines() |>
        map(parse_coords) |>
        {\(x) do.call(rbind, x)}()
}

build_diagram_base <- function(.mat) {
    # Because coords are 0-indexed and R is 1-indexed: add one
    x <- max(c(.mat[, "x1", drop = TRUE], .mat[, "x2", drop = TRUE])) + 1
    y <- max(c(.mat[, "y1", drop = TRUE], .mat[, "y2", drop = TRUE])) + 1
    matrix(0, nrow = y, ncol = x)
}

is_horizontal <- function(.coords) {
    .coords[["y1"]] == .coords[["y2"]]
}

is_vertical <- function(.coords) {
    .coords[["x1"]] == .coords[["x2"]]
}

draw_horizontal <- function(.coords, base_diagram) {
    adjusted_coords <- .coords + 1 # Because coords are 0-indexed and R is 1-indexed
    y <- adjusted_coords[["y1"]]
    x1 <- adjusted_coords[["x1"]]
    x2 <- adjusted_coords[["x2"]]
    base_diagram[y, x1:x2] <- 1
    base_diagram
}

draw_vertical <- function(.coords, base_diagram) {
    adjust_coords <- .coords + 1 # Because coords are 0-indexed and R is 1-indexed
    y1 <- adjust_coords[["y1"]]
    y2 <- adjust_coords[["y2"]]
    x <- adjust_coords[["x1"]]
    base_diagram[y1:y2, x] <- 1
    base_diagram
}

draw_line <- function(.coords, base_diagram) {
    if (is_horizontal(.coords)) {
        draw_horizontal(.coords, base_diagram)
    } else if (is_vertical(.coords)) {
        draw_vertical(.coords, base_diagram)
    }
}

draw_lines <- function(.coords_mat, .base_diagram) {
    .coords_mat |>
        apply(1, draw_line, base_diagram = .base_diagram, simplify = FALSE) |>
        compact() |>
        reduce(`+`)
}

overlaps <- function(.mat) {
    sum(.mat >= 2)
}


coords_mat <- get_coords("5/input.txt")
diagram <- build_diagram_base(coords_mat)
all_lines <- draw_lines(coords_mat, diagram)
overlaps(all_lines)

