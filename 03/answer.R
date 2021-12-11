library(purrr)
library(stringr)
library(readr)

x <- "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"

reader <- function(.in) {
    .in %>%
        readr::read_lines() %>%
        map(stringr::str_split, pattern = "", simplify = TRUE) %>%
        map(as.integer) %>%
        do.call(rbind, .)
}

test_mat <- reader(x)
mat <- reader("3/input.txt")

most_common <- function(.x) {
    counts <- rev(table(.x))
    names(counts)[which.max(counts)]
}

least_common <- function(.x) {
    counts <- table(.x)
    names(counts)[which.min(counts)]
}

power_consumption <- function(.mat) {
    f <- function(x, .c) {
        x %>%
            apply(2,.c) %>%
            paste0(collapse = "") %>%
            strtoi(base = 2)
    }
    gamma <- f(.mat, most_common)
    epsilon <- f(.mat, least_common)
    gamma * epsilon
}

power_consumption(test_mat)
power_consumption(mat)


# Part 2 ------------------------------------------------------------------
find_ <- function(.mat, start = 1, comparison) {
    .c <- apply(.mat[, start, drop = FALSE], 2, comparison)
    new_mat <- .mat[.mat[, start] == .c, ,drop = FALSE]
    if (nrow(new_mat) == 1) {
        strtoi(paste0(new_mat[1, ], collapse = ""), base = 2)
    } else {
        find_(new_mat, start + 1, comparison)
    }
}

find_oxygen <- function(.mat, start = 1) {
    find_(.mat, start, most_common)
}

find_co2 <- function(.mat, start = 1) {
    find_(.mat, start, least_common)
}

find_oxygen(test_mat) * find_co2(test_mat)
find_oxygen(mat) * find_co2(mat)

