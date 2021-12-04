library(purrr)
library(readr)
library(stringr)

read_draws <- function(.input = "4/sample.txt") {
    .input |>
        read_lines(n_max = 1) |>
        strsplit(",") |>
        unlist() |>
        as.integer()
}


read_board <- function(.which = 1, .input = "4/sample.txt") {
    which <- .which -1
    skip <- (which * 6) + 2
    .input |>
        readr::read_lines(skip = skip, n_max = 5) |>
        purrr::map(stringr::str_replace_all, "\\s+", " ") |>
        purrr::map(stringr::str_split, pattern = " ", simplify = TRUE) |>
        purrr::map(as.integer) |>
        {\(x) do.call(rbind, x)}()
}

read_boards <- function(.input = "4/sample.txt") {
    nboards <- (length(readLines(.input)) - 1) / 6
    map(seq_len(nboards), read_board, .input = .input)
}

create_scoreboards <- function(boards) {
    map(seq_along(boards), ~matrix(data = FALSE, nrow = 5, ncol = 5))
}

match_draw <- function(draw, boards) {
    map(boards, \(.b) .b == draw)
}

update_scoreboards <- function(matches, scoreboards) {
    map2(matches, scoreboards, \(.m, .s) {
        .s[.m] <- TRUE
        .s
    })
}

bingo <- function(scoreboards) {
    .bingo <- function(board) {
        rows <- rowSums(board) == ncol(board)
        cols <- colSums(board) == nrow(board)
        any(rows, cols)
    }
    map_lgl(scoreboards, .bingo)
}


score_board <- function(board, matches, drawn) {
    sum(board[!matches]) * drawn
}

play_bingo <- function(draws, boards, scoreboards, first = TRUE) {
    for (i in seq_along(draws)) {
        draw <- draws[[i]]
        scoreboards <- draw |>
            match_draw(boards) |>
            update_scoreboards(scoreboards)

        is_bingo <- bingo(scoreboards)

        if ( any(is_bingo) ) {

            bingo_board <- boards[[which(is_bingo)]]
            matches <- scoreboards[[which(is_bingo)]]

            score <- score_board(bingo_board, matches, draw)

            return(
                list(
                    draw = draw,
                    score = score,
                    board = bingo_board,
                    mask = matches,
                    i = i
                )
            )
        }
    }
}


draws <- read_draws("4/sample.txt")
boards <- read_boards("4/sample.txt")
scoreboards <- create_scoreboards(boards)
out <- play_bingo(draws, boards, scoreboards)
out
