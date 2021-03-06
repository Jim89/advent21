get_starts <- function(inpath) {
    readLines(inpath) |>
        sapply(\(x) substr(x, nchar(x), nchar(x))) |>
        as.numeric() |>
        setNames(c("player1", "player2"))
}


make_rolls <- function(n = 10000) {
    det_dice <- rep(1:100, 500)
    out <- vector("numeric", n)
    for (i in seq_len(n)) {
        roll <- sum(det_dice[1:3])
        out[i] <- roll
        det_dice <- det_dice[-c(1:3)]
    }
    out
}

play <- function(starts, win_at = 1000) {
    player1_pos <- starts["player1"]
    player2_pos <- starts["player2"]

    p1_score <- p2_score <- 0

    winner <- p1_score >= win_at || p2_score >= win_at

    rolls <- make_rolls(5000)
    nrolls <- 0

    i <- 1
    while (!winner) {
        roll <- rolls[[i]]
        # Only move by the "single digits" value, e.g. 6 = move 6, 15 = move 5
        # (loop once, and move 5), 123 = move 3 (loop 12 times, move 3)
        move <- as.numeric(substr(roll, nchar(roll), nchar(roll)))

        # On "odd" turns, it's player1's go
        if (i %% 2 != 0) {
            new_pos <- player1_pos + move
            player1_pos <- if (new_pos > 10) as.numeric(substr(new_pos, nchar(new_pos), nchar(new_pos))) else new_pos
            p1_score <- p1_score + player1_pos
        } else {
            new_pos <- player2_pos + move
            player2_pos <- if (new_pos > 10) as.numeric(substr(new_pos, nchar(new_pos), nchar(new_pos))) else new_pos
            p2_score <- p2_score + player2_pos
        }
        i <- i + 1
        nrolls <- nrolls + 3
        winner <- p1_score >= win_at || p2_score >= win_at
        if (winner) break
    }
    return(list(iters = i, p1_score = p1_score, p2_score = p2_score, n_rolls = nrolls))
}


# Part 1
sample_result <- "21/sample.txt" |> get_starts() |> play()
min(sample_result$p1_score, sample_result$p2_score) * sample_result$n_rolls # 739785

result <- "21/input.txt" |> get_starts() |> play()
min(result$p1_score, result$p2_score) * result$n_rolls # 752745


# Part 2
rolls <- expand.grid(d1 = 1:3, d2 = 1:3, d3 = 1:3)
potential_values <- c(0, 0, table(rowSums(rolls)))
cache <- list()

dirac_dice <- function(p1, p2, s1 = 0, s2 = 0, turn = 1, cnt = 1) {
    cache_name <- paste(p1, p2, s1, s2, turn)
    if (cache_name %in% names(cache)) {
        cache[[cache_name]] * cnt
    } else {
        rowSums(
            sapply(9:3, \(x) {
                if (turn == 1) {
                    p1 <- (p1 + x) %% 10; if (p1 == 0) p1 <- 10; s1 <- s1 + p1
                } else {
                    p2 <- (p2 + x) %% 10; if (p2 == 0) p2 <- 10; s2 <- s2 + p2
                }

                if (s1 >= 21 || s2 >= 21) {
                    cnt * potential_values[[x]] * as.numeric(turn == c(1, 2))
                } else {
                    value <- dirac_dice(p1, p2, s1, s2, 3 - turn, cnt * potential_values[[x]])
                    cache_name <- paste(p1, p2, s1, s2, 3 - turn)
                    if (!cache_name %in% names(cache)) {
                        cache[[cache_name]] <<- value / cnt / potential_values[[x]]
                    }
                    value
                }
            })
        )
    }
}

sample_starts <- get_starts("21/sample.txt")

sample_out <- dirac_dice(sample_starts["player1"], sample_starts["player2"])

cache <- list()
starts <- get_starts("21/input.txt")
my_out <- dirac_dice(starts[["player1"]], starts[["player2"]]) # 309196008717909
my_out