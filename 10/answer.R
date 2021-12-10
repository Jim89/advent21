corrupt_line_char <- function(line) {

    openers <- c("(", "[", "{", "<")
    closers <- c(")", "]", "}", ">")

    is_opener <- function(.char) .char %in% openers
    is_closer <- function(.char) .char %in% closers

    closes_safely <- function(.o, .c) {
        open_ind <- which(.o == openers)
        close_ind <- which(.c == closers)
        open_ind == close_ind
    }

    chars <- line |> strsplit("") |> unlist()
    found_openers <- vector("character")
    for (char in chars) {
        if (is_opener(char)) {
            found_openers <- append(found_openers, char)
        }
        if (is_closer(char)) {
            current_opener <- tail(found_openers, 1)
            closes <- closes_safely(current_opener, char)
            if (!closes) {
                return(char)
            } else {
                found_openers <- found_openers[-length(found_openers)]
            }
        }
    }
    NA_character_
}

score_lines <- function(.lines) {
    scores <- c(")" = 3, "]" = 57, "}" = 1197, ">" = 25137)

    problems <- sapply(.lines, corrupt_line_char)
    problems <- problems[!is.na(problems)]
    sum(scores[problems])

}

sample_lines <- readLines("10/sample.txt")
lines <- readLines("10/input.txt")
score_lines(lines)




# Part 2 ------------------------------------------------------------------
incomplete_lines <- function(.lines) {
    problems <- sapply(.lines, corrupt_line_char)
    incomplete <- names(problems[is.na(problems)])

    incomplete
}

hanging_openers <- function(.line) {
    openers <- c("(", "[", "{", "<")
    closers <- c(")", "]", "}", ">")

    is_opener <- function(.char) .char %in% openers
    is_closer <- function(.char) .char %in% closers

    closes_safely <- function(.o, .c) {
        open_ind <- which(.o == openers)
        close_ind <- which(.c == closers)
        open_ind == close_ind
    }

    chars <- .line |> strsplit("") |> unlist()
    found_openers <- vector("character")
    for (char in chars) {
        if (is_opener(char)) {
            found_openers <- append(found_openers, char)
        }
        if (is_closer(char)) {
            current_opener <- tail(found_openers, 1)
            closes <- closes_safely(current_opener, char)
            if (!closes) {
                return(char)
            } else {
                found_openers <- found_openers[-length(found_openers)]
            }
        }
    }
    found_openers
}

find_closers <- function(.ho) {
    openers <- c("(", "[", "{", "<")
    closers <- c(")", "]", "}", ">")
    closers[sapply(rev(.ho), \(x) which(x == openers))]
}

score_closers <- function(.closers) {
    scores <- c(")" = 1, "]" = 2, "}" = 3, ">" = 4)
    total <- 0
    for (close_char in .closers) {
        total <- total * 5
        total <- total + scores[close_char]
    }
    total
}


lines |>
    incomplete_lines() |>
    sapply(hanging_openers) |>
    sapply(find_closers) |>
    sapply(score_closers) |>
    median()


sample_lines |>
    incomplete_lines() |>
    sapply(hanging_openers) |>
    sapply(find_closers) |>
    sapply(score_closers) |>
    median()

