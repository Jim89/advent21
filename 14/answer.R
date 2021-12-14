read_sample <- function(.input) {
    .x <- .input |>
        readLines(1) |>
        strsplit("")
    .x[[1]]
}

read_insertions <- function(.input) {
    lines <- readLines(.input)
    lines <- lines[3:length(lines)]

    lines_split <- lines |>
        strsplit(" -> ") |>
        lapply(strsplit, "")

    pairs <- lines_split |>
        lapply(`[[`, 1) |>
        (\(x) do.call(rbind, x))()

    outputs <- sapply(lines_split, `[[`, 2)

    .m <- cbind(pairs, outputs)
    colnames(.m) <- c("first", "second", "output")
    .m
}

sample <- read_sample("14/sample.txt")
insertions <- read_insertions("14/sample.txt")

insert <- function(pair, rules) {
    lhs <- pair[1]
    rhs <- pair[2]
    output <- rules[rules[, "first"] == lhs & rules[, "second"] == rhs, "output"]
    .out <- c(lhs, output, rhs)
    names(.out) <- NULL
    .out
}

do_step <- function(.sample, .rules) {
    .iters <- seq_len(length(.sample) - 1)
    .out <- vector("list", max(.iters))
    for (i in .iters) {
        .pair <- .sample[i:(i+1)]
        .inserted <- insert(.pair, .rules)
        if (i > 1) {
            # For anything after the first iteration, the first character of the
            # trio is actually _in_ the last character of the previous set
            .inserted <- .inserted[-1]
        }
        .out[[i]] <- .inserted
    }
    unlist(.out)
}

run_steps <- function(.sample, .rules, .steps = 10) {
    for (i in seq_len(.steps)) {
        .new_sample <- do_step(.sample, .rules)
        .sample <- .new_sample
    }
    .sample
}

table(run_steps(sample, insertions, 10))

most_less_least <- function(.sample, .rules, .steps = 10) {
    .out <- run_steps(.sample, .rules, .steps)
    .counts <- table(.out)
    max(.counts) - min(.counts)
}

most_less_least(sample, insertions, 10)
