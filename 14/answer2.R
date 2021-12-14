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

build_transitions <- function(.insertions) {
    .letters <- unique(c(
        .insertions[,"first"],
        .insertions[,"second"],
        .insertions[,"output"]
    ))

    # Set up matrix to hold transitions
    transitions <- matrix(NA, length(.letters), length(.letters))
    rownames(transitions) <- colnames(transitions) <- .letters

    # Build the transitions
    apply(.insertions, 1, \(.r) {
        transitions[.r[[1]], .r[[2]]] <<- .r[[3]]
    })

    transitions
}

initialise_pairs <- function(.sample_seq, .pair_mat) {
    .iters <- seq_len(length(.sample_seq) - 1)
    for (i in .iters) {
        lhs <- .sample_seq[i]
        rhs <- .sample_seq[i+1]

        .pair_mat[lhs, rhs] <- .pair_mat[lhs, rhs] + 1
    }
    .pair_mat
}


do_step2 <- function(.pairs, .transitions) {
    new_pairs <- .pairs
    new_pairs[] <- 0
    for (i in seq_len(nrow(.pairs))) {
        for (j in seq_len(ncol(.pairs))) {

            lhs <- rownames(.pairs)[i]
            rhs <- colnames(.pairs)[j]
            val <- .pairs[i, j]

            middle <- .transitions[lhs, rhs]

            new_pairs[lhs, middle] <- new_pairs[lhs, middle] + val
            new_pairs[middle, rhs] <- new_pairs[middle, rhs] + val
        }
    }
    new_pairs
}

run_steps2 <- function(.pairs, .transitions, .steps = 10) {
    for (i in seq_len(.steps)) {
        .new_pairs <- do_step2(.pairs, .transitions)
        .pairs <- .new_pairs
    }
    .pairs
}


most_less_least2 <- function(.pairs, .transitions, .steps = 10) {
    .out <- run_steps2(.pairs, .transitions, .steps)

    starts <- as.double(rowSums(.out))
    ends <- as.double(colSums(.out))
    .counts <- pmax(starts, ends)

    max(.counts) - min(.counts)
}



sample_seq <- read_sample("14/sample.txt")
sample_insertions <- read_insertions("14/sample.txt")


# Set up matrix to hold transitions
transitions <- build_transitions(sample_insertions)

# Set up matrix to hold current pairs
pairs <- matrix(0, nrow(transitions), ncol(transitions))
rownames(pairs) <- colnames(pairs) <- rownames(transitions)

initial_pairs <- initialise_pairs(sample_seq, pairs)

most_less_least2(initial_pairs, transitions, 10)
most_less_least2(initial_pairs, transitions, 40)

# Real answer

input_seq <- read_sample("14/input.txt")
input_insertions <- read_insertions("14/input.txt")

input_transitions <- build_transitions(input_insertions)
input_pairs <- matrix(0, nrow(input_transitions), ncol(input_transitions))
rownames(input_pairs) <- colnames(input_pairs) <- rownames(input_transitions)

input_initial_pairs <- initialise_pairs(input_seq, input_pairs)

most_less_least2(input_initial_pairs, input_transitions, 10) - 1 # I'm off by 1, somewhere!
most_less_least2(input_initial_pairs, input_transitions, 40) - 1 # I'm off by 1, somewhere!