starting_fish <- function(.input) scan(.input, what = integer(0), sep = ",")

sim_day <- function(.fish) {
    n_new <- sum(.fish == 0)
    out <- rep(NA_integer_, length(.fish) + n_new)
    f <- .fish - 1
    f[which(f == -1)] <- 6
    out[1:length(f)] <- f
    out[is.na(out)] <- 8
    out
}

sim_days <- function(.fish, days = 10) {
    out <- list(rep(NA, days))
    f <- .fish
    for (i in seq_len(days)) {
        out[[i]] <- sim_day(f)
        f <- out[[i]]
    }
    out
}

.days <- 80
fish <- starting_fish("6/input.txt")
sim_result <- sim_days(fish, .days)
length(sim_result[[.days]])

results <- data.frame(x = 1:length(sim_result), y = sapply(sim_result, length))
results$diff <- c(0, diff(results$y))
plot(results$x, results$y)
plot(results$x, results$diff)
