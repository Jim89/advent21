starting_fish <- function(.input) scan(.input, what = integer(0), sep = ",")

sim_days <- function(.fish, days = 10) {
    all_freqs <- as.double(sapply(0:8, \(x) length(which(.fish == x))))
    for (i in seq_len(days)) {
        n_new <- all_freqs[[1]]
        all_freqs[1:8] <- all_freqs[2:9]
        all_freqs[[7]] <- all_freqs[[7]] + n_new
        all_freqs[[9]] <- n_new
    }
    all_freqs
}


fish <- starting_fish("6/input.txt")
.days <- 80
sim_result <- sim_days(fish, .days)
options(scipen = 999)
sum(sim_result, na.rm = TRUE)


