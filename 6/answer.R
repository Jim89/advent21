starting_fish <- function(.input) scan(.input, what = integer(0), sep = ",")

sim_days <- function(.fish, days = 10) {
    # Count how many fish of each age there are, from 0 -> 8
    fish_ages <- as.double(sapply(0:8, \(x) length(which(.fish == x))))
    for (i in seq_len(days)) {
        n_new_fish <- fish_ages[[1]]
        fish_ages[1:8] <- fish_ages[2:9] # Age all fish by 1
        fish_ages[[7]] <- fish_ages[[7]] + n_new_fish # Add the new fish at age 6
        fish_ages[[9]] <- n_new_fish # "Cycle" the fish that just gave birth
    }
    fish_ages
}


fish <- starting_fish("6/input.txt")

# Part 1
fish |>
    sim_days(80) |>
    sum()

# Part 2
options(scipen = 999)
fish |>
    sim_days(256) |>
    sum()
