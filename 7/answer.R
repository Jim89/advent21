initial_positions <- function(.input) scan(.input, what = integer(0), sep = ",")

fuel_spend <- function(positions) {
    med <- floor(median(positions))
    sum(abs(positions - med))
}

# Part 1
"7/sample.txt" |> initial_positions() |> fuel_spend()
"7/input.txt" |> initial_positions() |> fuel_spend()


# Part 2
fuel_spend2 <- function(positions) {
    # Helper to figure out cost moving to from/to
    move_cost <- \(from, to) if (from == to ) 0 else sum(abs((to:from) - to))

    # Can only be at points within the range of the data
    possible <- seq(min(positions), max(positions))

    # For every possible location, calculate distance to get there for all
    min(colSums(sapply(possible, \(poss) sapply(positions, move_cost, to = poss))))
}

fuel_spend2(initial_positions("7/sample.txt")) # 168
fuel_spend2(initial_positions("7/input.txt")) # 95167302


