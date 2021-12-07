initial_positions <- function(.input) scan(.input, what = integer(0), sep = ",")

fuel_spend <- function(positions) {
    med <- floor(median(positions))
    sum(abs(positions - med))
}

# Part 1
"7/sample.txt" |> initial_positions() |> fuel_spend()
"7/input.txt" |> initial_positions() |> fuel_spend()
