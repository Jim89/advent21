library(dplyr)

input <- readr::read_csv("1/input.txt", col_names = FALSE)

f <- function(.d) {
    .d %>%
        mutate(
            prev = lag(X1, 1),
            increased = X1 > prev
        ) %>%
        count(increased)
}

# Part 1
one <- f(input)

# Part 2
two <- input %>%
    pull(X1) %>%
    RcppRoll::roll_sum(n = 3) %>%
    tibble(X1 = .) %>%
    f()

# Code golf version ------------------------------------------------------------
input <- as.integer(readr::read_lines("1/input.txt"))
gt_lag_count <- function(x) sum(x > dplyr::lag(x, 1), na.rm = TRUE)

# Part 1
gt_lag_count(input)

# Part 2
gt_lag_count(RcppRoll::roll_sum(input, n = 3))
