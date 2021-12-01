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

