library(dplyr)

input <- readr::read_csv("1/input.txt", col_names = FALSE)

# Part 1
one <- input %>%
    mutate(
        prev = lag(X1, 1),
        increased = X1 > prev
    ) %>%
    count(increased)

# Part 2
two <- input %>%
    rsample::rolling_origin(initial = 3, cumulative = FALSE) %>%
    mutate(
        tr = purrr::map(splits, rsample::training),
        sum = purrr::map_dbl(tr, sum),
        prev = lag(sum, 1),
        increased = sum > prev
    ) %>%
    count(increased)

test <- "
199
200
208
210
200
207
240
269
260
263
"
