library(dplyr)

test <- "forward 5
down 5
forward 8
up 3
down 8
forward 2"

input <- readr::read_delim("2/input.txt", col_names = FALSE, col_types = "ci")

one <- input %>%
    mutate(
        axis = if_else(X1 == "forward", "horizontal", "vertical"),
        amnt = if_else(X1 == "up", -1L * X2, X2)
    ) %>%
    group_by(axis) %>%
    summarise(tot = sum(amnt)) %>%
    pull(tot) %>%
    prod()




