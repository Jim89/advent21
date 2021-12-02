library(dplyr)
library(tidyverse)

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



aim <- 0
hpos <- 0
vpos <- 0

"2/input.txt" %>%
    #test %>%
    read_lines() %>%
    str_split(" ") %>%
    walk(function(line){
        dir <- line[[1]]
        amt <- as.integer(line[[2]])

        if (dir != "forward") {
            if (dir == "down") {
                aim_inc <- amt
            } else {
                aim_inc <- -1L * amt
            }
            aim <<- aim + aim_inc
        }

        if (dir == "forward") {
            hpos <<- hpos + amt
            vpos_change <- aim * amt
            vpos <<- vpos + vpos_change
        }
    })

hpos * vpos
