x <- "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"

library(tidyverse)
test_mat <- x %>%
    read_lines() %>%
    map(str_split, pattern = "", simplify = T) %>%
    map(as.integer) %>%
    do.call(rbind, .)

sums <- apply(test_mat, 2, sum)
gamma <- strtoi(paste0(as.integer(sums >= nrow(test_mat) / 2), collapse = ""), base = 2)
epsilon <- strtoi(paste0(as.integer(sums <= nrow(test_mat) / 2), collapse = ""), base = 2)
gamma * epsilon

mat <- "3/input.txt" %>% 
    read_lines() %>% 
    map(str_split, "", simplify = T)
