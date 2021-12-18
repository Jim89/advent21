sample_lines <- readLines("18/sample.txt")

# Function to "add" two lines together inside [ ]
join_lines <- function(line1, line2) paste0("[", line1, ",", line2, "]")

# Figure out _where_ explosions happen
explode_locations <- function(sf_number) {
    depth <- 4
    pair_pattern <- "\\[\\d+,\\d+\\]"
    pair_starts <- gregexpr(pair_pattern, sf_number)[[1]]

    opens <- strsplit(sf_number, "")[[1]] == "["
    closes <- strsplit(sf_number, "")[[1]] == "]"

    depths_along_number <- cumsum(opens - closes)

    pair_starts[depths_along_number[pair_starts] > depth]
}

explode <- function(sf_number, pair_location, debug = F) {
    if (debug) browser()

    left_of_pair <- substr(sf_number, 1, pair_location - 1)
    right_of_pair <- substr(sf_number, pair_location + 5, nchar(sf_number))

    pair <- substr(sf_number, pair_location, pair_location + 4)
    pair_numbers <- stringr::str_extract_all(pair, "\\d+")[[1]]
    pair_left <- as.numeric(pair_numbers[[1]])
    pair_right <- as.numeric(pair_numbers[[2]])


    number_to_left <- number_to_right <- 0
    left_updated <- left_of_pair
    if (grepl("\\d+", left_of_pair)) {
        left_split <- strsplit(left_of_pair, "")[[1]]

        left_split_rev <- rev(left_split)
        left_split_rev_numbers <- left_split_rev[left_split_rev %in% 0:9]

        number_to_left <- as.numeric(left_split_rev_numbers[[1]])
        replacement_left <- number_to_left + pair_left

        left_split[max(which(left_split %in% 0:9))] <- replacement_left
        left_updated <- paste0(left_split, collapse = "")

    }

    right_updated <- right_of_pair
    if (grepl("\\d+", right_of_pair)) {
        right_split <- strsplit(right_of_pair, "")[[1]]

        right_split_numbers <- right_split[right_split %in% 0:9]

        number_to_right <- as.numeric(right_split_numbers[[1]])
        replacement_right <- number_to_right + pair_right

        right_split[min(which(right_split %in% 0:9))] <- replacement_right
        right_updated <- paste0(right_split, collapse = "")
    }

    new <- paste0(left_updated, "0", right_updated)

}



reduce_once <- function(sf_number) {
    exp_loc <- explode_locations(sf_number)
    split_loc <- split_locations(sf_number)

}

