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
    if (stringr::str_detect(new, "\\[\\d{1,2}\\]")) browser()
    reduce_once(new)
}



split_locations <- function(sf_number) {
    # Splits are anywhere number is >= 10, i.e. 2 digits (not more, because
    # we'll split them!)
    stringr::str_locate_all(sf_number, "\\d{2}")[[1]][, "start"]
}

split_sf <- function(sf_number, split_location) {
    left_of_split_point <- substr(sf_number, 1, split_location - 1)
    right_of_split_point <- substr(sf_number, split_location + 2, nchar(sf_number))
    number_to_split <- as.numeric(substr(sf_number, split_location, split_location + 1))

    lhs <- floor(number_to_split / 2)
    rhs <- ceiling(number_to_split / 2)

    new_pair_to_insert <- join_lines(lhs, rhs)

    new <- paste0(left_of_split_point, join_lines(lhs, rhs), right_of_split_point)
    if (stringr::str_detect(new, "\\[\\d{1,2}\\]")) browser()
    reduce_once(new)
}



reduce_once <- function(sf_number) {
    exp_loc <- explode_locations(sf_number)
    split_loc <- split_locations(sf_number)

    if (length(exp_loc) > 0) {
        explode_sf(sf_number, exp_loc[[1]])
    } else if (length(split_loc) > 0) {
        split_sf(sf_number, split_loc[[1]])
    } else {
        sf_number
    }
}

reduce_sf_pair <- function(first, second) {
    joined <- join_lines(first, second)
    reduce_once(joined)
}


# Tests -------------------------------------------------------------------
explode_sf("[[[[[9,8],1],2],3],4]", 5) == "[[[[0,9],2],3],4]"
explode_sf("[7,[6,[5,[4,[3,2]]]]]", 13) == "[7,[6,[5,[7,0]]]]"
explode_sf("[[6,[5,[4,[3,2]]]],1]", 11) == "[[6,[5,[7,0]]],3]"
explode_sf("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", 11) == "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" # Recurses, so don't expect to pass
explode_sf("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", 25) == "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"

# Sample pair
reduce_sf_pair("[[[[4,3],4],4],[7,[[8,4],9]]]", "[1,1]") == "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"

purrr::reduce(c("[1,1]" ,"[2,2]" ,"[3,3]" ,"[4,4]"), reduce_sf_pair) == "[[[[1,1],[2,2]],[3,3]],[4,4]]"
purrr::reduce(c("[1,1]" ,"[2,2]" ,"[3,3]" ,"[4,4]", "[5,5]"), reduce_sf_pair) == "[[[[3,0],[5,3]],[4,4]],[5,5]]"
purrr::reduce(c("[1,1]" ,"[2,2]" ,"[3,3]" ,"[4,4]", "[5,5]", "[6,6]"), reduce_sf_pair) == "[[[[5,0],[7,4]],[5,5]],[6,6]]"

test_lines <- c("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
,"[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
,"[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
,"[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
,"[7,[5,[[3,8],[1,4]]]]"
,"[[2,[2,2]],[8,[8,1]]]"
,"[2,9]"
,"[1,[[[9,3],9],[[9,0],[0,7]]]]"
,"[[[5,[7,4]],7],1]"
,"[[[[4,2],2],6],[8,7]]"
)

purrr::reduce(test_lines, reduce_sf_pair) == "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"


