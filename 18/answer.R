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

explode_sf <- function(sf_number, pair_location, debug = F) {
    if (debug) browser()

    left_of_pair <- substr(sf_number, 1, pair_location - 1)

    pair_end <- min(stringr::str_locate(substr(sf_number, pair_location, nchar(sf_number)), "\\]"))
    pair <- substr(sf_number, pair_location, pair_location + pair_end-1)
    pair_numbers <- stringr::str_extract_all(pair, "\\d+")[[1]]
    pair_left <- as.numeric(pair_numbers[[1]])
    pair_right <- as.numeric(pair_numbers[[2]])

    right_of_pair_start <- pair_location + pair_end
    right_of_pair <- substr(sf_number, right_of_pair_start, nchar(sf_number))

    number_to_left <- number_to_right <- 0
    left_updated <- left_of_pair
    if (grepl("\\d+", left_of_pair)) {
        # Find the last digit in the LHS of the pair
        number_to_left <- as.numeric(stringr::str_extract(left_of_pair, "(\\d+)(?!.*\\d)"))

        replacement_left <- number_to_left + pair_left

        # Perform the replacement
        left_updated <- stringr::str_replace(left_of_pair, "(\\d+)(?!.*\\d)", as.character(replacement_left))
    }

    right_updated <- right_of_pair
    if (grepl("\\d+", right_of_pair)) {
        # Find the first digit
        number_to_right <- as.numeric(stringr::str_extract(right_of_pair, "\\d{1,2}"))

        replacement_right <- number_to_right + pair_right

        # Perform the replacement
        right_updated <- stringr::str_replace(right_of_pair, "\\d{1,2}", as.character(replacement_right))
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

magnitude <- function(number) {
    pair_pattern <- "\\[\\d+,\\d+\\]"

    # Find all the "raw" pairs, and turn them into numbers
    pairs <- stringr::str_extract_all(number, pair_pattern)[[1]]
    numbers <- sapply(pairs, \(pair) {
        pair_numbers <- as.numeric(stringr::str_extract_all(pair, "\\d+")[[1]])
        (3 * pair_numbers[[1]]) + (2 * pair_numbers[[2]])

    })

    # cbind(numbers_split, c(numbers, "")) to understand
    numbers_split <- strsplit(number, pair_pattern)[[1]]
    mag <- paste0(numbers_split, c(numbers, ""), collapse = "")

    # If there are still list elements left ([ in the string), recurse
    if (stringr::str_detect(mag, "\\[")) mag <- magnitude(mag)

    # Return the number
    mag
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


magnitude("[[1,2],[[3,4],5]]") == "143"
magnitude("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]") == "1384"
magnitude("[[[[1,1],[2,2]],[3,3]],[4,4]]") == "445"
magnitude("[[[[3,0],[5,3]],[4,4]],[5,5]]") == "791"
magnitude("[[[[5,0],[7,4]],[5,5]],[6,6]]") == "1137"
magnitude("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]") == "3488"


test_lines_2 <- c(
     "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
    ,"[[[5,[2,8]],4],[5,[[9,9],0]]]"
    ,"[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
    ,"[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
    ,"[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
    ,"[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
    ,"[[[[5,4],[7,7]],8],[[8,3],8]]"
    ,"[[9,3],[[9,9],[6,[4,9]]]]"
    ,"[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
    ,"[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
)

out <- purrr::reduce(test_lines_2, reduce_sf_pair)
out == "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]"
magnitude(out) == 4140


# Part 1
my_input <- readLines("18/input.txt")
out <- purrr::reduce(my_input, reduce_sf_pair)
magnitude(out)


# Part 2
combos <- expand.grid(x = test_lines_2, y = test_lines_2)
combos <- combos[!combos$x == combos$y, ]
combos$reduced <- apply(combos, 1, \(row) reduce_sf_pair(row[[1]], row[[2]]))
combos$mag <- apply(combos, 1, \(row) magnitude(row[[3]]))
max(as.numeric(combos$mag))

my_combos <- expand.grid(x = my_input, y = my_input)
my_combos <- my_combos[!my_combos$x == my_combos$y, ]
my_combos$reduced <- apply(my_combos, 1, \(row) reduce_sf_pair(row[[1]], row[[2]]))
my_combos$mag <- apply(my_combos, 1, \(row) magnitude(row[[3]]))
max(as.numeric(my_combos$mag))
