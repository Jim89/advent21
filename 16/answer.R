mapping <- c(
    "0" = "0000",
    "1" = "0001",
    "2" = "0010",
    "3" = "0011",
    "4" = "0100",
    "5" = "0101",
    "6" = "0110",
    "7" = "0111",
    "8" = "1000",
    "9" = "1001",
    "A" = "1010",
    "B" = "1011",
    "C" = "1100",
    "D" = "1101",
    "E" = "1110",
    "F" = "1111"
)


build_binary <- function(.hexa, .mapping, .split = TRUE) {
    .hex_chrs <- strsplit(.hexa, "")[[1]]
    .out <- paste0(.mapping[.hex_chrs], collapse = "")
    if (.split) .out <- strsplit(.out, "")[[1]]
    .out
}

get_version <- function(.binary, .mapping) {
    get_value(.binary[1:3], .mapping)
}

get_type <- function(.binary, .mapping) {
    get_value(.binary[4:6], .mapping)
}

get_type_id <- function(.binary, .mapping) .binary[[7]]

get_type_0_length <- function(.binary) strtoi(paste0(.binary[8:22], collapse = ""), 2)

get_value <- function(.binary, .mapping) {
    .x <- paste0(.binary, collapse = "")
    .padded <- stringr::str_pad(.x, 4, "left", "0")
    names(which(.mapping == .padded))
}


consume <- function(.binary, .out = vector("character"), .mapping, .versions) {

    .next <- .binary[1:5]

    if ( .next[[1]] == "1") {
        .value <- .next[2:5]
        .out <- append(.out, .value)

        .new_b <- .binary[6:length(.binary)]
        .out <- consume(.new_b, .out, .mapping = .mapping, .versions = .versions)

    } else {
        .last_value <- .next[2:5]
        .out <- append(.out, .last_value)

        .remaining_b <- .binary[6:length(.binary)]

        if (all(.remaining_b == 0)) {
            return(.versions)
        } else {
            .out <- parse_binary(.remaining_b, .mapping, .versions)
        }

    }
    return(.out)
}


parse_binary <- function(.binary, .mapping, .versions = vector("character")) {

    .v <- get_version(.binary, .mapping)
    .versions <- append(.versions, .v)


    .t <- get_type(.binary, .mapping)

    if (.t == "4") {
        .new_b <- .binary[7:length(.binary)]
        .versions <- consume(.new_b, .mapping = .mapping, .versions = .versions)
        return(.versions)
    } else {
        .type_id <- get_type_id(.binary)
        if (.type_id == "0") {
            .type_0_length <- get_type_0_length(.binary)
            .new_b <- .binary[23:length(.binary)]
            .versions <- parse_binary(.new_b, .mapping, .versions)
        }
    }
    return(.versions)
}


"D2FE28" |>
    build_binary(mapping) |>
    parse_binary(mapping)

"38006F45291200" |>
    build_binary(mapping) |>
    parse_binary(mapping)



