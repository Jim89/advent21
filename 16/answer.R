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
    .v <- paste0(.binary[1:3], collapse = "")
    .padded <- stringr::str_pad(.v, 4, "left", "0")
    names(which(.mapping == .padded))
}

get_type <- function(.binary, .mapping) {
    .t <- paste0(.binary[4:6], collapse = "")
    .padded <- stringr::str_pad(.t, 4, "left", "0")
    names(which(.mapping == .padded))
}

