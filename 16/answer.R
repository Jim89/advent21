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

get_type_1_length <- function(.binary) strtoi(paste0(.binary[8:18], collapse = ""), 2)

get_value <- function(.binary, .mapping) {
    .x <- paste0(.binary, collapse = "")
    .padded <- stringr::str_pad(.x, 4, "left", "0")
    names(which(.mapping == .padded))
}

consume <- function(.binary, .out = list(message = character(0), remainder = character(0))) {
    .next <- .binary[1:5]

    if ( .next[[1]] == "1") {
        .value <- .next[2:5]
        .out[["message"]] <- append(.out[["message"]], .value)

        .new_b <- .binary[6:length(.binary)]
        .out <- consume(.new_b, .out)
    } else {
        .last_value <- .next[2:5]
        .out[["message"]] <- append(.out[["message"]], .last_value)
        .rem <- .binary[6:length(.binary)]
        if ( any(is.na(.rem)) ) {
            .out[["remainder"]] <- append(.out[["remainder"]], "0")
        } else {
            .out[["remainder"]] <- append(.out[["remainder"]], .rem)
        }

    }
    return(.out)
}



parse_packet <- function(.packet, .mapping, .pos = 1, .max_pos = 1) {
    .t <- get_type(.packet, .mapping)
    .v <- get_version(.packet, .mapping)

    if (.t == "4") {
        .packet_contents <- consume(.packet[7:length(.packet)])
        .message = as.numeric(.packet_contents$message)
        .remainder <- .packet_contents$remainder
        # strtoi was causing integer overflow, hopefully this fixes the issue
        .message_decoded <- sum((.message == 1)*2^(rev(seq_along(.message))-1))
        .sub_packets <- NULL
        .type_id <- NULL
    } else {
        .message <- NULL
        .message_decoded <- NULL

        .type_id <- get_type_id(.packet)

        if (.type_id == "0") {

            .type_0_length <- get_type_0_length(.packet)

            .type_0_packets <- .packet[23:(22 + .type_0_length)]

            .sub_packets <- parse_packet(.type_0_packets, .mapping, .max_pos = Inf)

            .remainder <- .packet[(23 + .type_0_length):length(.packet)]

            # Needed in case the type 0 bits flow to the end of the packet,
            # meaning the above would "loop" around and actually we should stop,
            # not carry on
            if (anyNA(.remainder)) .remainder <- "0"

        } else {
            .type_1_length <- get_type_1_length(.packet)
            .type_1_packets <- .packet[19:length(.packet)]
            .sub_packets <- parse_packet(.type_1_packets, .mapping, .pos = 1, .max_pos = .type_1_length)

            # Need to figure out remainder
            .remainder <- .sub_packets[[length(.sub_packets)]]$remainder

        }
    }
    #if (.t == "1") browser()
    .value <- switch(
        .t,
        "4" = .message_decoded,
        "0" = sum( sapply(.sub_packets, \(x) x[["value"]]) ),
        "1" = prod( sapply(.sub_packets, \(x) x[["value"]]) ),
        "2" = min( sapply(.sub_packets, \(x) x[["value"]]) ),
        "3" = max( sapply(.sub_packets, \(x) x[["value"]]) ),
        "5" = as.numeric(.sub_packets[[1]]$value > .sub_packets[[2]]$value),
        "6" = as.numeric(.sub_packets[[1]]$value < .sub_packets[[2]]$value),
        "7" = as.numeric(.sub_packets[[1]]$value == .sub_packets[[2]]$value)
    )

    if (is.na(.value)) browser()

    .out <- list(list(
            version = .v,
            type = .t,
            type_id = .type_id,
            message_str = .message,
            message = .message_decoded,
            remainder = .remainder,
            subpackets = .sub_packets,
            value = .value
        ))

    .remainder_all_0 <- all(.remainder == 0)
    if (is.na(.remainder_all_0)) browser()

    .still_packets_to_process <- .pos != .max_pos
    if (is.na(.still_packets_to_process)) browser()

    # If there are still packets to solve (i.e. traversing, then solve them)
    if (.still_packets_to_process & !.remainder_all_0) {
        .out <- c(.out, parse_packet(.remainder, .mapping, .pos = .pos + 1, .max_pos = .max_pos))
    }

    return(.out)
}

get_stuff <- function(.x, .what = "version", .stuff = character(0)) {
    .stuff <- append(.stuff, .x[[.what]])
    if (!is.null(.x$subpackets)) {
        for (sp in .x$subpackets) {
            .stuff <- get_stuff(sp, .what, .stuff)
        }
    }
    return(.stuff)
}

"D2FE28" |>
    build_binary(mapping) |>
    parse_packet(mapping)

x <- "38006F45291200" |>
    build_binary(mapping) |>
    parse_packet(mapping)



y <- "EE00D40C823060" |>
    build_binary(mapping) |>
    parse_packet(mapping)



.test <- "A0016C880162017C3686B18A3D4780" |>
    build_binary(mapping) |>
    parse_packet(mapping)

get_stuff(.test[[1]])


tests <- c("C200B40A82", "04005AC33890", "880086C3E88112", "CE00C43D881120", "D8005AC2A8F0", "F600BC2D8F", "9C005AC2F8F0", "9C0141080250320F1802104A08")
expected <- c(3, 54, 7, 9, 1, 0, 0, 1)
actual <- sapply(tests, \(.str) .str |> build_binary(mapping) |> parse_packet(mapping) |> (\(x) x[[1]]$value)()  )
all(expected == actual)




.input <- readLines("16/input.txt")
x <- .input |>
    build_binary(mapping) |>
    parse_packet(mapping)

options(scipen = 999)
x[[1]]$value
