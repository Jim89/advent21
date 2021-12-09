library(tibble)
library(dplyr)
library(stringr)
library(purrr)

# Helpers ----------------------------------------------------------------------
read_digits <- function(.file) {
    .file |>
        readLines() |>
        strsplit("\\s\\|\\s") |>
        lapply(\(.x) {
            list(
                "input" = strsplit(.x[[1]], "\\s")[[1]],
                "output" = strsplit(.x[[2]], "\\s")[[1]]
            )
        })
}

real_digits <- c(
    "0" = "abcefg",
    "1" = "cf" ,
    "2" = "acdeg",
    "3" = "acdfg",
    "4" = "bcdf",
    "5" = "abdfg",
    "6" = "abdefg",
    "7" = "acf",
    "8" = "abcdefg",
    "9" = "abcdfg"
)

lengths <- sapply(real_digits, nchar)

real_digits[names(lengths[which(lengths %in% names(which(table(lengths) == 1)))])]

digits <- read_digits("8/input.txt")
lengths_to_keep <- names(which(table(lengths) == 1))
.out_only <- lapply(digits, `[[`, "output")
.output_lengths <- lapply(.out_only, nchar)
.to_keep <- lapply(.output_lengths, \(x) x %in% lengths_to_keep)
purrr::map2(.out_only, .to_keep, `[`) |>
    sapply(length) |>
    sum()



# Part 2 ------------------------------------------------------------------
generate_input_mapping <- function(.input) {
    # "Stolen" / cribbed from https://twitter.com/jordifigueras/status/1468598875959222276?s=20
    # Holder to get wires active for each digit
    wires_active_by_digit <- vector("character", length = 10L)

    # Holder for the individual characters/wires activie, by digit
    masked_wire_characters <- replicate(10, NULL)

    # Assign those where we _know_ what the digit is based on the number of segments
    wires_active_by_digit[[1]] <- .input[nchar(.input) == 2] # We know 1 is when there are 2 wires active
    wires_active_by_digit[[4]] <- .input[nchar(.input) == 4] # We know 4 is when there are 4 wires active
    wires_active_by_digit[[7]] <- .input[nchar(.input) == 3] # We know 7 is when there are 3 wires active
    wires_active_by_digit[[8]] <- .input[nchar(.input) == 7] # We know 8 is when all 7 wires are active

    # Get segments for each digit we know about
    segments_for_digit <- function(digit) wires_active_by_digit[[digit]] |> strsplit("") |> unlist()
    masked_wire_characters[[1]] <- segments_for_digit(1)
    masked_wire_characters[[4]] <- segments_for_digit(4)
    masked_wire_characters[[7]] <- segments_for_digit(7)
    masked_wire_characters[[8]] <- segments_for_digit(8)

    # Every other digit  has either 5 or 6 segments
    seg_5 <- .input[nchar(.input) == 5]
    seg_6 <- .input[nchar(.input) == 6]

    # The only 5 segment digit with 2 segment overlaps with digit 1 is digit 3
    overlap_with_digit_1_segments <- seg_5 |>
        lapply(\(x) stringr::str_detect(x, masked_wire_characters[[1]])) |>
        sapply(mean)

    wires_active_by_digit[[3]] <- seg_5[overlap_with_digit_1_segments == 1]

    # Then chop out digit 3 (we just found it)
    seg_5 <- seg_5[!overlap_with_digit_1_segments == 1]

    # digit 5 contains 2 segments from digit 4 that are _not_ in digit 1
    segments_in_4_not_in_one <- setdiff(masked_wire_characters[[4]], masked_wire_characters[[1]])
    overlap_with_digit_4_segments <- seg_5 |>
        lapply(\(x) stringr::str_detect(x, segments_in_4_not_in_one)) |>
        sapply(mean) |>
        floor() |>
        as.logical()
    wires_active_by_digit[[5]] <- seg_5[overlap_with_digit_4_segments]

    # And 2, the other 5-segment digit, has to be the other one
    wires_active_by_digit[[2]] <- seg_5[!overlap_with_digit_4_segments]

    # And those with 6 wires:
    # Both digits 6 and 9 have 2 of the same segments as 4, digit 0 only has 1
    some_overlap_with_digit_4_segments <- seg_6 |>
        lapply(\(x) stringr::str_detect(x, segments_in_4_not_in_one)) |>
        sapply(mean) |>
        floor() |>
        as.logical()
    wires_active_by_digit[[10]] <- seg_6[!some_overlap_with_digit_4_segments]

    # Digit 9 contains all 4 segments of digit 4
    six_segment_overlaps_with_digit_4 <-  seg_6 |>
        lapply(\(x) str_detect(x, masked_wire_characters[[4]])) |>
        sapply(mean)

    wires_active_by_digit[[9]] <- seg_6[six_segment_overlaps_with_digit_4 == 1]
    wires_active_by_digit[[6]] <- seg_6[six_segment_overlaps_with_digit_4 != 1 & some_overlap_with_digit_4_segments]

    wires_active_by_digit |>
        unlist() |>
        enframe("digit", "mapping") |>
        mutate(mapping = unlist(map(mapping, resort)))
}

resort <- function(.x) {
    paste0(sort(strsplit(.x, "")[[1]]), collapse = "")
}

decode_output <- function(mapping, output) {
    output |>
        map_chr(resort) |>
        enframe() |>
        select(code = value) |>
        left_join(mapping, by = c("code" = "mapping")) |>
        pull(digit) |>
        paste0(collapse = "")
}

decode_outputs <- function(.line) {
    decode_output(
        generate_input_mapping(.line$input),
        .line$output
    )
}

inputs <- read_digits("8/sample.txt")
inputs |>
    map_chr(decode_outputs) |>
    map_dbl(as.double) |>
    reduce(`+`)

