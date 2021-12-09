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

digits <- read_digits("8/sample.txt")
lengths_to_keep <- names(which(table(lengths) == 1))
.out_only <- lapply(digits, `[[`, "output")
.output_lengths <- lapply(.out_only, nchar)
.to_keep <- lapply(.output_lengths, \(x) x %in% lengths_to_keep)
purrr::map2(.out_only, .to_keep, `[`) |>
    sapply(length) |>
    sum()



# Part 2 ------------------------------------------------------------------
real_digits_df <- c(
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
) |>
    tibble::enframe() |>
    dplyr::rename(digit = name, chars = value) |>
    dplyr::mutate(nc = nchar(chars))


.input <- read_digits("8/sample.txt")[[1]]$input
.output <- read_digits("8/sample.txt")[[1]]$output

.solved_digits <- .input |>
    sapply(\(x) NA_real_) |>
    enframe()


lengths_to_keep <- names(which(table(lengths) == 1))

.input_lengths <- sapply(.input, nchar)
known_inputs <- .input[.input_lengths %in% lengths_to_keep]


known_inputs |>
    sapply(nchar) |>
    tibble::enframe() |>
    dplyr::rename(nc = value) |>
    dplyr::left_join(real_digits_df) |>
    dplyr::select(name, digit)

# Part 2 -- with help ----------------------------------------------------------
# Holder to get wires active for each digit
wires_active_by_digit <- NULL

# Holder for the individual characters/wires activie, by digit
masked_wire_characters <- NULL


wires_active_by_digit[[1]] <- .input[nchar(.input) == 2] # We know 1 is when there are 2 wires active
wires_active_by_digit[[4]] <- .input[nchar(.input) == 4] # We know 4 is when there are 4 wires active
wires_active_by_digit[[7]] <- .input[nchar(.input) == 3] # We know 7 is when there are 3 wires active
wires_active_by_digit[[8]] <- .input[nchar(.input) == 7] # We know 8 is when all 7 wires are active

# Get wiring for known wires
masked_wire_characters[[1]] <- wires_active_by_digit[[1]] |> strsplit("") |> unlist()
masked_wire_characters[[4]] <- wires_active_by_digit[[4]] |> strsplit("") |> unlist()
masked_wire_characters[[7]] <- wires_active_by_digit[[7]] |> strsplit("") |> unlist()
masked_wire_characters[[8]] <- wires_active_by_digit[[8]] |> strsplit("") |> unlist()

# Everything else has either 5 or 6 segments/wires on
seg_5 <- .input[nchar(.input) == 5]
seg_6 <- .input[nchar(.input) == 6]

# The only 5 segment digit with 2 overlaps with digit 1 is digit 3
overlap_with_digit_1_segments <- seg_5 |>
    lapply(\(x) stringr::str_detect(x, masked_wire_characters[[1]])) |>
    sapply(mean) |>
    floor() |>
    as.logical()
wires_active_by_digit[[3]] <- seg_5[overlap_with_digit_1_segments]
seg_5 <- seg_5[!overlap_with_digit_1_segments]

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
# Both 6 and 9 have 2 of the same segments as 4, digit 0 only has 1
overlap_with_digit_4_segments <- seg_6 |>
    lapply(\(x) stringr::str_detect(x, segments_in_4_not_in_one)) |>
    sapply(mean) |>
    floor() |>
    as.logical()
wires_active_by_digit[[10]] <- seg_6[!overlap_with_digit_4_segments]

# Digit 9 contains all segments of 4
six_segment_overlaps_with_digit_4 <-  seg_6 |>
    lapply(\(x) str_detect(x, masked_wire_characters[[4]])) |>
    sapply(mean)

wires_active_by_digit[[9]] <- seg_6[six_segment_overlaps_with_digit_4 == 1]
wires_active_by_digit[[6]] <- seg_6[six_segment_overlaps_with_digit_4 != 1 & overlap_with_digit_4_segments]

wires_active_by_digit |>
    unlist() |>
    enframe("digit", "mapping") |>
    mutate(digit = digit - 1)
