real_digits <- c(
    "0" = "abcefg",
    "1" = "cf" ,
    "2" = "acdeg",
    "3" = "acdfg",
    "4" = "4cdf",
    "5" = "abdfg",
    "6" = "abdefg",
    "7" = "acf",
    "8" = "abcdefg",
    "9" = "abcdfg"
)

lengths <- sapply(real_digits, nchar)

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


real_digits[names(lengths[which(lengths %in% names(which(table(lengths) == 1)))])]

digits <- read_digits("8/sample.txt")
lengths_to_keep <- names(which(table(lengths) == 1))
.out_only <- lapply(digits, `[[`, "output")
.output_lengths <- lapply(.out_only, nchar)
.to_keep <- lapply(.output_lengths, \(x) x %in% lengths_to_keep)
purrr::map2(.out_only, .to_keep, `[`) |>
    sapply(length) |>
    sum()


