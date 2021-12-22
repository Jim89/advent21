cubes_intersect <- function(cube1, cube2) {
    min_x <- max(cube1[["xmin"]], cube2[["xmin"]])
    max_x <- min(cube1[["xmax"]], cube2[["xmax"]])
    xover <- min_x > max_x

    min_y <- max(cube1[["ymin"]], cube2[["ymin"]])
    max_y <- min(cube1[["ymax"]], cube2[["ymax"]])
    yover <- min_y > max_y

    min_z <- max(cube1[["zmin"]], cube2[["zmin"]])
    max_z <- min(cube1[["zmax"]], cube2[["zmax"]])
    zover <- min_z > max_z

    !any(c(xover, yover, zover))
}

get_intersect_region <- function(cube1, cube2) {
    min_x <- max(cube1[["xmin"]], cube2[["xmin"]])
    max_x <- min(cube1[["xmax"]], cube2[["xmax"]])

    min_y <- max(cube1[["ymin"]], cube2[["ymin"]])
    max_y <- min(cube1[["ymax"]], cube2[["ymax"]])

    min_z <- max(cube1[["zmin"]], cube2[["zmin"]])
    max_z <- min(cube1[["zmax"]], cube2[["zmax"]])

    data.frame(xmin = min_x, xmax = max_x, ymin = min_y, ymax = max_y, zmin = min_z, zmax = max_z)
}

ranges <- readLines("22/tester.txt") |>
    data.frame() |>
    setNames("x") |>
    tidyr::extract(
        x,
        c("instr", "xmin", "xmax", "ymin", "ymax", "zmin", "zmax"),
        "(on|off) x=(.*)\\.\\.(.*),y=(.*)\\.\\.(.*),z=(.*)\\.\\.(.*)",
        convert = TRUE
    )


build_ranges <- function(ranges) {
    browser()
    new_ranges <- vector("list", 10000)
    i <- 1
    for (instr_index in 1:nrow(ranges)) {
        # Get the current instruction details
        current_instruction <- ranges[instr_index, , drop = FALSE]
        current_switch <- current_instruction[["instr"]]

        if (i == 1) {
            # First time around, just carry on
            new_ranges[[1]] <- current_instruction
            i <- i + 1
            next
        }

        # Get all the previous instructions (so on the 2nd row, this is the first
        # instruction, 3rd row it's the first 2 etc)
        prev_instructions <- purrr::compact(new_ranges)

        # Only add the current if it's "on", for "offs" we just add the compensating rule(s)
        if (current_switch == "on") {
            # Put the current rule into the set of new rules
            put_at <- length(purrr::compact(new_ranges)) + 1
            new_ranges[[put_at]] <- current_instruction
        }

        for (prev_instruction_index in seq_along(prev_instructions)) {

            prior_instruction <- prev_instructions[[prev_instruction_index]]
            prior_switch <- prior_instruction$instr

            # Don't need to bother looking for intersections if the previous row
            # was "off"
            if (prior_switch == "off") next

            # Check if the current instruction overlaps with it
            they_intersect <- cubes_intersect(current_instruction, prior_instruction)

            # If they intersect, we need to make a compensating rule
            if (they_intersect) {
                intersect_region <- get_intersect_region(current_instruction, prior_instruction)
                intersect_region$instr <- "off"

                compensating_rule <- intersect_region |>
                    dplyr::select(instr, dplyr::everything())

                put_at <- length(purrr::compact(new_ranges)) + 1
                new_ranges[[put_at]] <- compensating_rule
                i <- i + 1
            }
        }
    }
    new_ranges |>
        purrr::compact() |>
        purrr::map_dfr(I)
}

ranges |>
    build_ranges() |>
    dplyr::mutate(
        # Adjust by 1 each to to count the full range
        combos = (xmax - xmin + 1) * (ymax - ymin + 1) * (zmax - zmin + 1),
        to_sum = dplyr::if_else(instr == "on", combos, -1 * combos)
    )



# For each instruction:
    # If on:
        # Figure out if it overlaps with the preceding one:
        # No? Just add it to the list
        # Yes? add a compensating off
    # If off:
        # Figure out of intersects

# Set up list of final instructions
# Then for each provided instruction
# If it's the first, just add it to the list
# If it's not the first, compare it against all existing final instructions and update the final instructions list
