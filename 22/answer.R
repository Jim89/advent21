reboot_steps <- function(inpath) {
    ranges <- readLines(inpath) |>
        data.frame() |>
        setNames("x") |>
        tidyr::extract(
            x,
            c("instr", "xmin", "xmax", "ymin", "ymax", "zmin", "zmax"),
            "(on|off) x=(.*)\\.\\.(.*),y=(.*)\\.\\.(.*),z=(.*)\\.\\.(.*)",
            convert = TRUE
        )

    ranges <- ranges |>
        mutate(
            xout = (xmin > 50 | xmax < -50),
            yout = (ymin > 50 | ymax < -50),
            zout = (zmin > 50 | zmax < -50),
            xallin = between(xmin, -50, 50) & between(xmax, -50, 50),
            yallin = between(ymin, -50, 50) & between(ymax, -50, 50),
            zallin = between(zmin, -50, 50) & between(zmax, -50, 50)
        ) |>
        rowwise() |>
        mutate(
            full_in = all(xallin, yallin, zallin),
            allout = any(xout, yout, zout)
        )

    apply(
        filter(ranges, full_in),
        1, \(row) {
            .x <- expand.grid(x = row["xmin"]:row["xmax"], y = row["ymin"]:row["ymax"], z = row["zmin"]:row["zmax"])
            .x[["instr"]] <- row[["instr"]]
            .x
        },
        simplify = FALSE
    )
}

run_reboot <- function(grid, steps) {
    for (i in seq_along(steps)) {
        step <- steps[[i]]
        joined <- grid |>
            dplyr::left_join(step, c("x", "y", "z")) |>
            dplyr::mutate(
                instr = dplyr::if_else(is.na(instr), status, instr),
                status = instr
            )
        grid <- dplyr::select(joined, -instr)
    }
    grid
}

grid <- expand.grid(x = -50:50, y = -50:50, z = -50:50)
grid[["status"]] <- "off"

tester_steps <- reboot_steps("22/tester.txt")
sample_steps <- reboot_steps("22/sample.txt")
input_steps <- reboot_steps("22/input.txt")

grid |>
    run_reboot(tester_steps) |>
    dplyr::count(status)

grid |>
    run_reboot(sample_steps) |>
    dplyr::count(status)

grid |>
    run_reboot(input_steps) |>
    dplyr::count(status) # 648681 is correct
