sample_target <- list(x = c(20, 30), y = c(-10, -5))
my_target <- list(x = c(34, 67), y = c(-215, -186))

out_of_range <- function(x, y, target) {
    x > max(target$x) | y < min(target$y)
}

reaches_target <- function(velx, vely, target) {
    x_range <- seq(target$x[[1]], target$x[[2]])
    y_range <- seq(target$y[[1]], target$y[[2]])

    y_visited <- numeric(0)

    x <- y <- 0
    dx <- velx
    dy <- vely

    while (!out_of_range(x, y, target)) {
        y_visited <- append(y_visited, y)

        x <- x + dx
        y <- y + dy

        if (x %in% x_range & y %in% y_range) {
            return(list(reaches = TRUE, yrange = y_visited))
        }

        dx <- if(dx < 0) dx + 1 else if (dx > 0) dx - 1 else 0
        dy <- dy - 1
    }
    return(list(reaches = FALSE, yrange = y_visited))
}

find_max_y <- function(target) {
    velocities <- expand.grid(
        velx = seq(0, max(target$x)),
        vely = seq(min(target$y), abs(min(target$y)))
    )
    velocities$results <- apply(velocities, 1, \(.row) reaches_target(.row[[1]], .row[[2]], target))
    velocities$reaches <- sapply(velocities$results, `[[`, 1)
    velocities$max_y <- sapply(velocities$results, \(.res) max(.res$yrange))
    max(velocities[velocities$reaches, "max_y"])
}

# part 1
find_max_y(sample_target)
find_max_y(my_target)








