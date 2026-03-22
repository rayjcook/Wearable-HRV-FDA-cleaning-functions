#' Create a normalized time grid
#'
#' Creates a sequence of evenly spaced time points from 0 to 1.
#' This is useful for interpolating wearable data onto a common scale.
#'
#' @param n_time Number of points in the time grid.
#'
#' @return A numeric vector of normalized time points.
#' @export
create_time_grid <- function(n_time = 100) {

  if (!is.numeric(n_time) || length(n_time) != 1 || n_time <= 1) {
    stop("`n_time` must be a single number greater than 1.")
  }

  seq(0, 1, length.out = n_time)
}
