#' Determines the Depth of the First Deco Stop
#'
#' @param dive_tbl A tibble with one row for each segment of the dive
#' @param gradient_low Double. The minimum gradient for a stop to be considered a deco stop
#'
#' @return Double. The depth (in meters) of the first deco stop
#' @export
#'
#' @examples
#' first_deco_stop(dive_table, gradient_low = 0.85)
#' first_deco_stop(dive_table, gradient_low = 0.30)
first_deco_stop <- function(dive_tbl, gradient_low = 0.85) {
  dive_tbl |>
    dplyr::filter(max_percent_gradient >= gradient_low * 100) |>
    dplyr::first() |>
    dplyr::pull(depth_start)
}