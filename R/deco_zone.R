#' Determines the depth at which the diver enters the decompression zone
#'
#' @param dive_tbl A Tibble containing information for all segments of a dive
#'
#' @return A numeric (\code{<int>}) value indicating the depth at which the diver enters the decompression zone
#' @export
#'
#' @examples
#' deco_zone(dive_table)
deco_zone <- function(dive_tbl) {
  
  dive_tbl |> 
    dplyr::mutate(deco_zone = ambiant_pressure_end * 10 - leading_tension) |> 
    dplyr::filter(deco_zone < 0) |>
    dplyr::first() |>
    dplyr::pull(depth_start)
  
}