#' Which tissue compartment is the leading one?
#'
#' @param dive_tbl A Tibble containing information for all segments of a dive, including the compartment tensions and percent gradient
#'
#' @return The same tibble with 3 more numeric (\code{<dbl>}) variables: \code{leading_compartment} (of the 16 compartments of the Bühlman model, which is the leading one?), \code{leading_tension} (what is the tension of the leading compartment for each segment?) and \code{max_percent_gradient} (what is the corresponding percent gradient for this compartment?)
#' @export
#'
#' @examples
#' leading_tissue(dive_table))
leading_tissue <- function(dive_tbl) {
  
  dive_tbl <- dive_tbl |> 
    dplyr::mutate(leading_compartment = purrr::map_int(tension, ~ which(.x == max(.x))),
           leading_tension = purrr::map_dbl(tension, max),
           max_percent_gradient = purrr::map_dbl(percent_gradient, ~max(.x)))
  
  return(dive_tbl)
}