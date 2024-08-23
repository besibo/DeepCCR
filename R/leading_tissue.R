#' Which tissue compartment is the leading one?
#'
#' @param dive_tbl A Tibble containing information for all segments of a dive, including the compartment tensions and percent gradient
#'
#' @return The same tibble with 3 more numeric (\code{<dbl>}) variables: \code{leading_compartment} (of the 16 compartments of the Bühlman model, which is the leading one?), \code{leading_tension} (what is the tension of the leading compartment for each segment?) and \code{max_percent_gradient} (what is the corresponding percent gradient for this compartment?)
#' @export
#'
#' @examples
#' max_depth <- 45
#' bottom_time <- 30
#' dive_tbl <- max_depth |> 
#' create_dive_segments(bottom_time) |>
#'   compute_mix() |> 
#'   initialize_tissue_loadings() |> 
#'   compute_tissue_loadings() |> 
#'   deco_data() |> 
#'   leading_tissue()
leading_tissue <- function(dive_tbl) {
  
  dive_tbl <- dive_tbl |> 
    mutate(leading_compartment = map_int(tension, ~ which(.x == max(.x))),
           leading_tension = map_dbl(tension, max),
           max_percent_gradient = map_dbl(percent_gradient, ~max(.x)))
  
  return(dive_tbl)
}