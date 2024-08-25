#' Computes the N2 and He M-values at the end of a segment for each of the 16 compartments defined by Bühlmann's ZH-L16 C model.
#'
#' @param N2_load_end Numeric vector. The N2 loadings of the 16 compartments at the end of the segment.
#' @param He_load_end Numeric vector. The He loadings of the 16 compartments at the end of the segment.
#' @param depth_end Double. The depth at the end of the segment.
#'
#' @return A numeric vector with the M-values for each of the 16 compartments at the end of the segment.
#' @export
#' 
#'
#' @examples
#' bottom <- dive_table |>
#'  dplyr::filter(phase == "bottom")
#' M_value(N2_load_end = bottom$N2_load_end, 
#'         He_load_end = bottom$He_load_end, 
#'         depth_end = bottom$depth_end)
#'         
M_value <- function(N2_load_end, He_load_end, depth_end) {
  MV <- ZHL16_C |>
    dplyr::select(.data$Compartment, .data$a, .data$b) |>
    dplyr::mutate(load = c(N2_load_end[[1]], He_load_end[[1]]),
                  prod_a = .data$a * .data$load,
                  # Compute the product of the loadings and coeficients
                  prod_b = .data$b * .data$load)

out <- MV |>
  dplyr::group_by(.data$Compartment) |>   # for each compartment
  dplyr::summarize(coef_a = sum(.data$prod_a) / sum(.data$load),
                   # Compute coef a and b
                   coef_b = sum(.data$prod_b) / sum(.data$load)) |>
  dplyr::transmute(M_val = .data$coef_a + (depth_end + 10) / .data$coef_b) # and compute M values

out$M_val
}
