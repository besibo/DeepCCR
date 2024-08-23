#' Computes the N2 and He M-values at the end of a segment for each of the 16 compartments defined by Bühlmann's ZH-L16 C model.
#'
#' @param N2_load_end Numeric vector. The N2 loadings of the 16 compartments at the end of the segment.
#' @param He_load_end Numeric vector. The He loadings of the 16 compartments at the end of the segment.
#' @param depth_depth Double. The depth at the end of the segment.
#'
#' @return A numeric vector with the M-values for each of the 16 compartments at the end of the segment.
#' @export
#'
#' @examples
#' bottom <- dive_table |>
#'  dplyr::filter(phase == "bottom")
#' M_value(N2_load_end = bottom$N2_load_end, 
#'         He_load_end = bottom$He_load_end, 
#'         depth_end = bottom$depth_end)
M_value <- function(N2_load_end, He_load_end, depth_end) {
  MV <- ZHL16_C |>
    dplyr::select(Compartment, a, b) |>
    dplyr::mutate(load = c(N2_load_end[[1]], He_load_end[[1]]),
                  prod_a = a * load,
                  # Compute the product of the loadings and coeficients
                  prod_b = b * load)

out <- MV |>
  dplyr::group_by(Compartment) |>   # for each compartment
  dplyr::summarize(coef_a = sum(prod_a) / sum(load),
                   # Compute coef a and b
                   coef_b = sum(prod_b) / sum(load)) |>
  dplyr::transmute(M_val = coef_a + (depth_end + 10) / coef_b) # and compute M values

out$M_val
}
