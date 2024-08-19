#' Computes the M-values at the end of a segment, for each of the 16 tissues defined by Bühlmann (table ZH-L16 C), for both N2 and He.
#'
#' @param N2_load_end Double. The N2 loading at the end of the segment.
#' @param He_load_end Double. The He loading at the end of the segment.
#' @param depth_depth Double. The depth at the end of the segment.
M_value <- function(N2_load_end, He_load_end, depth_end) {
    MV <- ZHL16_C %>%
        select(Compartment, a, b) %>%
        mutate(load = c(N2_load_end[[1]], He_load_end[[1]])) %>%  # concatenates end loadings in N2 and He
        mutate(prod_a = a * load,   # Compute the product of the loadings and coeficients
               prod_b = b * load)

    out <- MV %>%
        group_by(Compartment) %>%  # for each compartment
        summarize(coef_a = sum(prod_a) / sum(load),  # Compute coef a and b
                  coef_b = sum(prod_b) / sum(load)) %>%
        transmute(M_val = coef_a + (depth_end + 10)/ coef_b) # and compute M values

    out$M_val
}
