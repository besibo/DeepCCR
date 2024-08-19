#' EAD computes the Maximum Equivalent Air Depth (EAD) of one or several gas at Max_depth
#' N2: tibble, vector or scalar (dbl). Fraction of N2 in the gas mix
#' depth: tibble, vector or scalar (dlb). Current depth of the dive in meters (msw)
EAD <- function(N2, depth) {

    PN2_at_depth <- (N2) * (depth / 10 + 1)
    EAD_at_depth <- PN2_at_depth * 10 / 0.79 - 10
    EAD_at_depth[EAD_at_depth < 0] <- 0

    return(EAD_at_depth)
}
