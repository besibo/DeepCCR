#' Computes the Equivalent Air Depth (EAD) of one or several gas at any depth
#'
#' @param N2 Double. Fraction of N2 in the gas mix
#' @param depth Double. Depth of the dive in meters
#' 
#' @return Double. Equivalent Air Depth (EAD) in meters
#' @export
#' 
#' @examples
#' EAD(N2 = 0.60, depth = 30)
#' EAD(N2 = 0.30, depth = 100)
EAD <- function(N2, depth) {

    PN2_at_depth <- (N2) * (depth / 10 + 1)
    EAD_at_depth <- PN2_at_depth * 10 / 0.79 - 10
    EAD_at_depth[EAD_at_depth < 0] <- 0

    return(EAD_at_depth)
}
