#' Computes loadings in N2 and He for the 16 compartments defined by Bühlmann's ZH-L16 C model
#'
#' @param N2 Double. The fraction of N2 in the gas mix
#' @param He Double. The fraction of He in the gas mix
#' @param depth_start Double. The depth at the start of the segment in meters
#' @param depth_end Double. The depth at the end of the segment in meters
#' @param duration Double. The duration of the segment in minutes (or in fraction of a minute)
#' @param N2_load_start a vector of 16 values representing the N2 loadings at the start of the segment
#' @param He_load_start a vector of 16 values representing the He loadings at the start of the segment
#' @param penalty Integer. Either 1 (most permissive decompression model), 2 or 3 (default, most conservative decompression model)
#' 
#' @return A list of two numeric vectors of length 16, the first one representing the N2 loadings and the second one the He loadings at the end of the segment
#' @export
#' 
#' @examples
#' tissue_loadings(N2 = 0.79, He = 0, depth_start = 0, depth_end = 10, 
#'                 duration = 1, N2_load_start = rep(0, 16), He_load_start = rep(0, 16))
tissue_loadings <- function(N2, He, depth_start, depth_end, duration, N2_load_start, He_load_start, penalty = 3) {

    period <- ZHL16_C |> 
      dplyr::select(.data$Molecule, .data$Periode)
    ambiant_pressure <- depth_start + 10

    quot	<- c(0.627,0.567,0.493)
    p_H2O	<- quot[penalty]
    pi_N2	<- (ambiant_pressure - p_H2O) * N2
    pi_He	<- (ambiant_pressure - p_H2O) * He

    if (depth_start != depth_end) {
        speed <- round((depth_end - depth_start) / duration)
        r_N2 <- speed * N2
        r_He <- speed * He
    } else {
        r_N2 <- 0
        r_He <- 0
    }

    K <- log(2) / period$Periode
    K_N2 <- K[1:16]
    K_He <- K[17:32]

    po_N2 <- N2_load_start[[1]]
    po_He <- He_load_start[[1]]

    t <- duration

    p_N2 <- pi_N2 + r_N2 * (t - 1/K_N2) - (pi_N2 - po_N2 - r_N2 / K_N2) * exp(-K_N2 * t)
    p_He <- pi_He + r_He * (t - 1/K_He) - (pi_He - po_He - r_He / K_He) * exp(-K_He * t)
    
    return(list(p_N2, p_He))
}
