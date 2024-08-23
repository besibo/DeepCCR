#' Computes compartment loadings in N2 and He for the first segment of a dive
#' 
#' @param dive_tbl A tibble with dive segments as produced by \code{\link{compute_mix}}
#' @param penalty Integer. Either 1 (most permissive decompression model), 2 or 3 (default, most conservative decompression model)

#' @return The same tibble as input, with the columns N2_load_start, N2_load_end, He_load_start and He_load_end filled with the tissue loadings for the first segment of the dive
#' @export
#' 
#' @examples
#' initialize_tissue_loadings(dive_table)
initialize_tissue_loadings <- function(dive_tbl, penalty = 3) {
  
  quot	<- c(0.627,0.567,0.493)  # Do not change these values!
  p_H2O	<- quot[penalty]
  
  dive_tbl$N2_load_start[[1]] <- rep((10 - p_H2O) * 0.795, 16)
  
  seg1 <- dive_tbl[1,] 
  tmp <- tissue_loadings(N2 = seg1$N2_start, 
                         He = seg1$He_start, 
                         depth_start = seg1$depth_start, 
                         depth_end = seg1$depth_end, 
                         duration = seg1$duration,
                         N2_load_start = seg1$N2_load_start, 
                         He_load_start = seg1$He_load_start, 
                         penalty = penalty)
  
  dive_tbl$N2_load_end[[1]] <- tmp[[1]]
  dive_tbl$He_load_end[[1]] <- tmp[[2]]
  
  return(dive_tbl)

}