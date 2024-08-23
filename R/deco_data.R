#' Compute M-values, tensions and percent_gradient for all segments
#' 
#' @param dive_tbl A tibble. Dive segments with loading already computed with \code{\link{compute_tissue_loadings}}
#' 
#' @return The same tibble as input, with M-values, tensions and percent_gradient computed for all segments. Warning: at this stage, the values computed don't take into account any decompression stops. They will be used later to calculate the time needed for each deco stop.
#' @export
#' 
#' @examples
#' deco_data(dive_table)

deco_data <- function(dive_tbl) {
  
  for (i in 1:nrow(dive_tbl)) {
    dive_tbl$M_val[[i]] <- M_value(dive_tbl$N2_load_end[i], 
                                   dive_tbl$He_load_end[i], 
                                   dive_tbl$depth_end[i])
    dive_tbl$tension[[i]] <- dive_tbl$N2_load_end[[i]] + dive_tbl$He_load_end[[i]]
    dive_tbl$percent_gradient[[i]] <- percent_gradient(dive_tbl$tension[[i]], 
                                                       dive_tbl$depth_end[i], 
                                                       dive_tbl$M_val[[i]])
  }
  
  return(dive_tbl)
  
}