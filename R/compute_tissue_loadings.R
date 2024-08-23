#' Compute compartment loadings for a dive table using the Buhlmann ZHL-16C model.
#' 
#' @param dive_tbl Tibble. A dive table for which the loadings of the first segment have already been computed with the function \code{link{initialize_tissue_loadings}}.
#' @param penalty Integer. Either 1 (most permissive decompression model), 2 or 3 (default, most conservative decompression model).
#' 
#' @return The same tibble as input, with the columns N2_load_start, N2_load_end, He_load_start and He_load_end filled with the tissue loadings for all segments of the dive.
#' @export
#' 
#' @examples
#' compute_tissue_loadings(dive_table)
compute_tissue_loadings <- function(dive_tbl, penalty = 3){
  
  for (i in 2:nrow(dive_tbl)) {
    dive_tbl$N2_load_start[[i]] <- dive_tbl$N2_load_end[[i-1]]
    dive_tbl$He_load_start[[i]] <- dive_tbl$He_load_end[[i-1]]
    tmp <- tissue_loadings(N2 = dive_tbl$N2_start[i], 
                           He = dive_tbl$He_start[i], 
                           depth_start = dive_tbl$depth_start[i], 
                           depth_end   = dive_tbl$depth_end[i], 
                           N2_load_start = dive_tbl$N2_load_start[i], 
                           He_load_start = dive_tbl$He_load_start[i], 
                           duration = dive_tbl$duration[i],
                           penalty = penalty)
    dive_tbl$N2_load_end[[i]] <- tmp[[1]]
    dive_tbl$He_load_end[[i]] <- tmp[[2]]
  }
  
  return(dive_tbl)
  
}