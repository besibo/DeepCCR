#' Computes the time for one deco stop and the ascent up to the next stop. Necessary when the decompression is done with true deco stops every 3 meters.
#'
#' @param dive_tbl A tibble with only segments for two consecutive stops and ascent segments between them
#' @param steps Double. The time increment (in decimal minutes) to add to each deco stop in order to reavch the appropriate stop duration. Smaller values give more precise results but will increase the computation time
#' @param penalty Integer. Either 1 (most permissive decompression model), 2 or 3 (default, most conservative decompression model).
#'
#' @return The same tibble as \code{dive_tbl} but with \code{phase}, \code{time_end} and \code{duration} adjusted for each deco stop. All loadings, tensions, M-values and percent gradient are also re-computed with the correct deco stop durations.
#' @export
#' 
#' @details This function is used when the decompression is not just a slow and continuous ascent, but when true stops are made every 3 meters from the first stop to the surface. The duration of a stop is set so that the ascent to the next stop is possible without going above the factor gradient set for the next stop.
#' 
deco_up_to_next <- function(dive_tbl, steps = 0.25, penalty = 3) {
  
  l <- dive_tbl |> last()
  
  
    while (l$max_percent_gradient > l$target_GF) {
      
      dive_tbl[1, ]$duration <- dive_tbl[1, ]$duration + steps

      for (i in 1:nrow(dive_tbl)) {
        
        # New loadings
        tmp <- tissue_loadings(
          N2 = dive_tbl$N2_start[i],
          He = dive_tbl$He_start[i],
          depth_start = dive_tbl$depth_start[i],
          depth_end   = dive_tbl$depth_end[i],
          N2_load_start = dive_tbl$N2_load_start[i],
          He_load_start = dive_tbl$He_load_start[i],
          duration = dive_tbl$duration[i],
          penalty = penalty
        )
        dive_tbl$N2_load_end[[i]] <- tmp[[1]]
        dive_tbl$He_load_end[[i]] <- tmp[[2]]
        
        # M-values
        dive_tbl$M_val[[i]] <- M_value(dive_tbl$N2_load_end[i],
                                       dive_tbl$He_load_end[i],
                                       dive_tbl$depth_end[i])
        
        # Tensions
        dive_tbl$tension[[i]] <- dive_tbl$N2_load_end[[i]] + dive_tbl$He_load_end[[i]]
        
        # Percent gradient
        dive_tbl$percent_gradient[[i]] <- percent_gradient(dive_tbl$tension[[i]],
                                                           dive_tbl$depth_end[i],
                                                           dive_tbl$M_val[[i]])
        
        dive_tbl$leading_compartment[i] <- which.max(dive_tbl$tension[[i]])
        dive_tbl$leading_tension[i] <- max(dive_tbl$tension[[i]])
        dive_tbl$max_percent_gradient[i] <- max(dive_tbl$percent_gradient[[i]])
        
        
        dive_tbl$time_end[i] <- dive_tbl$time_start[i] + dive_tbl$duration[i]  
        
        if (i < nrow(dive_tbl)) {
          dive_tbl$time_start[i + 1] <- dive_tbl$time_end[i]
          dive_tbl$N2_load_start[[i + 1]] <- dive_tbl$N2_load_end[[i]]
          dive_tbl$He_load_start[[i + 1]] <- dive_tbl$He_load_end[[i]]
        }
        
      }

      l <- dive_tbl |> last()
    
    
  }
  
  return(dive_tbl)
}