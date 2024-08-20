#'  Increases the duration of the last stop
#'
#' @param last_tbl A data frame with the last 2 segment of the dive
#' @param steps Double. How much time should we add to the stop (decimal minutes)
#' @param penalty Integer. The penalty for the tissue loadings
increase_stop_duration <- function(last_tbl, steps, penalty = 3) {

  last_tbl[1, ]$duration <- last_tbl[1, ]$duration + steps
  
  # New loadings
  tmp <- tissue_loadings(
    N2 = last_tbl$N2_start[1],
    He = last_tbl$He_start[1],
    depth_start = last_tbl$depth_start[1],
    depth_end   = last_tbl$depth_end[1],
    N2_load_start = last_tbl$N2_load_start[1],
    He_load_start = last_tbl$He_load_start[1],
    duration = last_tbl$duration[1],
    penalty = penalty
  )
  last_tbl$N2_load_end[[1]] <- tmp[[1]]
  last_tbl$He_load_end[[1]] <- tmp[[2]]
  
  # M-values
  last_tbl$M_val[[1]] <- M_value(last_tbl$N2_load_end[1],
                                 last_tbl$He_load_end[1],
                                 last_tbl$depth_end[1])
  
  # Tensions
  last_tbl$tension[[1]] <- last_tbl$N2_load_end[[1]] + last_tbl$He_load_end[[1]]
  
  # Percent gradient
  last_tbl$percent_gradient[[1]] <- percent_gradient(last_tbl$tension[[1]],
                                                     last_tbl$depth_end[1],
                                                     last_tbl$M_val[[1]])
  
  last_tbl$leading_compartment[1] <- which.max(last_tbl$tension[[1]])
  last_tbl$leading_tension[1] <- max(last_tbl$tension[[1]])
  last_tbl$max_percent_gradient[1] <- max(last_tbl$percent_gradient[[1]])
  
  return(last_tbl)
}
