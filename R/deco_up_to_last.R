#' Computes the time for each deco stop up to the last one (its duration will have to be adjusted later)
#'
#' @param dive_tbl A tibble with each segment of the dive on a row
#' @param gradient_low Double. The low value of the factor gradient
#' @param gradient_high Double. The high value of the factor gradient
#' @param first_stop Double. The depth of the first stop in meters
#' @param last_stop Double. The depth of the last stop in meters
#' @param steps Double. The time increment (in decimal minutes) to add to each deco stop in order to reavch the appropriate stop duration. Smaller values give more precise results but will increase the computation time
#' @param penalty Integer. Either 1 (most permissive decompression model), 2 or 3 (default, most conservative decompression model).
#'
#' @return The same tibble as \code{dive_tbl} but with \code{phase}, \code{time_end} and \code{duration} adjusted for each deco stop. All loadings, tensions, M-values and percent gradient are also re-computed with the correct deco stop durations.
#' @export
deco_up_to_last <- function(dive_tbl, gradient_low = 0.85, gradient_high = 0.90, 
                            first_stop, last_stop = 4, 
                            steps = 0.25, penalty = 3) {

  # Compute the gradient factors for each possible stop
  tmp2 <- seq(gradient_low * 100,
              gradient_high * 100,
              length.out = length(first_stop:1))

    # remove the last values of tmp2 to account for the last stop
  tmp2 <- c(tmp2[-((length(tmp2) - (last_stop - 1)):length(tmp2))], gradient_high * 100)
  
  stops_table <- dive_tbl |>
    dplyr::filter(.data$phase == "ascent", .data$depth_start <= first_stop) |>
    dplyr::mutate(target_GF = tmp2, 
                  phase = "deco") |>
    dplyr::relocate(.data$target_GF, .data$max_percent_gradient, .after = .data$phase)
  
  for (i in 1:nrow(stops_table)) {
    # For each segment in the deco zone
    # as long as the true gradient is higher than the target GF,
    # We add time to the segment and recompute everything for that segment
    while (stops_table$max_percent_gradient[i] >= stops_table$target_GF[i]) {
      stops_table[i, ]$duration <- stops_table[i, ]$duration + steps
      
      # New loadings
      tmp <- tissue_loadings(
        N2 = stops_table$N2_start[i],
        He = stops_table$He_start[i],
        depth_start = stops_table$depth_start[i],
        depth_end   = stops_table$depth_end[i],
        N2_load_start = stops_table$N2_load_start[i],
        He_load_start = stops_table$He_load_start[i],
        duration = stops_table$duration[i],
        penalty = penalty
      )
      stops_table$N2_load_end[[i]] <- tmp[[1]]
      stops_table$He_load_end[[i]] <- tmp[[2]]
      
      # M-values
      stops_table$M_val[[i]] <- M_value(stops_table$N2_load_end[i],
                                        stops_table$He_load_end[i],
                                        stops_table$depth_end[i])
      
      # Tensions
      stops_table$tension[[i]] <- stops_table$N2_load_end[[i]] + stops_table$He_load_end[[i]]
      
      # Percent gradient
      stops_table$percent_gradient[[i]] <- percent_gradient(stops_table$tension[[i]],
                                                            stops_table$depth_end[i],
                                                            stops_table$M_val[[i]])
      
      stops_table$leading_compartment[i] <- which.max(stops_table$tension[[i]])
      stops_table$leading_tension[i] <- max(stops_table$tension[[i]])
      stops_table$max_percent_gradient[i] <- max(stops_table$percent_gradient[[i]])
    }
    
    stops_table$time_end[i] <- stops_table$time_start[i] + stops_table$duration[i]
    
    if (i < nrow(stops_table)) {
      stops_table$time_start[i + 1] <- stops_table$time_end[i]
      stops_table$N2_load_start[[i + 1]] <- stops_table$N2_load_end[[i]]
      stops_table$He_load_start[[i + 1]] <- stops_table$He_load_end[[i]]
    }
    
    
  }
  
  dive_tbl <- dive_tbl |>
    dplyr::filter(!(.data$phase == "ascent" & .data$depth_start <= first_stop)) |>
    dplyr::bind_rows(stops_table)
  
  return(dive_tbl)
}