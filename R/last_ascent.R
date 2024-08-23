#' Recomputes the loadings of the last ascent segment
#'
#' @param last_tbl A tibble containing the last 2 segments of the dive (last stop and ascent to the surface)
#' @param penalty Integer. Either 1 (most permissive decompression model), 2 or 3 (default, most conservative decompression model).
#'
#' @return The same tibble with the loadings, M-values, tensions, percent gradients, leading compartment, leading tension and max percent gradient of the last ascent segment recomputed.
#' @export
#'
last_ascent <- function(last_tbl, penalty = 3) {

  # Tissue loadings
  tmp <- tissue_loadings(
    N2 = last_tbl[2, ]$N2_start,
    He = last_tbl[2, ]$He_start,
    depth_start = last_tbl[2, ]$depth_start,
    depth_end   = last_tbl[2, ]$depth_end,
    N2_load_start = last_tbl[2, ]$N2_load_start,
    He_load_start = last_tbl[2, ]$He_load_start,
    duration = last_tbl[2, ]$duration,
    penalty = penalty
  )
  
  last_tbl$N2_load_end[[2]] <- tmp[[1]]
  last_tbl$He_load_end[[2]] <- tmp[[2]]
  
  # M-values
  last_tbl$M_val[[2]] <- M_value(last_tbl$N2_load_end[2],
                                 last_tbl$He_load_end[2],
                                 last_tbl$depth_end[2])
  
  # Tensions
  last_tbl$tension[[2]] <- last_tbl$N2_load_end[[2]] + last_tbl$He_load_end[[2]]
  
  # Percent gradient
  last_tbl$percent_gradient[[2]] <- percent_gradient(last_tbl$tension[[2]], last_tbl$depth_end[2], last_tbl$M_val[[2]])
  
  last_tbl$leading_compartment[2] <- which.max(last_tbl$tension[[2]])
  last_tbl$leading_tension[2] <- max(last_tbl$tension[[2]])
  last_tbl$max_percent_gradient[2] <- max(last_tbl$percent_gradient[[2]])
  
  return(last_tbl)
}
