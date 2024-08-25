#' Adjust the duration of the last stop so that the ascent from the last stop to the surface is done at a normal speed, and without exceeding the gradient_high value.
#'
#' @param dive_tbl A tibble with the dive profile where each row is a segment of the dive. All sgemnts should be correct except for the last 2 (last stop and ascent from the last stop to the surface)
#' @param last_stop Double. The depth of the last decompression stop (in meters)
#' @param speed_asc Double. The speed of the ascent (in meters per minute)
#' @param penalty Integer. Either 1 (most permissive decompression model), 2 or 3 (default, most conservative decompression model)
#' @param steps Double. The time increment (in decimal minutes) to add to each deco stop in order to reavch the appropriate stop duration. Smaller values give more precise results but will increase the computation time
#'
#' @return The same tibble as \code{dive_tbl} but with the last 2 rows corrected
#' @export
#'
#' @examples
#' fst <- first_deco_stop(dive_table)
#' dive_table |> 
#'  deco_up_to_last(first_stop = fst) |> 
#'  deco_last()
#'  
deco_last <- function(dive_tbl, last_stop = 4, speed_asc = 10, penalty = 3, steps = 0.25) {

    last_tbl <- utils::tail(dive_tbl, 2)
  
  # Fix the duration of the last ascent segment
  last_tbl[2, ]$duration <- (last_tbl[2, ]$depth_start - last_tbl[2, ]$depth_end) / speed_asc
  
  # Recompute the loadings of the last ascent segment
  last_tbl <- last_ascent(last_tbl, penalty)
  
  while (last_tbl$max_percent_gradient[2] > last_tbl$target_GF[2]) {
    last_tbl <- adjust_last_stop_duration(last_tbl, steps, penalty)
    
    last_tbl$time_start[2] <- last_tbl$time_end[1]
    last_tbl$time_end[2] <- last_tbl$time_start[2] + last_tbl$duration[2]
    last_tbl$N2_load_start[[2]] <- last_tbl$N2_load_end[[1]]
    last_tbl$He_load_start[[2]] <- last_tbl$He_load_end[[1]]
    
    last_tbl <- last_ascent(last_tbl, penalty)
    
  }
  
  last_tbl
  
  # Replace last 2 rows of dive_tbl by last_tbl
  dive_tbl <- dive_tbl |>
    dplyr::filter(!(.data$phase == "deco" & .data$depth_start <= (last_stop + 1))) |>
    dplyr::bind_rows(last_tbl)
  
  return(dive_tbl)
  
}