#' For a dive where decompression is done during true stops every 3 meters, computes the time for each deco stop up to the surface
#'
#' @param dive_tbl A tibble with each segment of the dive on a row
#' @param gradient_low Double. The low value of the factor gradient
#' @param gradient_high Double. The high value of the factor gradient
#' @param first_stop Double. The depth of the first stop in meters
#' @param last_stop Double. The depth of the last stop in meters
#' @param steps Double. The time increment (in decimal minutes) to add to each deco stop in order to reavch the appropriate stop duration. Smaller values give more precise results but will increase the computation time
#' @param penalty Integer. Either 1 (most permissive decompression model), 2 or 3 (default, most conservative decompression model).

#'
#' @return A tibble of dive segments limited to the decompression zone. Each segment is either a deco stop or an ascent segment between two stops
#' @export
#'
#' @examples
#' fs <- first_deco_stop(dive_table, gradient_low = 0.85)
#' create_stops_table(dive_table, first_stop = fs)
create_stops_table <- function(dive_tbl, gradient_low = 0.85, 
                               gradient_high = 0.90, 
                               first_stop, last_stop = 4,
                               steps = 0.25, penalty = 3) {
  
  # Create a vector of stops
  stop_list <- seq(first_stop - first_stop %% 3 + 3, 0, -3)
  stop_list <- c(stop_list[!(stop_list <= last_stop)], last_stop, 0)
  
  # Compute the gradient limits for each of these stops
  gradient_limits <- stop_list * ((gradient_high - gradient_low) / (0 - first_stop)) + gradient_high
  
  # Create a table of stops: add segments for stops for all appropriate depths
  # at first, each stop has a duration of 0 seconds
  # we also set ascent speed to 3 meters per second between stops
  stops_table <- dive_tbl |> 
    dplyr::filter(.data$phase == "ascent" & .data$depth_start <= stop_list[1]) |> 
    dplyr::mutate(phase = dplyr::if_else(.data$depth_start %in% stop_list, "stop", "deco"),
                  time_start = .data$time_start[1] + (.data$depth_start[1] - .data$depth_start) * 1/3,
                  time_end = .data$time_start + (.data$depth_start - .data$depth_end) * 1/3,
                  duration = .data$time_end - .data$time_start)
  
  final_table <- stops_table |> 
    dplyr::first() |> 
    dplyr::mutate(phase = "temp")
  
  for (i in 1:nrow(stops_table)) {
    
    current_row <- stops_table[i, ]
    
    if (current_row$phase == "stop") {
      current_stop <- current_row |> 
        dplyr::mutate(depth_end = .data$depth_start,
                      time_end = .data$time_start,
                      duration = 0)
      
      tmp_table <- dplyr::bind_rows(current_stop, current_row)
    } else {
      tmp_table <- current_row
    }
    
    final_table <- dplyr::bind_rows(final_table, tmp_table)
  }
  
  # Here, we add the target GF to the table and remove the temporary row
  stops_table <- final_table |> 
    dplyr::filter(.data$phase != "temp") |> 
    dplyr::mutate(phase = dplyr::if_else(.data$depth_start == .data$depth_end, "stop", "deco"),
                  target_GF = 100 * (.data$depth_end * ((gradient_high - gradient_low) / (0 - first_stop)) + gradient_high)) |> 
    dplyr::relocate(.data$target_GF, .data$max_percent_gradient, .after = .data$phase)
  
  # Segments are grouped in blocs of 2 consecutive stops and the ascent segments in between
  bloc <- list()
  for (i in 1:(length(stop_list) - 1)) {
    current_stop <- stops_table |> 
      dplyr::filter(.data$depth_start > stop_list[i+1], .data$depth_start <= stop_list[i])
    
    next_stop <- stops_table |> 
      dplyr::filter(.data$depth_start == stop_list[i+1]) |> 
      dplyr::first()
    
    bloc[[i]] <- dplyr::bind_rows(current_stop, next_stop) |> 
      dplyr::filter(!is.na(.data$phase))
  }
  
  # For each bloc, we compute the time of the first stop based on the gradient 
  # factor of the next stop
  for (i in 1:length(bloc)) {
    
    bloc[[i]] <- deco_up_to_next(bloc[[i]], steps, penalty)
    
    if (i < length(bloc)) {
      bloc[[i+1]] <- bloc[[i]] |> 
        dplyr::last() |> 
        dplyr::bind_rows(bloc[[i+1]] |> dplyr::slice(-1))
      bloc[[i]] <- bloc[[i]] |> dplyr::slice(-dplyr::n())
    }
    
  }
  
  # We put all blocs back together
  bloc_final <- dplyr::bind_rows(bloc)
  
  # add we merge these stop segments with all other segments of the dive
  dive_tbl |> 
    dplyr::filter(!(.data$phase == "ascent" & .data$depth_start <= stop_list[1])) |> 
    dplyr::bind_rows(bloc_final)
    
}