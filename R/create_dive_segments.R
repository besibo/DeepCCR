#' Creates a table with all segments of a dive
#'
#' @param max_depth Double. The maximum depth of the dive (in meters)
#' @param bottom_time Double. Total dive time (in minutes) befor starting the ascent
#' @param speed_desc Double. The speed of the descent (in meters per minute)
#' @param speed_asc Double. The speed of the ascent (in meters per minute)
#' @param last_stop Double. The depth of the last decompression stop (in meters)
#' @param speed_desc Double. The speed of the descent (in meters per minute)
#' 
#' @return A 5-column tibble with the diving phase, the start and end depth and time for each segment of the dive
#' @export
#' 
#' @examples
#' create_dive_segments(max_depth = 40, bottom_time = 50)
create_dive_segments <- function(max_depth,
                                 bottom_time,
                                 speed_desc = 20,
                                 speed_asc = 10,
                                 last_stop = 4) {
  # Table for the descent
  desc_tbl <- tibble::tibble(
    phase = "descent",
    depth_start = 0:(max_depth - 1),
    depth_end   = 1:(max_depth)
  ) |>
    dplyr::mutate(time_start = depth_start * (1 / speed_desc),
           time_end   = depth_end   * (1 / speed_desc))
  
  # Table for the bottom
  bottom_tbl <- tibble::tibble(
    phase = "bottom",
    depth_start = max_depth,
    depth_end   = max_depth,
    time_start = dplyr::last(desc_tbl$time_end),
    time_end   = bottom_time
  )
  
  # Table for the ascent. Deco stops will be added later
  asc_tbl <- tibble::tibble(
    phase = "ascent",
    depth_start = max_depth:last_stop,
    depth_end   = c((max_depth - 1):last_stop, 0),
    time_start = dplyr::last(bottom_tbl$time_end) + (max_depth - depth_start) *
      (1 / speed_asc),
    time_end   = dplyr::last(bottom_tbl$time_end) + (max_depth - depth_end) * (1 /
                                                                          speed_asc)
  )
  
  dive_tbl <- dplyr::bind_rows(desc_tbl, bottom_tbl, asc_tbl)
  return(dive_tbl)
}
