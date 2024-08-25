#' Given a set of parameters for a dive, \code{profile} calculates the dive profile by slicing the dive into small segments of 1 meters and computing all decompression data for each segment up to the surface.
#'
#' @param max_depth Double. The maximum depth of the dive (in meters)
#' @param bottom_time Double. Total dive time (in minutes) befor starting the ascent
#' @param speed_desc Double. The speed of the descent (in meters per minute)
#' @param speed_asc Double. The speed of the ascent (in meters per minute)
#' @param last_stop Double. The depth of the last decompression stop (in meters)
#' @param ppO2_low Double. The low setting for the partial pressure of oxygen
#' @param ppO2_high Double. The high setting for the partial pressure of oxygen
#' @param ppO2_switch_depth Double. The depth at which the partial pressure of oxygen switches from the low to the high setting
#' @param diluent A numeric vector of length 2 with the percentage of oxygen, and helium in the diluent
#' @param penalty Integer. Either 1 (most permissive decompression model), 2 or 3 (default, most conservative decompression model)
#' @param gradient_low Double. The low value of the factor gradient
#' @param gradient_high Double. The high value of the factor gradient
#' @param steps 
#'
#' @return A tibble with the dive profile where each row is a segment of the dive. See \link{\code{dive_table}} for a detailed description of each variable
#' @export
#'
#' @examples
#' dive_profile <- profile(max_depth = 40, bottom_time = 50, speed_desc = 20, speed_asc = 10, 
#'         last_stop = 4, ppO2_low = 0.7, ppO2_high = 1.3, 
#'         ppO2_switch_depth = 15, diluent = c(21, 00), penalty = 3, 
#'         gradient_low = 0.85, gradient_high = 0.90, steps = 0.25)
#' dive_profile |> print(n = Inf)
dive_profile <- function(max_depth, bottom_time, speed_desc, speed_asc, last_stop, 
                         ppO2_low, ppO2_high, ppO2_switch_depth, diluent, penalty, 
                         gradient_low, gradient_high, steps) {
  
  # 1. Create the dive table
  # When diving with a CCR, the composition of the breathing gas in the loop  
  # changes continually with depth. Here, we approximate this continuous change
  # by dividing the dive into small segments. Each segment is defined by a start
  # and end depth, and a start and end time. The composition of the breathing gas
  # is assumed to be constant within each segment.
  
  # The code below does the following:
  # a. Creates a segments table 
  # b. Adds columns to track the composition of the breathing gas and empty
  #    columns for later computations
  # c. Initializes compartment loadings for both N2 and He for the first segment
  # d. Computes the compartment loadings for all other segments based on the 1st
  # e. Compute M-values, tensions and percent_gradient for all segments
  # f. Get the leading compartment, its tension and % gradient for all segments
  dive_tbl <- max_depth |> 
    create_dive_segments(bottom_time, speed_desc, speed_asc, last_stop) |>
    compute_mix(ppO2_low, ppO2_high, ppO2_switch_depth, diluent) |> 
    initialize_tissue_loadings(penalty) |> 
    compute_tissue_loadings(penalty) |> 
    deco_data() |> 
    leading_tissue()
  
  # 2. Where is first stop?
  first_stop <- first_deco_stop(dive_tbl, gradient_low)
  
  # 3. Add deco stops
  
  # a. First, for all segments but the last 2
  # b. Then, for the last 2 segments so that the ascent from the last stop  
  #    to the surface is done at a normal speed, and without exceeding the 
  #    gradient_high value.
  dive_tbl <- dive_tbl |> 
    deco_up_to_last(gradient_low, gradient_high, first_stop, last_stop, 
                    steps, penalty) |> 
    deco_last(last_stop = last_stop, speed_asc = speed_asc, 
              penalty = penalty, steps = steps)
  
  return(dive_tbl)
}