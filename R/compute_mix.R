#' Compute the breathing gas mix for each dive segment and sets up the dive table for loadings computations
#' 
#' @param dive_segments A tibble with dive segments as produced by the function \code{\link{create_dive_segments}}
#' @param ppO2_low Double. The low setting for the partial pressure of oxygen
#' @param ppO2_high Double. The high setting for the partial pressure of oxygen
#' @param ppO2_switch_depth Double. The depth at which the partial pressure of oxygen switches from the low to the high setting
#' @param diluent A numeric vector of length 2 with the percentage of oxygen, and helium in the diluent
#' 
#' @return A tibble with the composition of the breathing gas at the start and end of each segment, the partial pressures of oxygen nitrogen and helium at the start and end of each segment, the equivalent air depth at the start and end of each segment, and columns for tissue loadings, tensions, M-values and percent gradients
#' @export
#' 
#' @examples
#' create_dive_segments(max_depth = 40, bottom_time = 50) |> 
#' compute_mix()
compute_mix <- function(dive_segments,
                        ppO2_low = 0.7,
                        ppO2_high = 1.3,
                        ppO2_switch_depth = 15,
                        diluent = c(21, 0)) {
  dive_tbl <- dive_segments |>
    dplyr::mutate(
      # Duration of each segment
      duration = .data$time_end - .data$time_start,
      # Set appropriate ppO2 for each segment
      ppO2_mix_start = dplyr::if_else(
        .data$phase == "descent" & .data$depth_start < ppO2_switch_depth |
        .data$phase == "ascent" & .data$depth_start <= 3,
        ppO2_low,
        ppO2_high
      ),
      ppO2_mix_end = dplyr::if_else(
        .data$phase == "descent" & .data$depth_end < ppO2_switch_depth |
        .data$phase == "ascent" & .data$depth_end <= 3,
        ppO2_low,
        ppO2_high
      ),
      # Compute ambiant pressures at the start and end of each segment
      ambiant_pressure_start = .data$depth_start / 10 + 1,
      ambiant_pressure_end   = .data$depth_end / 10 + 1,
      # Composition of the breathing gas at the start and end of each segment
      mix_start  = purrr::map2(.data$ppO2_mix_start, .data$depth_start, ~ loop_mix(diluent, .x, .y)),
      mix_end    = purrr::map2(.data$ppO2_mix_end, .data$depth_end, ~ loop_mix(diluent, .x, .y))
    ) |>
    tidyr::unnest_wider(c(.data$mix_start, .data$mix_end), names_sep = "_") |>
    # Rename columns for the compistion of the breathing gas
    dplyr::rename(
      O2_start = .data$mix_start_1,
      N2_start = .data$mix_start_2,
      He_start = .data$mix_start_3,
      O2_end   = .data$mix_end_1,
      N2_end   = .data$mix_end_2,
      He_end   = .data$mix_end_3
    ) |>
    # Partial pressures for inert gases at the start and end of each segment
    dplyr::mutate(
      ppN2_mix_start = .data$N2_start * .data$ambiant_pressure_start,
      ppHe_mix_start = .data$He_start * .data$ambiant_pressure_start,
      ppN2_mix_end   = .data$N2_end * .data$ambiant_pressure_end,
      ppHe_mix_end   = .data$He_end * .data$ambiant_pressure_end,
      # Compute equivalent aire depth (for nitrogen narcosis)
      EAD_start = EAD(.data$N2_start, .data$depth_start),
      EAD_end = EAD(.data$N2_end, .data$depth_end),
      # Initialize columns for tissue loadings
      N2_load_start = list(rep(0, 16)),
      N2_load_end = list(rep(0, 16)),
      He_load_start = list(rep(0, 16)),
      He_load_end = list(rep(0, 16)),
      # Initialize columns for tensions, M-values and percent gradients
      M_val = list(rep(0, 16)),
      tension = list(rep(0, 16)),
      percent_gradient = list(rep(0, 16))
    )
  
  return(dive_tbl)
  
}