#' Compute the breathing gas mix for each dive segment and sets up the dive table for loadings computations
#' 
#' @param dive_segments A tibble with dive segments as produced by the function \code{\link{create_dive_segments}}
#' @param ppO2_low Double. The low setting for the partial pressure of oxygen
#' @param ppO2_high Double. The high setting for the partial pressure of oxygen
#' @param ppO2_switch_depth Double. The depth at which the partial pressure of oxygen switches from the low to the high setting
#' @param diluent A numeric vector of length 2 with the percentage of oxygen, and helium in the diluent
#' @return A tibble with the composition of the breathing gas at the start and end of each segment, the partial pressures of oxygen nitrogen and helium at the start and end of each segment, the equivalent air depth at the start and end of each segment, and columns for tissue loadings, tensions, M-values and percent gradients
compute_mix <- function(dive_segments,
                        ppO2_low = 0.7,
                        ppO2_high = 1.3,
                        ppO2_switch_depth = 15,
                        diluent = c(21, 0)) {
  dive_tbl <- dive_segments |>
    mutate(
      # Duration of each segment
      duration = time_end - time_start,
      # Set appropriate ppO2 for each segment
      ppO2_mix_start = if_else(
        phase == "descent" & depth_start < ppO2_switch_depth |
        phase == "ascent" & depth_start <= 3,
        ppO2_low,
        ppO2_high
      ),
      ppO2_mix_end = if_else(
        phase == "descent" & depth_end < ppO2_switch_depth |
        phase == "ascent" & depth_end <= 3,
        ppO2_low,
        ppO2_high
      ),
      # Compute ambiant pressures at the start and end of each segment
      ambiant_pressure_start = depth_start / 10 + 1,
      ambiant_pressure_end   = depth_end / 10 + 1,
      # Composition of the breathing gas at the start and end of each segment
      mix_start  = map2(ppO2_mix_start, depth_start, ~ loop_mix(diluent, .x, .y)),
      mix_end    = map2(ppO2_mix_end, depth_end, ~ loop_mix(diluent, .x, .y))
    ) |>
    unnest_wider(c(mix_start, mix_end), names_sep = "_") |>
    # Rename columns for the compistion of the breathing gas
    rename(
      O2_start = mix_start_1,
      N2_start = mix_start_2,
      He_start = mix_start_3,
      O2_end   = mix_end_1,
      N2_end   = mix_end_2,
      He_end   = mix_end_3
    ) |>
    # Partial pressures for inert gases at the start and end of each segment
    mutate(
      ppN2_mix_start = N2_start * ambiant_pressure_start,
      ppHe_mix_start = He_start * ambiant_pressure_start,
      ppN2_mix_end   = N2_end * ambiant_pressure_end,
      ppHe_mix_end   = He_end * ambiant_pressure_end,
      # Compute equivalent aire depth (for nitrogen narcosis)
      EAD_start = EAD(N2_start, depth_start),
      EAD_end = EAD(N2_end, depth_end),
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