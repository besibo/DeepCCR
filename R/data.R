#' A Dive Table with Information for All Segments of a Dive
#' 
#' This data set contains information for all segments of a dive before 
#' computations of decompression times. The parameters for this dive are as
#' follows: \code{max_depth} = 45 meters and \code{bottom_time} = 50 minutes,
#' \code{diluent} is air (21\% O2, 79\% N2, 0\% He), \code{penalty} is set to 3 
#' (the most conservative setting) and all other parameters are kept at their 
#' default value: \code{ppO2_low} = 0.7, \code{ppO2_high} = 1.3, 
#' \code{ppO2_switch_depth} = 15, \code{last_stop} = 4, \code{speed_desc} = 20,
#' \code{speed_asc} = 10, \code{ppO2_min} = 0.18, \code{ppO2_max} = 1.61.
#' 
#' @format ## `dive_tbl`
#' A Tibble with 88 rows and 32 columns:
#' \describe{
#'   \item{phase}{The phase of the dive (descent, bottom, or ascent)}
#'   \item{depth_start}{The depth (in meters) at the start of the segment} 
#'   \item{depth_end}{The depth (in meters) at the end of the segment}
#'   \item{time_start}{The dive time (in decimal minutes) at the start of the segment}
#'   \item{time_end}{The dive time (in decimal minutes) at the end of the segment}
#'   \item{duration}{The duration (in decimal minutes) of the segment}
#'   \item{ppO2_mix_start}{The partial pressure (in bar) of oxygen in the loop at the start of the segment. Sice CCR dives are done at a constant ppO2, this variable can only take 2 values: either \code{ppO2_low} or \code{ppO2_high} depending on the depth and phase of the segment}
#'   \item{ppO2_mix_end}{The partial pressure (in bar) of oxygen in the loop at the end of the segment. Sice CCR dives are done at a constant ppO2, this variable can only take 2 values: either \code{ppO2_low} or \code{ppO2_high} depending on the depth and phase of the segment}
#'   \item{ambiant_pressure_start}{The ambiant pressure (in bar) at the start of the segment}
#'   \item{ambiant_pressure_end}{The ambiant pressure (in bar) at the end of the segment}
#'   \item{O2_start}{The fraction of oxygen in the loop at the start of the segment}
#'   \item{O2_end}{The fraction of oxygen in the loop at the end of the segment}
#'   \item{N2_start}{The fraction of nitrogen in the loop at the start of the segment}
#'   \item{N2_end}{The fraction of nitrogen in the loop at the end of the segment}
#'   \item{He_start}{The fraction of helium in the loop at the start of the segment}
#'   \item{He_end}{The fraction of helium in the loop at the end of the segment}
#'   \item{ppN2_mix_start}{The partial pressure (in bar) of nitrogen in the loop at the start of the segment}
#'   \item{ppHe_mix_start}{The partial pressure (in bar) of helium in the loop at the start of the segment}
#'   \item{ppN2_mix_end}{The partial pressure (in bar) of nitrogen in the loop at the end of the segment}
#'   \item{ppHe_mix_end}{The partial pressure (in bar) of helium in the loop at the end of the segment}
#'   \item{EAD_start}{The equivalent air depth (in meters) at the start of the segment}
#'   \item{EAD_end}{The equivalent air depth (in meters) at the end of the segment}
#'   \item{N2_load_start}{The nitrogen loadings for each of the 16 compartments at the start of the segment. This variable is a list-column}
#'   \item{N2_load_end}{The nitrogen loadings for each of the 16 compartments at the end of the segment. This variable is a list-column}
#'   \item{He_load_start}{The helium loadings for each of the 16 compartments at the start of the segment. This variable is a list-column}
#'   \item{He_load_end}{The helium loadings for each of the 16 compartments at the end of the segment. This variable is a list-column}
#'   \item{M_val}{The M-values for each of the 16 compartments at the end of the segment. This variable is a list-column}
#'   \item{tension}{The total tension for each of the 16 compartments at the end of the segment. This variable is a list-column. The tension is simply the sum of the loadings for nitrogen and helium at the end of a segment}
#'   \item{percent_gradient}{The percent gradient for each of the 16 compartments at the end of the segment. This variable is a list-column}
#'   \item{leading_compartment}{The compartment with the highest tension at the end of the segment}
#'   \item{leading_tension}{The tension of the leading compartment at the end of the segment}
#'   \item{max_percent_gradient}{The maximum percent gradient among all compartments at the end of the segment}
#' }
#' @source
#' \code{
#' dive_table <- max_depth |> 
#'    create_dive_segments(bottom_time) |>
#'    compute_mix() |> 
#'    initialize_tissue_loadings() |> 
#'    compute_tissue_loadings() |> 
#'    deco_data() |> 
#'    leading_tissue()
#'    }
"dive_table"

