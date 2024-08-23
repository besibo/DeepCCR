#' Percentage of over-saturation in each compartment of Bühlmann' ZHL16-C model
#'
#' @param tension numeric. The tension in the compartment (sum of the ppart of each gas)
#' @param depth numeric. The depth in meters
#' @param M_val numeric vector of M-values (16 values)
#' 
#' @return numeric. The percentage of over-saturation for each compartment
#' @export
#' 
#' @details
#' Given the M-values, depth, and inert gas partial pressures in each compartment, this function calculates the percentage of the gradient achieved through saturation. In other words, it determines how close the compartment tensions are to the M-values, expressed as a percentage of the gradient from ambient pressure.
#' 
#' @examples
#' first_seg <- dplyr::first(dive_table)
#' last_seg <- dplyr::last(dive_table)
#' percent_gradient(first_seg$tension[[1]],
#'                 first_seg$depth_end[1],
#'                 first_seg$M_val[[1]])
#' percent_gradient(last_seg$tension[[1]], 
#'                  last_seg$depth_end[1], 
#'                  last_seg$M_val[[1]])
#'                  
percent_gradient <- function(tension, depth, M_val) {
  ambiant_pressure <- depth + 10
  out <- (tension - ambiant_pressure) * 100 / (M_val - ambiant_pressure)
  return(out)
}
