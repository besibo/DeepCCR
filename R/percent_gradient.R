#' Given the M-values, depth, and inert gas partial pressures in each compartment, this function calculates the percentage of the gradient achieved through saturation. In other words, it determines how close the compartment tensions are to the M-values, expressed as a percentage of the gradient from ambient pressure.
#'
#' @param tension numeric. The tension in the compartment (sum of the ppart of each gas)
#' @param depth numeric. The depth in meters
#' @param M_val numeric vector of M-values (16 values)
percent_gradient <- function(tension, depth, M_val) {
  ambiant_pressure <- depth + 10
  out <- (tension - ambiant_pressure) * 100 / (M_val - ambiant_pressure)
}
