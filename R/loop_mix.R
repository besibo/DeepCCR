#' Compute the mix of gases in the loop to achieve a desired ppO2 at a given depth
#' 
#' @param diluent A numeric vector of length 3 with the fraction of O2, N2, and He in the diluent
#' @param ppO2 Double. The desired ppO2 at the depth
#' @param depth Double. The depth in meters

#' @return A numeric vector of length 3 with the fraction of O2, N2, and He in the mix
#' @export
#' 
#' @examples
#' loop_mix(c(0.21, 0), 1.3, 45)
loop_mix <- function(diluent, ppO2, depth) {
  # Switch diluent from percentages to fractions and add the missing gas (N2)
  diluent <- diluent / 100
  diluent <- c(diluent[1], 1- sum(diluent), diluent[2])
  
  # Ambient pressure
  ambient_pressure <- depth / 10 + 1
  
  # Compute the ppO2 at the depth using only the diluent
  ppO2_diluent <- diluent[1] * ambient_pressure
  
  if (ppO2_diluent > ppO2) {
    stop(paste0("ppO2 at depth (", ppO2_diluent, ") is higher than desired ppO2"))
  }
  
  # Compute the disered percentage of oxygen at the depth
  oxygen_desired <- ppO2 / ambient_pressure
  
  # How much O2 do I need to add?
  oxygen_to_add <- oxygen_desired - diluent[1]
  
  # Ratios of N2 and He if we dont consider O2
  diluent_no_oxygen <- diluent[2:3] / sum(diluent[2:3])
  
  # new fraction of gases with the added oxygen
  new_diluent <- c(diluent[1] + oxygen_to_add, 
                   diluent[2:3] - oxygen_to_add * diluent_no_oxygen)
  
  return(new_diluent)
}
