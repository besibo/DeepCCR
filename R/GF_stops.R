#' I don't know what this function does, but it does it good...
#'
#' @param a numeric vector of Bühlmann coefficients (length 16)
#' @param b numeric vector of Bühlmann coefficients (length 16)
#' @param ppart numeric vector of partial pressures (length 2)
#' @param GF numeric value of gradient factor
#' @param prof.max Double. Maximum depth

GF_stops <- function(a, b, ppart, GF, prof.max) {
  
  coef.a <- (a[, 2] * ppart[, 2] + a[, 1] * ppart[, 1]) / apply(ppart, 1, sum)
  coef.b <- (b[, 2] * ppart[, 2] + b[, 1] * ppart[, 1]) / apply(ppart, 1, sum)
  
  charge.inerte <- apply(ppart, 1, sum)
  
  pression.toleree <- (charge.inerte - coef.a * GF) / (GF / coef.b - GF + 1)
  
  if (any(pression.toleree < 0)) {
    pression.toleree[which(pression.toleree<0)] <- 0
  }
  
  stops <- seq(0, prof.max,3)
  out <- list(min(stops[stops > max(pression.toleree - 10)]), pression.toleree)
  out
}
