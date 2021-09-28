#' @title Quantifies soil oxidation
#'
#' @description Calculates soil oxidation performance measure. Accumulates length units when stages are below a threshold value.
#' 
#' @param data       input dataset (e.g., one year of stage data for a point/pixel)
#' @param threshold  threshold is the value *below which* depths are accumulated. Values equal to the threshold are treated as inundated.
#' 
#' @return numeric \code{oxidation} returns a numeric value with time units equivalent to the time increments in input data
#' 
#' 
#' @examples
#' dat <- 1:100
#' oxidation(data = dat, threshold = 75)
#' \dontrun{
#' }
#' 
#'  
#' @export



oxidation <- function(data, threshold = 0) { 
  # data must be depths relative to sediment surface
  # threshold is the value *below which* depths are accumulated. This excludes the threshold ("<" is used, not "<=")
  # for days where water is below the surface, 
  # get the difference between stage and sediment surface (depth to water), 
  # and get cumulative sum. Example: if water stage is 10 cm below the surface for 5 days, the value would be 50 cm.
  
  newDat <- data[(data < threshold) & !is.na(data)]
  if (length(newDat) == 0) { # this accommodates datasets that don't fall below the threshold
    outDat <- 0
  } else {
    outDat <- sum(abs(newDat), na.rm = TRUE) # depths will be negative, so abs() makes that positive
  }
  invisible(outDat) # length (e.g., cm if one measurement per day; no accounting for time)
} 

