#' @title Quantifies hydroperiod (continuous or discontinuous)
#'
#' @description Calculates the duration of inundation, either continuous or discontinuous
#' 
#' @param data       input dataset (e.g., one year of stage data for a point/pixel)
#' @param threshold  threshold value for determining inundation. Note: values equal to or greater than the threshold count as inundated.
#' @param continuous if TRUE, the quantity calculated is continuous hydroperiod (largest run of consecutive days inundated). If `FALSE`, the function returns discontinuous hydroperiod (total days inundated in record)
#' @param percent    if TRUE, returned value is expressed as a fraction of all non-NA observations. This may not be meaningful in all cases (e.g., continuous hydroperiod)
#' 
#' @return numeric \code{hydroperiod} returns a numeric value with time units equivalent to the time increments in input data
#' 
#' 
#' @examples
#' dat <- 1:100
#' hydroperiod(data = dat, threshold = 75, continuous = TRUE)
#' \dontrun{
#' }
#' 
#'  
#' @export



hydroperiod <- function(x, threshold = 0, continuous = TRUE, percent = FALSE) {
  
  if (continuous) {
    newDat     <- logical(length = length(x))
    newDat[x >= threshold] <- TRUE 
    tmpDat     <- rle(newDat)
    targetRuns <- tmpDat$lengths[tmpDat$values == TRUE] # lengths of runs with values above threshold
    
    if (sum(newDat, na.rm = TRUE) > 0) { # for cases where there are no TRUE values
      outDat      <- max(targetRuns, na.rm = TRUE)
      numberOfObs <- sum(!is.na(x))
    } else {
      outDat      <- 0
      numberOfObs <- 0
    } 
  }
  if (continuous == FALSE) { # for discontinuous hydroperiod
    outDat      <- length(which(x[!is.na(x)] >= threshold)) # exclude NAs
    numberOfObs <- sum(!is.na(x))
  }
  if (percent == TRUE) {
    outDat     <- outDat / numberOfObs
  }
  invisible(outDat)
}
