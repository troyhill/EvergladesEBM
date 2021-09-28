#' @title Quantifies hydroperiod (continuous or discontinuous)
#'
#' @description Calculates the duration of inundation, either continuous or discontinuous
#' 
#' @param data       input dataset (e.g., one year of stage data for a point/pixel)
#' @param threshold  threshold value for determining inundation. Note: values equal to or greater than the threshold count as inundated.
#' @param continuous if TRUE, the quantity calculated is continuous hydroperiod (largest run of consecutive days inundated). If `FALSE`, the function returns discontinuous hydroperiod (total days inundated in record)
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



hydroperiod <- function(x, threshold = 0, continuous = TRUE) {
  
  if (continuous) {
    newDat     <- logical(length = length(x))
    newDat[x >= threshold] <- TRUE 
    tmpDat     <- rle(newDat)
    targetRuns <- tmpDat$lengths[tmpDat$values == TRUE] # lengths of runs with values above threshold
    
    if (sum(newDat, na.rm = TRUE) > 0) { # for cases where there are no TRUE values
      outDat   <- max(targetRuns, na.rm = TRUE)
    } else {
      outDat   <- 0
    } 
  }
  if (!continuous) {
    outDat     <- length(which(x[!is.na(x)] >= threshold)) # exclude NAs
  }
  invisible(outDat)
}
