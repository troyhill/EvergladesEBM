#' @title Generate a three-color gradient palette
#'
#' @description Converts feet to centimeters
#' 
#' @param uniqueValues    input values (or vector with max/min)
#' @param binSize         size of increment between color gradations
#' @param colorGradient   vector with three colors that form the gradient
#' @param threshold       value of gradient transition. This value will be the middle color listed in `colorGradient`
#' 
#' @return A list with color codes and breaks
#' 
#' 
#' @examples
#' getColors(uniqueValues = range(-10:10), binSize = 5)
#' getColors(uniqueValues = range(-10:10), binSize = 3)
#' getColors(uniqueValues = range(-10:10), binSize = 3)
#' getColors(uniqueValues = range(-13:67)/100, binSize = 0.025)
#' 
#' @importFrom grDevices colorRampPalette
#'  
#' @export



getColors <- function(uniqueValues = c(-2:4)/10, # feet per week, e.g.
                    binSize = 0.025,   # for asymmetries: https://stackoverflow.com/a/29280215
                    colorGradient = c("red", "white", "blue"),
                    threshold     = 0) {
  # round.choose <- function(x, roundTo, dir = 1) {
  #   ### function reproduces plyr::round_any
  #   ### modified from https://stackoverflow.com/a/32508105/3723870
  #   if(dir == 1) {  ##ROUND UP
  #     x + (roundTo %% roundTo)
  #   } else {
  #     if(dir == 0) {  ##ROUND DOWN
  #       x - (x %% roundTo)
  #     }
  #   }
  # }
  round.choose <- function(x, roundTo, dir = 1) {
    convertBack <- FALSE
    if (roundTo < 1) {
      x <- x * 1000
      roundTo <- roundTo * 1000
      convertBack <- TRUE
    }
    if(dir == 1) {  ##ROUND UP
      ans <- x + (roundTo - x %% roundTo)
    } else {
      if(dir == 0) {  ##ROUND DOWN
        ans <- x - (x %% roundTo)
      }
    }
    if (convertBack) {
      ans <- ans / 1000
    }
    return(ans)
  }
  ### round max and min values to highest/lowest increment of binSize
  # Min <- plyr::round_any(min(uniqueValues, na.rm = TRUE), accuracy = binSize, f = floor)
  # Max <- plyr::round_any(max(uniqueValues, na.rm = TRUE), accuracy = binSize, f = ceiling)
  Min <- round.choose(x = min(uniqueValues, na.rm = TRUE), roundTo = binSize, dir = 0)
  Max <- round.choose(x = max(uniqueValues, na.rm = TRUE), roundTo = binSize, dir = 1)
  
  uniqueValues <- seq(from = Min,
                      to   = Max,
                      by   = binSize)
  isOdd <- length(uniqueValues) %% 2
  if ((isOdd == 0) & !(0 %in% uniqueValues)) {
    uniqueValues <- c(uniqueValues, max(uniqueValues) + binSize)
  }
  
  Thresh <- threshold
  nHalf  <- length(uniqueValues)/2
  valsAboveThreshold <- length(uniqueValues[uniqueValues > Thresh])
  valsBelowThreshold <- length(uniqueValues[uniqueValues < Thresh])
  Min <- min(uniqueValues, na.rm = TRUE)
  Max <- max(uniqueValues, na.rm = TRUE)
  
  ## Make vector of colors for values below threshold
  rc1 <- grDevices::colorRampPalette(colors = c(colorGradient[1], colorGradient[2]), space="Lab")(valsBelowThreshold)    
  ## Make vector of colors for values above threshold
  rc2 <- grDevices::colorRampPalette(colors = c(colorGradient[2], colorGradient[3]), space="Lab")(valsAboveThreshold)
  rampcols <- c(rc1, rc2)
  ## In your example, this line sets the color for values between 49 and 51. 
  # rampcols[nHalf] = rgb(t(col2rgb("white")), maxColorValue=256) 
  
  rb1 <- seq(from =  Min, to = Thresh, by = binSize) # length.out=nHalf+1)
  if (Thresh + binSize > Max) {
    rb2 <- seq(Thresh + binSize, Max, by = binSize) # length.out=nHalf+1
  } else {
    rb2 <- Max
  }
  rampbreaks = c(rb1, rb2)
  
  list(key    = rampbreaks, 
       colors = rampcols)
}

