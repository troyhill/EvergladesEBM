#' @title Generate a three-color gradient palette
#'
#' @description Generate a three-color gradient palette
#' 
#' @param uniqueValues    input values (or vector with max/min)
#' @param binSize         size of increment between color gradations
#' @param colorGradient   vector with three colors that form the gradient
#' @param threshold       value of gradient transition. This value will be the middle color listed in `colorGradient`
#' @param type            type of scale to be produced: 'continuous' (equally-sized intervals) or 'interval' (unequal intervals).
#' 
#' @return A list with color codes and breaks
#' 
#' 
#' @examples
#' getColors(uniqueValues = range(-10:10), binSize = 5)
#' getColors(uniqueValues = range(-10:10), binSize = 3)
#' getColors(uniqueValues = range(0:100), binSize = 5, threshold = 50) # odd behavior
#' getColors(uniqueValues = range(0:99), binSize = 5, threshold = 50)
#' getColors(uniqueValues = range(-13:67)/100, binSize = 0.025)
#' 
#' ### lop-sided categories with unequal sizes
#' getColors(uniqueValues = c(-365,-30, 0, 10, 30, 60, 365, 400, 700),
#'  type = 'interval', binSize = 5, threshold = 0)
#' 
#' f <- system.file("ex/elev.tif", package="terra") 
#' r <- rast(f)
#' plot(r)
#' 
#' 
#' cols <- getColors(uniqueValues = c(minmax(r)), binSize = 40, 
#'         threshold = mean(c(minmax(r))))
#' plot(r, breaks = cols$key, col = cols$colors)   
#' 
#' ### example with five colors
#' cols <- getColors(uniqueValues = c(minmax(r)), binSize = 20, 
#'    threshold = mean(c(minmax(r))) 
#'    colorGradient = c("red", "yellow", "white", "green", "blue"))
#' plot(r, col = cols$colors, type = 'continuous', breaks = cols$key)
#' 
#' cols <- getColors(uniqueValues = c(100, 195, 200, 205, 300, 350, 450, 550), 
#'         binSize = 40, # binSize argument is irrelevant if type = 'interval'
#'         threshold = 200, type = 'interval')
#' plot(r, breaks = cols$key, col = cols$colors)   
#' 
#' @importFrom grDevices colorRampPalette
#'  
#' @export



getColors <- function(uniqueValues = c(-2:4)/10, # feet per week, e.g.
                    binSize = 0.025,   # for asymmetries: https://stackoverflow.com/a/29280215
                    colorGradient = c("red", "white", "blue"),
                    threshold     = 0,
                    type = 'continuous') {
  if(!grepl(x = tolower(type), pattern = 'continuous|interval')) {
    stop("'type' argument isn't valid. Acceptable inputs: 'continuous' or 'interval'\n")
  }
  breaks <- uniqueValues
  if (grepl(x = tolower(type), pattern = "^interval$")) {
    center_index <- which.min(abs(breaks- threshold))
    # breaks_unindexed <- -floor((length(breaks))/2):ceiling((length(breaks)-3)/2)
    breaks_indexed <- 1:(length(breaks)-1) - center_index
    
    rampcols <- getColors(uniqueValues = breaks_indexed, 
                           threshold = 0, 
                          colorGradient = colorGradient,
                           type      = 'continuous',
                           binSize   = 1)$colors
    rampbreaks <- breaks
  } else if (grepl(x = tolower(type), pattern = "^continuous$")) {
    
  sigfig_digits <- nchar(sapply(X = strsplit(as.character(binSize), "\\."), FUN = '[', 2))
  if (is.na(sigfig_digits)) {
    sigfig_digits <- 1
  }
  
  round.choose <- function(x, roundTo, dir = 1) {
    convertBack <- FALSE
    if (roundTo < 1) {
      x <- x * 1000
      roundTo <- roundTo * 1000
      convertBack <- TRUE
    }
    if(dir == 1) {  ##ROUND UP
      ans <- x + (roundTo - x %% roundTo) # original
      # ans <- x + (roundTo %% roundTo) # modified
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
  
  uniqueValues_new <- round(x = seq(from = Min,
                                    to   = Max,
                                    by   = binSize), 
                            digits = sigfig_digits)
  isOdd <- length(uniqueValues_new) %% 2
  if ((isOdd == 0) & !(threshold %in% uniqueValues_new)) { #  
    uniqueValues_new <- c(uniqueValues_new, max(uniqueValues_new) + binSize)
  }
  
  Thresh <- threshold
  nHalf  <- length(uniqueValues_new)/2
  valsAboveThreshold <- length(uniqueValues_new[uniqueValues_new > Thresh])
  valsBelowThreshold <- length(uniqueValues_new[uniqueValues_new < Thresh])
  Min <- min(uniqueValues_new, na.rm = TRUE)
  Max <- max(uniqueValues_new, na.rm = TRUE)
  
  ### define breaks
  rb1 <- seq(from =  Min, to = Thresh, by = binSize) # length.out=nHalf+1)
  if ((Thresh + binSize) < Max) {
    rb2 <- seq(Thresh + binSize, Max, by = binSize) # length.out=nHalf+1
  } else {
    rb2 <- Max
  }
  
  ### define colors
  if (length(colorGradient) == 3) {
    ## Make vector of colors for values below threshold
    rc1 <- grDevices::colorRampPalette(colors = c(colorGradient[1], colorGradient[2]), space="Lab")(valsBelowThreshold)    
    ## Make vector of colors for values above threshold
    rc2 <- grDevices::colorRampPalette(colors = c(colorGradient[2], colorGradient[3]), space="Lab")(valsAboveThreshold)
    ## In your example, this line sets the color for values between 49 and 51. 
    # rampcols[nHalf] = rgb(t(col2rgb("white")), maxColorValue=256) 
    
  } else if (length(colorGradient) == 5) {
    cols_p1 <- getColors(uniqueValues = rb1,
              binSize = binSize,
              threshold = mean(rb1), # median?
              colorGradient = colorGradient[1:3])
    cols_p2 <- getColors(uniqueValues = rb2,
                         binSize = binSize,
                         threshold = mean(rb2),
                         colorGradient = colorGradient[3:5])
    rc1 <- cols_p1$colors
    rc2 <- cols_p2$colors
  } else {
      stop("`colorGradient` is limited to three or five colors.\n")
  }
  rampcols   <- c(rc1, rc2)
  rampbreaks <- c(rb1, rb2)
  }
  list(key    = rampbreaks, 
       colors = rampcols)
}


