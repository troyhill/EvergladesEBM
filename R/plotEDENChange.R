#' @title Calculate ascension/recession rates over some time period
#'
#' @description Calculates two-point ascension/recession rates from EDEN data. Produces a map, and two verisons of the raster output (one with rates of change, and one with rates categorized into poor/fair/good)
#' 
#' @param EDEN_date    Date (format = '\%Y-\%m-\%d') at end of recession rate calculation.
#' @param changePeriod Time period (units = weeks) over which stage changes are measured. Default is two weeks.
#' @param poor Vector of paired values identifying 'poor' ascension/recession rates. Units must be feet/week. Pairs must have the lower value first, e.g., c(0, Inf, -Inf, -1.8) 
#' @param fair Vector of paired values identifying 'fair' ascension/recession rates. Units must be feet/week. Pairs must have the lower value first
#' @param good Vector of paired values identifying 'good' ascension/recession rates. Units must be feet/week. Pairs must have the lower value first.
#' @param other Vector of paired values for an additional category (optional)
#' @param otherName Legend entry for the additional category (optional)
#' @param otherColor Color to be used for additional category (optional)
#' @param plotOutput If the produced plot should be saved, use this argument to set the filename (include any extension, e.g. "plot.png").
#' @param addToPlot If you'd like an SPDF added to the plot, pass it to this argument.
#' @param maskPlot If set to TRUE, the spdf in addToPlot is used to mask and crop the data. This is useful if a small area (e.g., WCA3A) is of interest.
#' 
#' @return list \code{plotEDENChange} returns a list with the calculated rates (stageChange; units are inches/week), rates categorized into poor/fair/good (categories), a description of the time period used (description), and the criteria used to assign categories (criteria; units are inches/week) 
#' 
#' 
#' @examples
#' \dontrun{
#' ### by default, the most recent one-week period in EDEN is used
#' plotOut <- plotEDENChange(addToPlot = sfwmd.shp)
#' ylim.range <- max(abs(floor(cellStats(plotOut$stageChange, min))), 
#' abs(ceiling(cellStats(plotOut$stageChange, max))))
#' 
#' spplot(plotOut$stageChange, main = plotOut$description, # inches/week
#' col.regions=colorRampPalette(c('red', 'white', 'blue'))(100),
#' at = seq(-ylim.range, ylim.range, by = 0.5))  + 
#' latticeExtra::layer(sp.polygons(sfwmd.shp, fill = NA))
#' 
#' ### identify recession rates at specific locations
#' extract(plotOut$stageChange, bay.coords, fun = mean)
#' extract(plotOut$categories, bay.coords, fun = mean)
#' }
#' 
#' @importFrom fireHydro getEDEN
#' @importFrom raster reclassify
#' @importFrom raster rasterToPolygons
#' @importFrom raster plot
#' @importFrom raster crop
#' @importFrom raster crs
#' @importFrom graphics legend
#' @importFrom sp spTransform
#' @importFrom graphics par
#' @importFrom grDevices png
#' @importFrom grDevices dev.off
#'  
#' @export



plotEDENChange <- function(EDEN_date    = Sys.Date(), # format = '%Y-%m-%d'
                         changePeriod = 1, # units = weeks
                         poor = c(c(0, Inf), c(-Inf, -0.18)), # inches/week
                         fair = c(c(-0.01, 0), c(-0.18, -0.05)),
                         good = c(-0.05, -0.01),
                         other = NA,     # values for an additional category
                         otherName = "other", # legend name for additional category
                         otherColor = "darkred", # color to apply to additional category
                         plotOutput = NULL, # NULL or filename
                         addToPlot = NA, # optional spdf to add to plot. TODO: accept a list of spdfs
                         maskPlot  = FALSE  # If TRUE, raster data are clipped/masked using spdf in addToPlot[1]
) {
  
  colorNames <- c("red", "yellow", "green")
  if (!all(is.na(other))) { 
    colorNames <- c(colorNames, otherColor)
  }
  categoryNames <- c("poor", "fair", "good")
  if (!all(is.na(other))) { 
    categoryNames <- c(categoryNames, otherName)
  }
  
  ### pull EDEN data
  EDEN_date1  <- gsub(x = EDEN_date, pattern = "-", replacement = "")
  EDEN_date2 <- gsub(x = EDEN_date - changePeriod*7, pattern = "-", replacement = "")
  
  eden1 = fireHydro::getEDEN(EDEN_date = EDEN_date1,  returnType = "raster")
  eden2 = fireHydro::getEDEN(EDEN_date = EDEN_date2,  returnType = "raster")
  
  ### get time period in weeks
  timePeriod <- as.numeric(as.Date(eden1$date, format = "%Y%m%d") - as.Date(eden2$date, format = "%Y%m%d")) / 7 
  
  ### plot prep: create EDEN boundary, set title, etc.
  mainTitle <- paste("Recession/ascension rate categories \n", as.Date(eden2$date, format = "%Y%m%d"), "to", as.Date(eden1$date, format = "%Y%m%d"))
  r <- raster::reclassify(eden2$data, cbind(-Inf, Inf, 1))
  # convert to polygons (you need to have package 'rgeos' installed for this to work)
  pp <- raster::rasterToPolygons(r, dissolve=TRUE)
  
  ### get recession rates in feet per week (cm / 2.54 / 12 /wks)
  recRates <- (eden1$data - eden2$data) / 2.54 / 12 / timePeriod # positive = ascension
  
  if (maskPlot == TRUE) {
    recRates <- crop(x = recRates, y    = addToPlot)
    recRates <- mask(x = recRates, mask = addToPlot)
  } 
  
  
  # reclassify the values into three groups 
  pvals <- matrix(poor, nrow = length(poor) / 2, byrow = TRUE)
  fvals <- matrix(fair, nrow = length(fair) / 2, byrow = TRUE)
  gvals <- matrix(good, nrow = length(good) / 2, byrow = TRUE)
  
  ### add in label. gotta be a more efficient way
   pvals <- cbind(pvals, 1)
   fvals <- cbind(fvals, 2)
   gvals <- cbind(gvals, 3)
  
  # all values > 0 and <= 0.25 become 1, etc.
  m      <- do.call(rbind, list(pvals, fvals, gvals))
  if (!all(is.na(other))) {
    othervals <- matrix(other, nrow = length(other) / 2, byrow = TRUE)
    othervals <- cbind(othervals, 4)
    m      <- do.call(rbind, list(pvals, fvals, gvals, othervals))
  }
  rclmat <- matrix(m, ncol=3, byrow=FALSE)
  
  recRatesReclassed <- raster::reclassify(recRates, rclmat)
  
  ### remove any missing categories or plot will be messed up
  colorNames    <- colorNames[unique(recRatesReclassed)]
  categoryNames <- categoryNames[unique(recRatesReclassed)]
  
  # plot reclassified data
  if (!is.null(plotOutput)) {
    grDevices::png(plotOutput, width = 5, height = 6, units = "in", res = 150)
    graphics::par(mar = c(0.5, 0.5, 3.25, 0.5))
  }
  raster::plot(recRatesReclassed,
       legend = FALSE,
       col = colorNames, axes = FALSE, box=FALSE,
       main = mainTitle)
  graphics::legend("topleft",
         legend = categoryNames,
         fill = colorNames,
         border = FALSE,
         bg = "white", 
         bty = "n"
  ) # turn off legend border
  raster::plot(pp, add = TRUE)
  if (grepl(x = class(addToPlot), pattern = "SpatialPolygonsDataFrame|SpatialPointsDataFrame")) {
    addToPlot <- sp::spTransform(addToPlot, raster::crs(fireHydro::edenDEM))
    addToPlot <- raster::crop(x = addToPlot, y = recRatesReclassed)
    raster::plot(addToPlot, add = TRUE)
  }
  if (!is.null(plotOutput)) {
    grDevices::dev.off()
  }
  
  ### return object that can be queried
  ### e.g., to identify recession rates at specific locations:
  ### extract(object$stageChange, bay.coords, fun = mean)
  
  return(list(stageChange = recRates, 
              categories  = recRatesReclassed,
              description = mainTitle,
              criteria    = rclmat)) # in 3rd column, 1 = poor, 2 = fair, 3 = good
}
