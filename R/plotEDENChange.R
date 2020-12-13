#' @title Calculate ascension/recession rates over some time period
#'
#' @description Calculates two-point ascension/recession rates from EDEN data. Produces a map, and two verisons of the raster output (one with rates of change, and one with rates categorized into poor/fair/good)
#' 
#' @param EDEN_date    Date (format = '\%Y-\%m-\%d') at end of recession rate calculation.
#' @param changePeriod Time period (units = weeks) over which stage changes are measured. Default is two weeks.
#' @param poor Vector of paired values identifying 'poor' ascension/recession rates. Units must be inches/week. Pairs must have the lower value first, e.g., c(0, Inf, -Inf, -1.8) 
#' @param fair Vector of paired values identifying 'fair' ascension/recession rates. Units must be inches/week. Pairs must have the lower value first
#' @param good Vector of paired values identifying 'good' ascension/recession rates. Units must be inches/week. Pairs must have the lower value first.
#' @param plotOutput If the produced plot should be saved, use this argument to set the filename (include any extension, e.g. "plot.png").
#' @param addToPlot If you'd like an SPDF added to the plot, pass it to this argument.
#' 
#' @return list \code{plotEDENChange} returns a list with the calculated rates (stageChange; units are inches/week), rates categorized into poor/fair/good (categories), a description of the time period used (description), and the criteria used to assign categories (criteria; units are inches/week) 
#' 
#' 
#' @examples
#' \dontrun{
#' ### by default, the most recent two-week period in EDEN is used
#' plotOut <- plotWLChange(addToPlot = sfwmd.shp)
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
                         changePeriod = 2, # units = weeks
                         poor = c(c(0, Inf), c(-Inf, -0.18*12)), # inches/week
                         fair = c(c(-0.01*12, 0), c(-0.18*12, -0.05*12)),
                         good = c(-0.05*12, -0.01*12),
                         plotOutput = NULL, # NULL or filename
                         addToPlot = NA # optional spdf to add to plot
) {
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
  
  ### get recession rates in inches per week
  recRates <- (eden1$data - eden2$data) / 2.54 / timePeriod # positive = ascension
  # reclassify the values into three groups 
  pvals <- matrix(poor, nrow = length(poor) / 2)
  pvals <- c(rbind(pvals, 1))# "poor"))
  fvals <- matrix(fair, nrow = length(fair) / 2)
  fvals <- c(rbind(fvals, 2))# "fair"))
  gvals <- matrix(good, nrow = length(poor) / 2)
  gvals <- c(rbind(gvals, 3))# "good"))
  
  # all values > 0 and <= 0.25 become 1, etc.
  m      <- c(pvals, fvals, gvals)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  
  recRatesReclassed <- raster::reclassify(recRates, rclmat)
  
  # plot reclassified data
  if (!is.null(plotOutput)) {
    grDevices::png(plotOutput, width = 5, height = 6, units = "in", res = 150)
    graphics::par(mar = c(0.5, 0.5, 3.25, 0.5))
  }
  raster::plot(recRatesReclassed,
       legend = FALSE,
       col = c("red", "yellow", "green"), axes = FALSE, box=FALSE,
       main = mainTitle)
  graphics::legend("topleft",
         legend = c("poor", "fair", "good"),
         fill = c("red", "yellow", "green"),
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
