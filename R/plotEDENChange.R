#' @title Calculate ascension/recession rates over some time period
#'
#' @description Calculates two-point ascension/recession rates from EDEN data. Produces a map, and two versions of the SpatRaster output (one with rates of change, and one with rates categorized into poor/fair/good)
#' 
#' @param EDEN_end    EDEN list (data/date; see `fireHydro::getEDEN`` for more details) for the end of the recession calculation period.
#' @param EDEN_begin  EDEN list (data/date; see `fireHydro::getEDEN`` for more details) for the beginning of the recession calculation period.
#' @param changePeriod Deprecated. Time period (units = days) over which stage changes are measured. Default is seven days (e.g., Sunday-Sunday).
#' @param poor Vector of paired values identifying 'poor' ascension/recession rates. Units must be feet/week. Pairs must have the lower value first, e.g., c(0, Inf, -Inf, -1.8) 
#' @param fair Vector of paired values identifying 'fair' ascension/recession rates. Units must be feet/week. Pairs must have the lower value first
#' @param good Vector of paired values identifying 'good' ascension/recession rates. Units must be feet/week. Pairs must have the lower value first.
#' @param other Vector of paired values for an additional category (optional)
#' @param otherName Legend entry for the additional category (optional)
#' @param otherColor Color to be used for additional category (optional)
#' @param plotOutput If the produced plot should be saved, use this argument to set the filename (include any extension, e.g. "plot.png").
#' @param addToPlot If you'd like an `SpatVector` added to the plot, pass it to this argument.
#' @param maskPlot If set to `TRUE`, the `SpatVector` in `addToPlot` is used to mask and crop the data. This is useful if a small area (e.g., WCA3A) is of interest.
#' @param download.method Argument used in `getEDEN()` call. Method to be used for downloading files. See options in `utils::download.file`
#' 
#' @return list \code{plotEDENChange} returns a list with the calculated rates (stageChange; units are feet/week), rates categorized into poor/fair/good (categories), a description of the time period used (description), and the criteria used to assign categories (criteria; units are feet/week) 
#' 
#' 
#' @examples
#' \dontrun{
#' ### pull some eden data
#' eden1 <- fireHydro::getEDEN(Sys.Date(), returnType = 'terra')
#' eden2 <- fireHydro::getEDEN(Sys.Date() - 8, returnType = 'terra')
#' 
#' plotOut <- plotEDENChange(EDEN_begin = eden2, EDEN_end = eden1, 
#'                            addToPlot = sfwmd.shp)
#' ylim.range <- max(abs(floor(cellStats(plotOut$stageChange, min))), 
#' abs(ceiling(cellStats(plotOut$stageChange, max))))
#' 
#' spplot(plotOut$stageChange, main = plotOut$description, # feet/week
#' col.regions=colorRampPalette(c('red', 'white', 'blue'))(100),
#' at = seq(-ylim.range, ylim.range, by = 0.5))  + 
#' latticeExtra::layer(sp.polygons(sfwmd.shp, fill = NA))
#' 
#' ### identify recession rates at specific locations
#' extract(plotOut$stageChange, bay.coords, fun = mean)
#' extract(plotOut$categories, bay.coords, fun = mean)
#' }
#' 
#' @importFrom terra rast
#' @importFrom terra vect
#' @importFrom terra classify
#' @importFrom terra as.polygons
#' @importFrom terra plot
#' @importFrom terra crop
#' @importFrom terra mask
#' @importFrom terra values
#' @importFrom terra crs
#' @importFrom terra unique
#' @importFrom graphics legend
#' @importFrom terra project
#' @importFrom graphics par
#' @importFrom grDevices png
#' @importFrom grDevices dev.off
#'  
#' @export



plotEDENChange <- function(EDEN_end    = Sys.Date() - as.numeric(format(Sys.Date(),"%w")), # format = '%Y-%m-%d'
                           EDEN_begin   = NULL,
                           changePeriod = 7, # units = days
                           poor = c(c(0, Inf), c(-Inf, -0.18)), # feet/week
                           fair = c(c(-0.01, 0), c(-0.18, -0.05)),
                           good = c(-0.05, -0.01),
                           other = NA,     # values for an additional category
                           otherName = "other", # legend name for additional category
                           otherColor = "darkred", # color to apply to additional category
                           plotOutput = NULL, # NULL or filename
                           addToPlot = NA, # optional spatVector to add to plot. TODO: accept a list of spatVectors
                           maskPlot  = FALSE,  # If TRUE, raster data are clipped/masked using spatVector in addToPlot[1]
                           download.method = "libcurl" # passed to getEDEN
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
  if (!any(grepl(x = class(EDEN_end), pattern = 'eden|list'))) {
    stop('EDEN data are required. ')
    # EDEN_date1  <- gsub(x = EDEN_end, pattern = "-", replacement = "")
    # # eden1 <- fireHydro::getEDEN(EDEN_date = EDEN_date1,  returnType = "raster",  download.method =  download.method)
    # ### EDEN_date2 will be null, rec rates calculated from changePeriod
    # EDEN_date2 <- gsub(x = EDEN_begin, pattern = "-", replacement = "") # gsub(x = as.Date(eden1$date, format = "%Y%m%d") - changePeriod, pattern = "-", replacement = "")
    # # eden2 <- fireHydro::getEDEN(EDEN_date = EDEN_date2,  returnType = "raster",  download.method =  download.method)
  } else {
    eden1 <- EDEN_end
    eden2 <- EDEN_begin
  }
  
  ### convert to terra::SpatRaster
  if (!grepl(x = tolower(class(eden1$data)), pattern = "spatraster")){
    eden1$data <- terra::rast(eden1$data*1, crs = terra::crs(eden1$data, proj = TRUE))
  }
  if (!grepl(x = tolower(class(eden2$data)), pattern = "spatraster")){
    eden2$data <- terra::rast(eden2$data*1, crs = terra::crs(eden2$data, proj = TRUE))
  }
  if (!is.null(addToPlot)) {
    if (!grepl(x = tolower(class(addToPlot)), pattern = "spatvector")){
      addToPlot <- terra::vect(addToPlot, crs = terra::crs(addToPlot, proj = TRUE))
    }
  }
  
  ### get time period in weeks
  timePeriod <- as.numeric(as.Date(eden1$date, format = "%Y%m%d") - as.Date(eden2$date, format = "%Y%m%d")) / 7 
  
  ### plot prep: create EDEN boundary, set title, etc.
  mainTitle <- paste("Recession/ascension rate categories \n", as.Date(eden2$date, format = "%Y%m%d"), "to", as.Date(eden1$date, format = "%Y%m%d"))
  # r <- raster::reclassify(eden2$data, cbind(-Inf, Inf, 1))
  r <- terra::classify(eden2$data, cbind(-Inf, Inf, 1))
  # convert to polygons (you need to have package 'rgeos' installed for this to work)
  # pp <- raster::rasterToPolygons(r, dissolve=TRUE)
  pp  <- terra::as.polygons(r, dissolve=TRUE)
  
  if (!identical(terra::crs(eden2$data, proj = TRUE), terra::crs(eden1$data, proj = TRUE))) {
    eden1$data <- terra::project(x = eden1$data, y = terra::crs(eden2$data, proj = TRUE))
  }
  ### get recession rates in feet per week (cm / 2.54 / 12 /wks)
  recRates <- (eden1$data - eden2$data) / 2.54 / 12 / timePeriod # positive = ascension
  
  if (maskPlot == TRUE) {
    recRates <- terra::crop(x = recRates, y    = addToPlot)
    recRates <- terra::mask(x = recRates, mask = addToPlot, overwrite = TRUE)
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
  
  recRatesReclassed <- terra::classify(x = recRates, rcl = rclmat)
  
  
  ### remove any missing categories or plot will be messed up
  colorNames    <- colorNames[sort(terra::unique(terra::values(recRatesReclassed, na.rm = TRUE)))]
  categoryNames <- categoryNames[sort(terra::unique(terra::values(recRatesReclassed, na.rm = TRUE)))]
  
  # plot reclassified data
  if (!is.null(plotOutput)) {
    grDevices::png(plotOutput, width = 5, height = 6, units = "in", res = 150)
    graphics::par(mar = c(0.5, 0.5, 3.25, 0.5))
  }
  terra::plot(recRatesReclassed,
              legend = F, # doesn't work in development version of terra, so we need a workaround
              plg=list(x = 0, y = 0, title = ''),
              col = colorNames, axes = FALSE, 
              main = mainTitle)
  graphics::legend("topleft",
                   legend = categoryNames,
                   fill = colorNames,
                   border = FALSE,
                   bg = "white", 
                   bty = "n"
  ) # turn off legend border
  terra::plot(pp, add = TRUE)
  if (grepl(x = class(addToPlot), pattern = "SpatVector")) {
    # addToPlot <- terra::project(addToPlot, terra::crs(fireHydro::edenDEM, proj = TRUE)) # necessary?
    addToPlot <- terra::crop(x = addToPlot, y = recRatesReclassed)
    terra::plot(addToPlot, add = TRUE, border = "gray")
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
