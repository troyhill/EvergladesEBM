#' @title Create tables and maps of discontinuous hydroperiod (focus on Cape Sable Seaside Sparrow criteria)
#'
#' @description Calculates discontinuous hydroperiod for cells within a polygon and creates figures showing discontinuous hydroperiod (days dry) for the time period covered by the input EDEN dataset.
#' 
#' @param areaOfInterest    SpatialPolygonDataFrame for population of interest. CSSS habitat shapefiles are available from the RSM package (https://github.com/troyhill/RSM). See usage example below.
#' @param EDEN_data EDEN water depth data (units = cm w.r.t. soil surface), downloaded using fireHydro package (https://github.com/troyhill/fireHydro). See usage example below. This input is used in its entirety; no subsetting is done in the function.
#' @param elevation_offset Elevation offset for determining inundation (units = cm). To quantify hydroperiod at ground surface + 17 cm, use `elevation_offset = 17`.
#' @param plotOutput If the categorical plot should be saved, use this argument to set the filename (include any extension, e.g. "plot.png").
#' @param plotTitle Optional title for plot. If NULL, title is produced automatically and includes the date range in the EDEN data
#' @param categoryColors colors to be used for the discontinuous hydroperiod categories (corresponding to this order: 0 days dry, 1-89 days, 90-210 days, and >210 days)
#' 
#' @return list \code{plotCSS} returns a list with the calculated discontinuous hydroperiods, both raw and binned into the categories noted above (0 days dry, 1-89 days, 90-210 days, and >210 days), and a table with the days dry in each category.
#' 
#' 
#' @examples
#' \dontrun{
#' ### CSSS population shapefiles are available in RSM package
#' ### EDEN API available from fireHydro package
#' 
#' csss    <- vect(system.file("extdata/gis/misc", package = "RSM"), layer = "CSSS")
#' subpopA <- csss[6, ]
#' eden_input <- fireHydro::getAnnualEDEN(2021)
#' plotFileName  <- "subA_discontinuous_calendar_2021.png"
#' 
#' plotCSSS(areaOfInterest = subpopA, 
#'    EDEN_data = eden_input, 
#'    plotTitle = plotFileName)
#' 
#' 
#' }
#' 
#' 
#' @importFrom terra classify
#' @importFrom terra plot
#' @importFrom terra crop
#' @importFrom terra mask
#' @importFrom terra values
#' @importFrom terra subset
#' @importFrom terra crs
#' @importFrom terra unique
#' @importFrom terra sbar
#' @importFrom graphics legend
#' @importFrom terra project
#' @importFrom terra rast
#' @importFrom terra vect
#' @importFrom graphics par
#' @importFrom grDevices png
#' @importFrom terra ext
#' @importFrom utils tail
#' @importFrom graphics mtext
#'  
#' @export



plotCSSS <- function(areaOfInterest, # spdf; readOGR(system.file("extdata/gis/misc", package = "RSM"), layer = "CSSS")[6, ]
                             EDEN_data, # must be eden class, and cover a one year period (no subsetting is done in the function; entire series is used)
                             elevation_offset = 0,
                             plotOutput = NULL, # NULL or filename
                             plotTitle = NULL,
                             categoryColors = c("#eff3ff", "#9ecae1", "#3182bd") # 0-89, 90-210, >210
)
{
  ### input checks
  if (!grepl(x = tolower(class(areaOfInterest)), pattern = "spatvector")) {
    areaOfInterest <- terra::vect(areaOfInterest)
    # stop("'areaOfInterest' input must be a spatialPolygonDataFrame")
  }
  if (!grepl(x = class(EDEN_data), pattern = "eden")) {
    message("'EDEN_data' input must be of class 'eden'; use EDEN API in fireHydro. See fireHydro::getAnnualEDEN()")
  }
  if (!grepl(x = tolower(class(EDEN_data$data)), pattern = "spatraster")){
     EDEN_data$data <- terra::rast(EDEN_data$data*1)
  }
  ### transform CRS
  # areaOfInterest <- sp::spTransform(areaOfInterest, raster::crs(EDEN_data$data))
  # subA <- raster::crop(x = mask(x = EDEN_data$data, mask = areaOfInterest), y = areaOfInterest)
  areaOfInterest <- terra::project(areaOfInterest, terra::crs(EDEN_data$data, proj = TRUE))
  subA           <- terra::crop(x = terra::mask(x = EDEN_data$data, mask = areaOfInterest), y = areaOfInterest)
  
  
  ### including this in case filtering is desired in future
  dateMin <- EDEN_data$date[1]
  dateMax <- tail(EDEN_data$date, 1)
  ### target date ranges
  dateRange <- gsub(x = names(subA), pattern = "X", replacement = "")
  dateRange <- as.Date(gsub(x = dateRange, pattern = "\\.", replacement = "-"))
  # which((dateRange >= dateMin) & (dateRange < dateMax))
  dateSelect <- dateRange[(dateRange >= dateMin) & (dateRange < dateMax)]
  
  ### reclassify raster: 0s when dry, 1 when wet
  m      <- c(-9999, elevation_offset, 0,  
              elevation_offset, 9999, 1)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  tst    <- terra::classify(subA, rclmat, include.lowest = FALSE) # values equal to the lowest value in rcl are excluded from the row 
  # https://rspatial.org/terra/pkg/4-algebra.html
  tst2   <- sum(terra::subset(x = tst, subset = which((dateRange >= dateMin) & (dateRange < dateMax))))
  
  
  if (is.null(plotTitle)) { 
    title_text <- paste0("Discontinuous hydroperiod, ", substr(names(tst)[1], 2, 5))
  } else {
    title_text <- plotTitle
  }
  
  
  ### plot with continuous scale
  # png("subpopA_daysDry.png", width = 5, height = 5, units = "in", res = 150)
  par(mar = c(0.5, 1, 2, 1))
  plot(areaOfInterest, axes=FALSE)
  plot(tst2, axes=FALSE, box=FALSE, add = TRUE)
  # plot(tst2, axes=FALSE, box=FALSE, breaks=cuts, col = pal(length(dateSelect)), add = TRUE)
  plot(areaOfInterest, add = TRUE)
  mtext(paste0("Discontinuous hydroperiod\n", dateMin, " - ", dateMax), side = 3, line = -1)
  # dev.off()
  
  ### histogram
  # hist(subA.vals, 105)
  # abline(v = c(90, 210), lty = 2)
  
  
  ### area in each category:
  ### discontinuous hydroperiods during the calendar year of 0 to 89, 
  ### 90 to 210, and > 210 days
  subA.vals <- terra::values(tst2, na.rm = TRUE)[, 1] # discontinuous hydroperiod
  
  ### reclassify to get area in each category
  m      <- #c(-9999, #0, 0,
             # 0, 90, 1,
              c(-9999, 90, 0,
              90, 210, 1,
              210, 9999, 2)
  rclmat       <- matrix(m, ncol=3, byrow=TRUE)
  subA_cats    <- terra::classify(tst2, rclmat)
  # cuts <- c(0:3) #set breaks. 120 days in time period
  
  ### table: area in each category
  area.cats      <- data.frame(table(terra::values(subA_cats, na.rm = TRUE)[, 1]) / length(terra::values(subA_cats, na.rm = TRUE)[, 1]))
  area.cats$Var1 <- as.numeric(as.character(area.cats$Var1))
  if (nrow(area.cats) < nrow(rclmat)) { # if there's a missing category, add it
    missingVals <- c(0:(nrow(rclmat)-1))[-which(0:(nrow(rclmat)-1) %in% as.numeric(as.character(area.cats$Var1)))]
    filler.df   <-  data.frame(Var1 = missingVals,
                               Freq = 0)
    area.cats <- rbind(area.cats, filler.df)
    area.cats <- area.cats[order(area.cats$Var1), ]
  }
  area.cats$desc  <- rev(c(">210 days", "90-210 days", "0-89 days"))
  area.cats$color <- categoryColors
  # paste0(round(area.cats$Freq*100, 1), "%")
  subpopA.area    <- length(terra::values(subA_cats, na.rm = TRUE)[, 1]) * 400*400 * 0.000247105 / 1e3 # kacres
  area.cats$kac   <- area.cats$Freq * subpopA.area # kacres in each category
  area.cats$label <- paste0(area.cats$desc, ": ", paste0(round(area.cats$Freq*100, 1), "%; ", round(area.cats$kac, 1), " kac"))
  
  
  ### map of categorized sparrow hydroperiod
  # grep(colors(), pattern = "gold", value = TRUE)
  # plot reclassified data
  if (!is.null(plotOutput)) {
    grDevices::png(plotOutput, width = 5, height = 6, units = "in", res = 250)
    graphics::par(mar = c(1, 1, 2, 1))
  }
  terra::plot(areaOfInterest, axes = FALSE)
  terra::plot(subA_cats, add = TRUE, axes=FALSE, box=FALSE, legend = FALSE, #breaks=cuts, 
       col = area.cats$color[sort(unique(terra::values(subA_cats, na.rm = TRUE)[, 1]) + 1)]) # pal(4))
  terra::plot(areaOfInterest, add = TRUE)
  ### add scale bar
  barLength <- (terra::ext(subA_cats)[2] - terra::ext(subA_cats)[1])/10
  terra::sbar(barLength, label = paste0(round(barLength/1e3), " km"))
  
  graphics::mtext(text = title_text, side = 3, cex = 1.3)
  graphics::legend("topleft",
                   legend = rev(area.cats$label),
                   fill = rev(area.cats$color),
                   border = FALSE,
                   cex = 0.85,
                   bg = "white", 
                   bty = "n"
  )
  
  if (!is.null(plotOutput)) {
    grDevices::dev.off()
  }
  
  return(list(table = area.cats,
                 days_dry = tst2,
                 categorized = subA_cats
                 ))
}

