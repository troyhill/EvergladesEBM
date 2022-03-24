#' extractSimData
#'
#' @description This function extracts data and aggregates EverForecast simulation data (contained in a list of netCDFs) for spatial locations of interest. The output is a list of four permutations of the same data. This function can take a very long time depending on the number of regions-of-interest and their size (e.g., 2 hours for ~50 Combined Operations Plan indicator regions). 
#' 
#' @param simulationData       a list of netCDF simulation data in SpatRaster format (e.g., "datList" object)
#' @param targetLocations      locations to extract data from; must be class SpatVector
#' @param targetLocationNames  option to specify the name of target locations (e.g., pts$gage)
#' @param func                 function to apply to the data  (if multiple raster cells are in location of interest). The function name can be a character string or the function object (i.e., "mean" and mean produce identical output)
#'
#' @return a list.
#' This function extracts simulation data for spatial locations of interest (polygons, points)
#' It returns a list with four objects (permutations of the same data): 
#' (1) a list with a dataframe for each simulation data time series for each target location, 
#' (2) a list with a dataframe for each target location: rows = simulations, columns = days. Useful in cluster analysis
#' (3) a wide dataframe, and 
#' (4) a long dataframe
#'
#' @importFrom stats reshape
#' @importFrom terra plot
#' @importFrom terra vect
#' @importFrom terra rast
#' @importFrom terra crs
#' @importFrom terra project
#' @importFrom terra extract
#' 
#' @examples 
#' \dontrun{
#' ### step 1: identify region(s) of interest
#' locs   <- IRMap[[2]]
#' loc    <- locs[locs$INDICATOR %in% 118:119, ]
#' ### step 2: homogenize projections (or let the function do this internally)
#' # loc  <- spTransform(loc, crs(edenDEM))
#' 
#' ### step 3: load EverForeCast data from netCDFs
#' ### available here: 
#' ### https://s3.amazonaws.com/jem.models.headless/Ever4Cast_2020_12/simulations_interpolated.zip
#' # simDat  <- loadSims()
#' 
#' # step 4: extract simulation data at each ROIs
#' # EFDat <- extractSimData(simulationData      = simDat, 
#' #                           targetLocations     = loc, 
#' #                           targetLocationNames = loc$NAME)
#' }
#' @export
#' 



extractSimData <- function(simulationData,              # = datList,# *a list* of ncdf simulation data
                           targetLocations,             # = pts,# locations to extract data from (IRMap[[2]])
                           targetLocationNames = NULL,  # name of target locations (e.g., pts$gage)
                           func            = mean      # function to apply to data (if multiple raster cells are in location of interest) 
) {
  ### This function extracts simulation data for spatial locations of interest (polygons, points)
  ### It returns a list with four objects (permutations of the same data): 
  ### (1) a list with a dataframe for each simulation data time series for each target location, 
  ### (2) a list with a dataframe for each IR: rows = simulations, columns = days. Useful in cluster analysis
  ### (3) a wide dataframe, and 
  ### (4) a long dataframe
  
  ### convert data to terra 
  if(any(class(targetLocations) %in% c("SpatialPolygonsDataFrame", "SpatialPointsDataFrame", 'sf'))) {
    targetLocations <- terra::vect(targetLocations)
  }
  ### convert EDEN data to terra, if necessary
  if(any(class(simulationData[[1]]) %in% c("RasterBrick", "RasterStack", "Raster", "raster"))) {
    ### if test is a raster:
    simulationData <- lapply(X = simulationData, FUN = terra::rast)
  } else if (!any(class(simulationData[[1]]) %in% c("SpatRaster"))) {
    stop("simulationData input must be in terra (SpatRaster) or raster format. Pro-tip: use `returnType = 'raster'` argument in fireHydro::getEDEN()")
  }
  ### check/transform CRS
  if (!identical(terra::crs(targetLocations, proj = TRUE), terra::crs(simulationData[[1]], proj = TRUE))) {
    targetLocations <- terra::project(targetLocations, terra::crs(simulationData[[1]], proj = TRUE))
    cat("Coordinate reference systems did not match. Conversion has been performed internally but may be incorrect (and result in NAs). \n")
  }
  
  
  
  if (!identical(terra::crs(targetLocations, proj = TRUE), terra::crs(simulationData[[1]], proj = TRUE))) {
    targetLocations <- terra::project(targetLocations, terra::crs(simulationData[[1]], proj = TRUE))
    cat("Coordinate reference systems did not match. Conversion has been performed internally but may be incorrect (and result in NAs). \n")
  }
  
  terra::plot(simulationData[[1]][[1]], main = "Target locations used", axes = FALSE, legend = FALSE)
  terra::plot(targetLocations, add = TRUE)
  
  cat("Extracting simulation data for each target location. This may take a while... \n")
  extracted.int   <- lapply(simulationData, function(x) { # takes a LONG time
    x.df          <- data.frame(t(terra::extract(x, y = targetLocations, fun = func, na.rm = TRUE)))
    x.df
  })
  
  ### get start date from simulation data
  sim_dates <- gsub(x = rownames(extracted.int[[1]]), pattern = "X|\\.", replacement = "" )
  # extracted.int   <- lapply(simulationData, function(x) { # takes a LONG time
  #   x.df          <- t(raster::extract(x, y = targetLocations, fun = func, na.rm = TRUE))
  #   x.df
  # })
  # extracted.int    <- lapply(extracted.int, as.data.frame)
  sim_means_locs   <- mapply(cbind, extracted.int, "simulation" = 1:length(simulationData), SIMPLIFY = FALSE) # add an ID column to each element in the list
  traces_locs      <- do.call(rbind, sim_means_locs)
  
  ### first version:
  # traces_locs$date <- as.Date(beginDate, format = "%Y%m%d") + 1:raster::nlayers(simulationData[[1]])
  # dates.int  <- as.Date(strptime(as.character(beginDate), format = "%Y%m%d"))
  ### 2nd version:
  # traces_locs$date   <-  format(seq.Date(from = dates.int, to = dates.int + raster::nlayers(simulationData[[1]]), by = "day"), "%Y%m%d") 
  traces_locs$date <- as.Date(strptime(as.character(sim_dates), format = "%Y%m%d"))
  traces_locs <- traces_locs[!is.na(traces_locs$date), ] # using terra introduced some artifacts
  if (length(targetLocations) == 1) { # otherwise, name will appear as "t.raster..extract.x..y...targetLocations..fun...func..na.rm...TRUE.."
    names(traces_locs)[1] <- "X1"
  }
  
  traces_locs_long <- stats::reshape(traces_locs, 
                              direction = "long",
                              varying = list(names(traces_locs)[1:c(ncol(traces_locs) - 2)]),
                              v.names = "value",
                              timevar = "time", # change to "location_number" (requires updating package data (polyDat))
                              idvar = c("simulation", "date"))
  newDF                 <- data.frame(time = 1:length(targetLocationNames), name = targetLocationNames) # change "time" to "location_number"
  traces_locs_long$name <- newDF$name[match(traces_locs_long$time, newDF$time)] # change "time" to "location_number"
  
  for (i in 1:length(targetLocations)) {
    if (i == 1) featureDat <- list()
    featureDat[[i]] <-  data.frame(t(sapply(extracted.int, '[[', i))) # transpose, so that each day is a column
    featureDat[[i]] <-  featureDat[[i]][, -1] # remove day 1; all values are the same (all simulations have the same starting point)
  }
  
  ### return all useful objects in a list
  list(listOfSims      = extracted.int,    # a list with one dataframe per simulation, columns = locations, rows = days
       listOfLocations = featureDat,  # a list with a dataframe for each IR: rows = 100 simulations, columns = 182 days  (Day 1 is removed)
       traceDataWide   = traces_locs,      # a single dataframe, semi-wide form, with one row per simulation-date
       traceDataLong   = traces_locs_long) # a single dataframe, long form, with one row per location-simulation-date
}
