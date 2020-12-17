#' extractSimData
#'
#' @description This function extracts data and aggregates EverForecast simulation data (contained in a list of netCDFs) for spatial locations of interest. The output is a list of four permutations of the same data. This function can take a very long time depending on the number of regions-of-interest and their size (e.g., 2 hours for ~50 Combined Operations Plan indicator regions). 
#' 
#' @param simulationData       a list of netCDF simulation data in rasterStack format (e.g., "datList" object)
#' @param targetLocations      locations to extract data from; must be class SpatialPointsDataFrame or SpatialPolygonsDataFrame
#' @param targetLocationNames  option to specify the name of target locations (e.g., pts$gage)
#' @param func                 function to apply to the data  (if multiple raster cells are in location of interest). The function name can be a character string or the function object (i.e., "mean" and mean produce identical output)
#' @param beginDate            disregarded; this is now determined internally. the first date in the simulation time series
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
#' @importFrom raster plot
#' @importFrom raster nlayers
#' @importFrom raster compareCRS
#' @importFrom sp spTransform
#' @importFrom sp CRS
#' 
#' @examples 
#' 
#' ### load simulation data
#' # fileNames  <- grep(list.files(dataFolder, full.names = TRUE), pattern = ".nc$", value = TRUE)
#' # datList    <- lapply(X = fileNames, FUN = stack)
#' 
#' ### load points of interest
#' # pts              <- read.delim("L:/Restoration Assessments/Non CERP Projects/COP/Position_ana
#' #                                 lysis/data/data_drought_gage_table_20200109.txt")
#' # coordinates(pts) <- c("UTM_east", "UTM_north")
#' 
#' ### extract data
#' # extractSimData(simulationData = datList, targetLocations = pts, beginDate = startDate)
#' 
#' @export
#' 



extractSimData <- function(simulationData,              # = datList,# *a list* of ncdf simulation data
                           targetLocations,             # = pts,# locations to extract data from (IRMap[[2]])
                           targetLocationNames = NULL,  # name of target locations (e.g., pts$gage)
                           func            = mean,      # function to apply to data (if multiple raster cells are in location of interest) 
                           beginDate       = Sys.Date() # = startDate # first date for simulations
) {
  ### This function extracts simulation data for spatial locations of interest (polygons, points)
  ### It returns a list with four objects (permutations of the same data): 
  ### (1) a list with a dataframe for each simulation data time series for each target location, 
  ### (2) a list with a dataframe for each IR: rows = simulations, columns = days. Useful in cluster analysis
  ### (3) a wide dataframe, and 
  ### (4) a long dataframe
  if (!raster::compareCRS(targetLocations, simulationData[[1]])) {
    targetLocations <- sp::spTransform(targetLocations, raster::crs(simulationData[[1]]))
    cat("Coordinate reference systems did not match. Conversion has been performed internally but may be incorrect (and result in NAs). \n")
  }
  
  raster::plot(simulationData[[1]][[1]], main = "Target locations used")
  raster::plot(targetLocations, add = TRUE)
  
  cat("Extracting simulation data for each target location. This may take a while... \n")
  extracted.int   <- lapply(simulationData, function(x) { # takes a LONG time
    x.df          <- data.frame(t(raster::extract(x, y = targetLocations, fun = func, na.rm = TRUE)))
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
    featureDat[[i]] <- data.frame(t(sapply(extracted.int, '[[', i))) # transpose, so that each day is a column
    featureDat[[i]] <-  featureDat[[i]][, -1] # remove day 1; all values are the same (all simulations have the same starting point)
  }
  
  ### return all useful objects in a list
  list(listOfSims      = extracted.int,    # a list with one dataframe per simulation, columns = locations, rows = days
       listOfLocations = featureDat,  # a list with a dataframe for each IR: rows = 100 simulations, columns = 182 days  (Day 1 is removed)
       traceDataWide   = traces_locs,      # a single dataframe, semi-wide form, with one row per simulation-date
       traceDataLong   = traces_locs_long) # a single dataframe, long form, with one row per location-simulation-date
}
