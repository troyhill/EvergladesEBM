#' @title Simulation data extracted for 51 south Florida indicator regions used in development of the Combined Operational Plan
#'
#' @description A list containing the output from running \link[EvergladesEBM]{extractSimData} on an EverForeCast simulation dataset beginning 20201201. The variables are as follows:
#'
#' \itemize{
#'   \item listOfSims      a list of simulation data for each indicator region. The list is composed of a dataframe for each simulation (n = 100); each dataframe has a column for each indicator region (n = 51) and a row for each day (n = 182). Values are mean water depth within the indicator region.
#'   \item listOfLocations a list of simulation data, re-organized. The list is composed of a dataframe for each indicator region (n = 51) with a row for each simulation (n = 100) and a column for each day (n = 181; first day is removed because it is identical in each simulation, creating a problem in cluster analysis)
#'   \item traceDataWide.  a somewhat-wide dataframe, with a column for each indicator region (n = 51) and a row for each simulation (n = 100) and date (n = 182). Additional rows specify the simulation number and date. In total there are 18200 rows and 53 columns.
#'   \item traceDataLong   a long dataframe, with one row for each unique combination of indicator region (n = 51), day (n = 182), and simulation (n = 100). There are 51x182x100 = 928,200 rows and five columns.
#'   }
#' @name polyDat 
#' @format A list with four elements
#' @examples 
#' summary(polyDat)
#' 
#' \dontrun{
#' ### code used to generate object 
#' ### step 1: identify region(s) of interest
#' locs   <- IRMap[[2]]
#' loc    <- locs[locs$INDICATOR %in% 118:119, ]
#' ### step 2: homogenize projections (or let the function do this internally)
#' # loc  <- spTransform(loc, crs(edenDEM))
#' 
#' ### step 3: load EverForeCast data from netCDFs
#' ### available here: 
#' ### https://s3.amazonaws.com/jem.models.headless/Ever4Cast_2020_12/simulations_interpolated.zip
#' simDat  <- loadSims()
#' 
#' # step 4: extract simulation data at each ROIs
#' polyDat <- extractSimData(simulationData      = simDat, 
#'                           targetLocations     = loc, 
#'                           targetLocationNames = loc$NAME)
#' # save("polyDat", file =  paste0(here::here(), "/data/polyDat.RData"))
#' 
#' }
NULL