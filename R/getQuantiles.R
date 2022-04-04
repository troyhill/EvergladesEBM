#' @title Summarizes EverForecast simulations. 
#'
#' @description Generates a dataframe of daily EverForecast quantiles for each point/region of interest (where population is the mean water depth on a given day in n EverForecast simulations).
#' 
#' @param traceDataLong    Lightly processed EverForecast simulation data, found in the traceDataLong object returned by RSM::extractSimData(). RSM::extractSimData() needs to be run for this function to be used.
#' @param ROI_names grep-style character vector used to select a subset of the targetLocations in the input in the traceDataLong object.
#' @param quantiles percentiles to include in output. For example, 0.5 would return the 50th percentile (median value from x simulations) for each day in the EverForecast simulations and for each region of interest identified in ROI_names
#' 
#' @return list \code{getQuantiles} returns a long dataframe with one row per date, region of interest, and quantile
#' 
#' 
#' @examples
#' getQuantiles
#' \dontrun{
#' }
#' 
#' @importFrom stats quantile
#' @importFrom stats reshape
#'  
#' @export



getQuantiles <- function(traceDataLong, #simDatPoly$traceDataLong, # this is extractSimData() output. extractSimData() needs to be run for this function to be relevant.
                         ROI_names = paste0(unique(traceDataLong$name), collapse = "|"), # character vector used in grep to select a subset of the targetLocations used as input in extractSimData()
                         quantiles = c(0.10, 0.25, 0.5, 0.75, 0.90)) {
  ### function generates a dataframe of quantiles for each region of interest (where population is the mean water depth on a given day of n = 100 simulations)
  newTraceData <- traceDataLong[grepl(x = traceDataLong$name, pattern = ROI_names), ]
  ### old ddply version:
  # IR.quantiles <- plyr::ddply(newTraceData, c("name", "date"), 
  #                             function(x) stats::quantile(x$value, probs = quantiles, na.rm = TRUE))
  
  # Loop for each ROI
  for(i in 1:length(unique(newTraceData$name))){
    # Make the output data frame on the first loop
    newTraceData_sub <- newTraceData[grepl(x = newTraceData$name, pattern = paste0("^", unique(newTraceData$name)[i], "$")), ]
    ### get quantiles for each day
    dat.means2 <- with(newTraceData_sub, data.frame(date=unique(newTraceData_sub$date),VAR1=tapply(value,date, function(x) stats::quantile(x, probs = quantiles, na.rm = TRUE))))
    foo <- data.frame(do.call(rbind, dat.means2$VAR1)) #data.frame(do.call('rbind', strsplit(as.character(dat.means2$VAR1),',',fixed=TRUE)))
    foo$date <- dat.means2$date
    foo$name <- unique(newTraceData$name)[i]
    
    if (i == 1) {
      IR.quantiles <- foo
    } else {
      IR.quantiles <- rbind(IR.quantiles, foo)
    }
  }
  
  tmp <- reshape(IR.quantiles, 
                 direction = "long",
                 varying = list(grep(x = names(IR.quantiles), pattern = "date|name", invert = TRUE, value = TRUE)),
                 v.names = "ave", # set this to "ave" so it can be easily plotted with EDEN output
                 idvar = c("date", "name"),
                 timevar = "quantile", # name of "time column
                 times = quantiles)    # categories for "time" column
  return(tmp)
}