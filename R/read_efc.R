#' @title Read loaded EverForecast data saved to disk
#'
#' @description Load an EverForecast list saved with write.efc. Returns a list of SpatRasters.
#' 
#' @param directory  character. Input directory
#' 
#' @return list of SpatRasters
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' everforecastDirectory <- "C:/path/to/data"
#' dat1 <- loadSims(directory = everforecastDirectory)
#' 
#' write.efc(x = dat1, directory = "G:/data/ever4cast/20220501/output/")
#' newdat <- read.efc(directory = "G:/data/ever4cast/20220501/output/")
#' 
#' newdat
#' 
#' }
#' 
#' @importFrom terra rast
#' 
#' @export


read.efc <- function(directory = "G:/data/ever4cast/20220501/output/") {
  input_files <- list.files(path = directory, pattern ='.tif$', full.names = TRUE)
  e4c_data    <- lapply(X= input_files, FUN = terra::rast)
  invisible(e4c_data)
}

