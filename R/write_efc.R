#' @title Write EverForecast data to disk
#'
#' @description Write a list of EverForecast data to a file folder
#' 
#' @param x  list of EverForecast SpatRaster data sets
#' @param directory  character. Output directory; this is not a filename. An individual file is saved for each element in the EverForecast list
#' 
#' @return none
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
#' @importFrom terra writeRaster
#' 
#' @export


write.efc <- function(x, 
                      directory = "G:/data/ever4cast/20220501/output/") {
  ### need to save data and dates. filename will be used to save an tiff (data) and a .txt (dates) 
  ### with the same name
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  # filename_date <- paste0(filename_base, ".txt")
  
  ### convert to SpatRaster if needed
  if (!any(grepl(x = tolower(class(x[[1]])), pattern = '^spatraster$'))) {
    x <- lapply(X = x, FUN = function(y) {terra::rast(y*1)})
  }
  
  ### save files
  # dput(x = x$date, file = filename_date)
  for(i in 1:length(x)) {
    filename_data <- paste0(directory, i, ".tif")
    terra::writeRaster(x = x[[i]], filename = filename_data, overwrite = TRUE)
  }
}
