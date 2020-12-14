#' @title Loads and pre-processes EverForecast simulations
#'
#' @description Loads netCDF files and converts units from cm depth to feet NGVD29. highly recommended to set raster options as in the example section.
#' 
#' @param directory    the address of an individual file in a folder of EverForecast .nc files. The default action is <code>file.choose()</code>; a browser menu appears so the user can select the the desired directory by identifying a single .nc file in the folder of netCDFs.
#' 
#' @return A rasterStack object with units feet NGVD29
#' 
#' @examples
#' loadSims
#' \dontrun{
#' ### recommended raster options to save to disk rather than work in 
#' ### memory
#' rasterOptions(progress = "text", timer = TRUE, overwrite = TRUE, 
#' todisk=TRUE, # critical to write temp files to disk rather than store in RAM
#' tmpdir = "G:/data/raster/temp/delete", # critical to specify a temp folder
#' tmptime = 4)
#' 
#'  }
#'  
#' @importFrom raster raster
#' @importFrom sp proj4string
#' @importFrom raster stack
#' @importFrom raster overlay
#' @importFrom raster raster
#' @importFrom raster raster
#'  
#' @export



loadSims <- function(directory = file.choose() 
                  ) {
  
  if (substr(directory, nchar(directory) - 2, nchar(directory)) %in% ".nc") {
    directory <- dirname(directory)
  } else stop("Incorrect directory name: directory specified in a character string must end with a '/'; if 'file.choose()' is used, the selected file must be a dicom image")
   
  # altq_folder <- "G:/data/ever4cast/20201201/eden/simulations_interpolated"
  # load("C:/RDATA/EVER4cast/NGVD_to_NAVD.RData") # loads convertNGVD, a raster surface of elevations NAVD88 rel to NGVD29. Units = feet. to convert NAVD88 to NGVD29, add this. subtract from NGVD29 to get NAVD88.
  dem_eden   <- raster::raster(system.file("extdata/edenDEM.grd", package = "fireHydro")) # DEM used in eden data 9meters NAVD88)
  dem_ngvd   <- (dem_eden * 3.2808) + convertNGVD # end units: feet NGVD29
  newCRS     <- sp::proj4string(dem_ngvd)
  
  fileNames  <- grep(list.files(directory, full.names = TRUE), pattern = ".nc$", value = TRUE) # find .nc files in data folder
  altQ_dat   <- lapply(X = fileNames[1:2], FUN = raster::stack) # water depth, cm w.r.t. soil surface
  stack.tryCatch <- function (x) { # ignores simulations that can't be read
    return(tryCatch(raster::stack(x), error=function(e) NULL))
  }
  altQ_ngvd   <- lapply(X = fileNames, FUN = stack.tryCatch) # pull all simulations that don't cause an error
  altQ_ngvd   <- altQ_ngvd[!sapply(altQ_ngvd, is.null)] # remove null entries
  ### convert cm to feet
  altQ_ngvd2  <- lapply(X = altQ_ngvd, FUN = function(x){return(x * 0.0328084)}) # convert cm to feet
  ### change projection
  altQ_ngvd3  <- lapply(altQ_ngvd2, function(x.stack) {sp::proj4string(x.stack) <- newCRS; return(x.stack)}) 
  ### convert to feet NGVD29
  altQ_ngvd4  <- lapply(altQ_ngvd3, function(x.stack) raster::overlay(x.stack, dem_ngvd, fun=function(r1, r2){return(r1 + r2)}))
  ### rename layers to include date in layer name)
  altQDat     <- lapply(X = altQ_ngvd4, function(x) {names(x) <- names(altQ_dat[[1]]); return(x)})
  # save("altQDat", file = "G:/data/ever4cast/20201101/altQ/ever4cast_altq.RData")
  
  return(altQDat)
  }
