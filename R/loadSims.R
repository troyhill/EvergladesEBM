#' @title Loads and pre-processes EverForecast simulations
#'
#' @description Loads netCDF files and converts units from cm depth to feet NGVD29. highly recommended to set raster options as in the example section. Note: it is highly recommended that users change terra options to write temp files to disk rather than work in memory. See Examples section below. 
#' 
#' @param directory    the address of an individual file in a folder of EverForecast .nc files. The default action is <code>file.choose()</code>; a browser menu appears so the user can select the the desired directory by identifying a single .nc file in the folder of netCDFs.
#' 
#' @return A SpatRaster object with units feet NGVD29
#' 
#' @examples
#' 
#' \dontrun{
#' ### recommended raster options to save to disk rather than work in 
#' ### memory. My settings:
#' # terraOptions(memfrac=0.7, memmax = 2,
#' #   tmpdir = "G:/data/raster/temp/delete", 
#' #   todisk = TRUE,
#' #   progress = 1)
#' 
#' # select a .nc in the folder you want to load
#' # efcDat <- loadSims(directory = file.choose()) 
#' 
#'  }
#'  
#' @importFrom terra rast
#' @importFrom terra crs
#' @importFrom terra project
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 nc_close
#' @importFrom ncdf4 ncatt_get
#' @importFrom ncdf4 ncvar_get
#'  
#' @export



loadSims <- function(directory = file.choose() 
                  ) {
  
  ### using ncdf4. drawing on https://stackoverflow.com/a/20629634/3723870
  if (substr(directory, nchar(directory) - 2, nchar(directory)) %in% ".nc") {
    directory <- dirname(directory)
  } else stop("Incorrect directory name: directory specified in a character string must end with a '/'; if 'file.choose()' is used, the selected file must be a dicom image")
  
  # altq_folder <- "G:/data/ever4cast/20201201/eden/simulations_interpolated"
  # load("C:/RDATA/EVER4cast/NGVD_to_NAVD.RData") # loads convertNGVD, a raster surface of elevations NAVD88 rel to NGVD29. Units = feet. to convert NAVD88 to NGVD29, add this. subtract from NGVD29 to get NAVD88.
  dem_eden   <- terra::rast(system.file("extdata/edenDEM.grd", package = "fireHydro")) # DEM used in eden data 9meters NAVD88)
  newCRS     <- terra::crs(dem_eden, proj = TRUE)
  dem_ngvd   <- (dem_eden * 3.2808) + terra::project(x = terra::rast(EvergladesEBM::convertNGVD), y = dem_eden) # EvergladesEBM::convertNGVD end units: feet NGVD29
  fileNames  <- grep(list.files(directory, full.names = TRUE), pattern = ".nc$", value = TRUE) # find .nc files in data folder
  
  ### ndcf4 usage - can likely remove as terra develops
  nc_tmp     <- ncdf4::nc_open(fileNames[1])
  ### get dates
  ### returns a posixlt vector of dates from a netCDF (converting units from "days from XXXX-XX-XX")
  tdstr   <- paste0(unlist(strsplit(unlist(strsplit(ncdf4::ncatt_get(nc_tmp, 'time', "units")$value, " "))[3], "-")), collapse = "-")
  tdstr   <- substr(x = tdstr, start = 1, stop = 10) # remove T12:00:00, if present
  dateVec <- format(as.POSIXlt(as.POSIXlt(tdstr, format = "%Y-%m-%d") + 
                                 ncdf4::ncvar_get(nc_tmp, 'time')*60*60*24), format = "%Y-%m-%d")
  lon     <- ncdf4::ncvar_get(nc_tmp, "x", verbose = F) # meters state plane
  # lon[lon > 180] <- lon[lon > 180] - 360
  lat     <- ncdf4::ncvar_get(nc_tmp, "y", verbose = F) # may need to reverse this per https://stackoverflow.com/a/20629634/3723870
  fillvaluecrwc <- ncdf4::ncatt_get(nc_tmp, "depth", "_FillValue")$value
  
  
  ### have to use raster for loading netcdfs; replace this with ncdf4 use in future
  # altQ_dat_new   <- lapply(X = fileNames[1:2], FUN = raster::stack) # water depth, cm w.r.t. soil surface.
  # https://gis.stackexchange.com/a/413678/32782: "raster reads the CDF files with the ncdf4 package. In contrast, terra reads all data with GDAL. That is convenient from a programming perspective, but GDAL is very inefficient in dealing with datasets with many layers" 'For now, it is better to use raster to read NetCDF files with many layers.'
  # terra isn't preserving dates as layer names, and takes forever
  # e <- terra::rast(fileNames[1])
  # e2 <- stack(fileNames[1])
  # altQ_dat   <- lapply(X = fileNames[1:2], FUN = terra::rast) # water depth, cm w.r.t. soil surface.
  terra.tryCatch <- function (x) { # ignores simulations that can't be read
    # return(tryCatch(terra::rast(x), error=function(e) NULL)) 
    return(tryCatch(terra::rast(x), error=function(e) NULL)) 
  }
  # tst <- ncvar_get(nc_open(fileNames[1]), "depth", verbose = TRUE)
  # plot(tst[[1]])
  altQ_ngvd_new   <- lapply(X = fileNames, FUN = terra.tryCatch) # pull all simulations that don't cause an error
  altQ_ngvd_new   <- altQ_ngvd_new[!sapply(altQ_ngvd_new, is.null)] # remove null entries
  
  ### convert cm to feet
  # message('converting water depths: cm to feet NGVD29\n')
  altQ_ngvd2  <- lapply(X = altQ_ngvd_new, FUN = function(x){return(x * 0.0328084)}) # convert cm to feet
  ### change projection
  ### TODO: only do this if projections differ
  altQ_ngvd3  <- lapply(altQ_ngvd2, function(x.stack) {terra::crs(x.stack) <- newCRS; return(x.stack)}) 
  ### convert to feet NGVD29
  altQ_ngvd4 <- lapply(altQ_ngvd3, function(x.stack) {x.stack + dem_ngvd})
  # altQ_ngvd4  <- lapply(altQ_ngvd3, function(x.stack) raster::overlay(x.stack, dem_ngvd, fun=function(r1, r2){return(r1 + r2)}))
  ### rename layers to include date in layer name
  altQDat     <- lapply(X = altQ_ngvd4, function(x) {names(x) <- dateVec; return(x)})
  # save("altQDat", file = "G:/data/ever4cast/20201101/altQ/ever4cast_altq.RData")
  
  ncdf4::nc_close(nc_tmp)
  return(altQDat)
  }
