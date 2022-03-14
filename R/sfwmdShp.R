#' 
#' @title Watershed shapefile used by the South Florida Water Management District 
#' 
#' @description A SpatVector downloaded and slightly modified from https://www.sfwmd.gov/science-data/gis. proj4string :+proj=utm +zone=17 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
#'
#' @name sfwmd
#' @format a SpatVector
#' @examples 
#' summary(sfwmd)
#' 
#' \dontrun{
#' ### code used to generate object 
#' library(fireHydro)
#' sfwmd <- terra::vect("C:/ESRIDATA/SFWMD/AHED_Watersheds-shp", "Watershed")
#' sfwmd <- terra::project(sfwmd, terra::crs(terra::rast(fireHydro::edenDEM), proj = TRUE))
#' sfwmd <- terra::crop(x = sfwmd, y = terra::rast(fireHydro::edenDEM))
#' # save("sfwmd", file =  paste0(here::here(), "/data/sfwmd.RData"))
#' 
#' }
NULL