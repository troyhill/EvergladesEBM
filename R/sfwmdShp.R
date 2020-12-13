#' 
#' @title Watershed shapefile used by the South Florida Water Management District 
#' 
#' @description A SpatialPolygonsDataFrame downloaded and slightly modified from https://www.sfwmd.gov/science-data/gis. proj4string :[+proj=utm +zone=17 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
#'
#' @name sfwmd
#' @format a SpatialPolygonsDataFrame
#' @examples 
#' summary(sfwmd)
#' 
#' \dontrun{
#' ### code used to generate object 
#' library(fireHydro)
#' sfwmd <- rgdal::readOGR("C:/ESRIDATA/SFWMD/AHED_Watersheds-shp", "Watershed")
#' sfwmd <- sp::spTransform(sfwmd, crs(fireHydro::edenDEM))
#' sfwmd <- raster::crop(x = sfwmd, y = fireHydro::edenDEM)
#' # save("sfwmd", file =  paste0(here::here(), "/data/sfwmd.RData"))
#' 
#' }
NULL