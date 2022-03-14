#' 
#' @title Florida Bay shapefile with subregions
#' 
#' @description A SpatialPolygonsDataFrame. proj4string: [+proj=utm +zone=17 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
#'
#' @name fb
#' @format a SpatialPolygonsDataFrame
#' @examples 
#' summary(fb)
#' 
#' \dontrun{
#' ### code used to generate object 
#' fb <- terra::vect("C:/ESRIDATA/Florida_Bay/subregions", "SFL_NNC") 
#' fb <- fb[grepl(x = fb$SEGMENT_NA, pattern = "Florida Bay|Manatee Bay"), ]
#' fb <- terra::project(fb, terra::crs(terra::rast(fireHydro::edenDEM), proj = TRUE))
#' # save("fb", file =  paste0(here::here(), "/data/fb.RData"))
#' 
#' }
NULL