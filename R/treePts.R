#' 
#' @title Tree island locations and elevations
#' 
#' @description A SpatialPointsDataFrame with tree island elevations. Data provided by SFWMD. epsg:4326 (equivlane to proj4string :[+proj=utm +zone=17 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs). Elevations are in a variety of units (ft, cm, m) and datums (NGVD29, NAVD88). An hclust.cluster column includes hierarchical cluster groups identified by Jed Redwine.
#'
#' @name treePts
#' @format a SpatialPolygonsDataFrame
#' @examples 
#' summary(treePts)
#' 
#' \dontrun{
#' ### code used to generate object 
#' treePts <- read.csv("C:/RDATA/EVER_misc/tree_islands/data_TI_inundation_20201102.csv", 
#' stringsAsFactors = FALSE)
#' ### point/polygon of interest (with latitude and longitude coordinates)
#' names(treePts) <- tolower(names(treePts))
#' coordinates(treePts) <- c("longitude", "latitude")
#' proj4string(treePts) <- CRS("+init=epsg:4326")
#' treePts$stn    <- treePts$ti_id
#' # save("treePts", file =  paste0(here::here(), "/data/treePts.RData"))
#' 
#' }
NULL