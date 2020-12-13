#' @title Convert feet to centimeters
#'
#' @description Converts feet to centimeters
#' 
#' @param ft    input value with units of feet.
#' 
#' @return A single value with units of centimeters
#' 
#' 
#' @examples
#' feetToCm(0.18)
#' 
#'  
#' @export



feetToCm     <- function(ft) {
  cmVal <- ft * 30.48
  invisible(cmVal)
}