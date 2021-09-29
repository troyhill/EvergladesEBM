#' @title Convert feet to centimeters
#'
#' @description Converts feet to centimeters
#' 
#' @param ft    input value with units of feet.
#' 
#' @return A numeric value with units of centimeters
#' 
#' 
#' @examples
#' feetToCm(0.18)
#' 
#'  
#' @export



feetToCm     <- function(ft) {
  cmVal <- ft * 30.48
  return(cmVal)
}