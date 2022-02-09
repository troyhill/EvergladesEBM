#' @title Plot extracted EverForecast and EDEN data
#'
#' @description A ggplot wrapper for easily plotting extracted EDEN data and EverForecast output aggregated by `getQuantiles` 
#' 
#' @param quantileData    EverForecast quantile dataframe, as returned by `getQuantiles()`
#' @param EDENdata    EDEN quantile dataframe, as returned by `getQuantiles()`
#' @param daysToPlot    days of EDEN data to plot prior to EverForecast model run date
#' 
#' @return A ggplot object
#' 
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 scale_colour_brewer
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 labs
#' 
#'  
#' @export



plotPM <- function(quantileData,# = quantDat_dd, # value must be in variable named "ave" 
                   EDENdata, #= EDEN_dd,         # value must be in variable named "ave" 
                   daysToPlot = 60) {
  quantileData$quantile <- factor(quantileData$quantile)
  
  ### creates quantile plot with overlaid EDEN trace
  ggplot2::ggplot(quantileData, ggplot2::aes_string(x = "date", y = "ave")) + 
    ggplot2::theme_bw() + 
    ggplot2::geom_line(ggplot2::aes_string(color = "quantile"), size = 1.5) + #facet_wrap(. ~ name) + 
    ggplot2::scale_colour_brewer(palette = "RdYlBu") + 
    ggplot2::ylab("Stage (ft. NGVD29)") + 
    ggplot2::xlab("")  + 
    ggplot2::labs(color='Quantile') + 
    ggplot2::geom_line(data = EDENdata[(EDENdata$date >= (min(quantileData$date) - daysToPlot)) & (EDENdata$date < max(quantileData$date)),], 
              mapping = ggplot2::aes_string(x = "date", y = "ave"), size = 1.5)#+ 
    # ggplot2::coord_cartesian(min(c(quantileData$ave, EDENdata$ave), na.rm = TRUE), max(c(quantileData$ave, EDENdata$ave), na.rm = TRUE))
}
