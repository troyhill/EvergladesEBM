% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotEDENChange.R
\name{plotEDENChange}
\alias{plotEDENChange}
\title{Calculate ascension/recession rates over some time period}
\usage{
plotEDENChange(
  EDEN_date = Sys.Date() - as.numeric(format(Sys.Date(), "\%w")),
  changePeriod = 1,
  poor = c(c(0, Inf), c(-Inf, -0.18)),
  fair = c(c(-0.01, 0), c(-0.18, -0.05)),
  good = c(-0.05, -0.01),
  other = NA,
  otherName = "other",
  otherColor = "darkred",
  plotOutput = NULL,
  addToPlot = NA,
  maskPlot = FALSE
)
}
\arguments{
\item{EDEN_date}{Date (format = '\%Y-\%m-\%d') at end of recession rate calculation. Default behavior is to find the nearest Sunday.}

\item{changePeriod}{Time period (units = weeks) over which stage changes are measured. Default is one week.}

\item{poor}{Vector of paired values identifying 'poor' ascension/recession rates. Units must be feet/week. Pairs must have the lower value first, e.g., c(0, Inf, -Inf, -1.8)}

\item{fair}{Vector of paired values identifying 'fair' ascension/recession rates. Units must be feet/week. Pairs must have the lower value first}

\item{good}{Vector of paired values identifying 'good' ascension/recession rates. Units must be feet/week. Pairs must have the lower value first.}

\item{other}{Vector of paired values for an additional category (optional)}

\item{otherName}{Legend entry for the additional category (optional)}

\item{otherColor}{Color to be used for additional category (optional)}

\item{plotOutput}{If the produced plot should be saved, use this argument to set the filename (include any extension, e.g. "plot.png").}

\item{addToPlot}{If you'd like an SPDF added to the plot, pass it to this argument.}

\item{maskPlot}{If set to TRUE, the spdf in addToPlot is used to mask and crop the data. This is useful if a small area (e.g., WCA3A) is of interest.}
}
\value{
list \code{plotEDENChange} returns a list with the calculated rates (stageChange; units are feet/week), rates categorized into poor/fair/good (categories), a description of the time period used (description), and the criteria used to assign categories (criteria; units are feet/week)
}
\description{
Calculates two-point ascension/recession rates from EDEN data. Produces a map, and two verisons of the raster output (one with rates of change, and one with rates categorized into poor/fair/good)
}
\examples{
\dontrun{
### by default, the most recent one-week period in EDEN is used
plotOut <- plotEDENChange(addToPlot = sfwmd.shp)
ylim.range <- max(abs(floor(cellStats(plotOut$stageChange, min))), 
abs(ceiling(cellStats(plotOut$stageChange, max))))

spplot(plotOut$stageChange, main = plotOut$description, # feet/week
col.regions=colorRampPalette(c('red', 'white', 'blue'))(100),
at = seq(-ylim.range, ylim.range, by = 0.5))  + 
latticeExtra::layer(sp.polygons(sfwmd.shp, fill = NA))

### identify recession rates at specific locations
extract(plotOut$stageChange, bay.coords, fun = mean)
extract(plotOut$categories, bay.coords, fun = mean)
}

}
