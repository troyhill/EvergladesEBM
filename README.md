# EvergladesEBM



## What you have here

`EvergladesEBM` is an R package to support ecosystem based management in south Florida.



## Installation

```
remotes::install_github("troyhill/EvergladesEBM", ref = "main")
```


## Usage

EvergladesEBM can be used for post-processing EverForecast model output and making direct comparisons between observed conditions and ecological recommendations.


```
library(EvergladesEBM)

### map of recession rates over past two weeks
### function is highly flexible - see ?plotEDENChange for options
eden_end   <- fireHydro::getEDEN(Sys.Date(), returnType = 'terra')
eden_begin <- fireHydro::getEDEN(Sys.Date() - 8, returnType = 'terra')

rec_rates <- plotEDENChange(EDEN_begin = eden_begin, EDEN_end = eden_end, 
                            addToPlot = sfwmd.shp)

           

```

## Regional ascension/recession rates

&nbsp;

<img src="https://github.com/troyhill/EvergladesEBM/blob/main/docs/figures/recession_EDEN.png" width="375" height="450" /> <img src="https://github.com/troyhill/EvergladesEBM/blob/main/docs/figures/recessionRates.png" width="375" height="450" />

Figure 1. Recession rates (inches/week; left side) and categorizations based on Everglades Ecosystem-Based Management recommendations (right).


&nbsp;

## Position analysis using EverForecast output

&nbsp;

EverForecast model simulations can be downloaded here: https://s3.amazonaws.com/jem.models.headless/index.html


<img src="https://github.com/troyhill/EvergladesEBM/blob/main/docs/figures/forecast_PM_3GageAve.png" width="600" height="360" />

Figure 2. Distribution of future water levels predicted by EverForeCast simulations (colored lines), along with observed water levels (black line) at the three-gage average in Water Conservation Area 3.







[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/troyhill/evergladesebm?branch=main&svg=true)](https://ci.appveyor.com/project/troyhill/evergladesebm) [![codecov.io](https://codecov.io/github/troyhill/evergladesebm/coverage.svg?branch=main)](https://codecov.io/github/troyhill/evergladesebm?branch=main)
