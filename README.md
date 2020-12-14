# EvergladesEBM

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/troyhill/fireHydro?branch=master&svg=true)](https://ci.appveyor.com/project/troyhill/fireHydro) [![codecov.io](https://codecov.io/github/troyhill/fireHydro/coverage.svg?branch=master)](https://codecov.io/github/troyhill/fireHydro?branch=master)


Analytical tools supporting ecosystem based management in south Florida


## What you have here

`EvergladesEBM` is an R package with analytical tools supporting ecosystem based management in south Florida.



## EvergladesEBM installation

```
install.packages("devtools")
devtools::install_github("troyhill/EvergladesEBM", ref = "main")
```


## EvergladesEBM usage

EvergladesEBM can be used for post-processing model output and making direct comparisons to ecological recommendations.


```
library(EvergladesEBM)

### map of recession rates over past two weeks
### function is highly flexible - see ?plotEDENChange for options
twoWeeks <- plotEDENChange(EDEN_date = Sys.Date(), 
           changePeriod = 2, # weeks
           addToPlot = sfwmd)
           
### recession/ascension during past month
pastMonth <- plotEDENChange(EDEN_date = Sys.Date(), 
           changePeriod = 4, # weeks
           addToPlot = sfwmd)

```

## Output: Regional ascension/recession rates

&nbsp;

<img src="https://github.com/troyhill/EvergladesEBM/blob/main/docs/figures/recession_EDEN_twoWeeks.png" width="375" height="450" /> <img src="https://github.com/troyhill/EvergladesEBM/blob/main/docs/figures/recessionRates.png" width="375" height="450" />

Figure 1. Recession rates (inches/week; left side) and categorizations based on Everglades Ecosystem-Based Management recommendations (right).


&nbsp;

## Output: Position analysis using EverForecast output

&nbsp;

<img src="https://github.com/troyhill/EvergladesEBM/blob/main/docs/figures/forecast_TreeIsland_HighWaterPM.png" width="600" height="360" />

Figure 2. Distribution of future water levels predicted by EverForeCast simulations (colored lines), along with observed water levels (black line). Horizontal line shows the ecological target for tree island inundation (objective: less than 120 days above 10.84'NGVD29).


