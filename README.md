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

```

### Output example: Regional ascension/recession rates

&nbsp;

<img src="https://github.com/troyhill/EvergladesEBM/blob/main/docs/figures/recession_EDEN_twoWeeks.png" width="375" height="450" /> <img src="https://github.com/troyhill/EvergladesEBM/blob/main/docs/figures/recessionRates.png" width="375" height="450" />

Figure 1. Recession rates (inches/week; left side) and categorizations based on Everglades Ecosystem-Based Management recommendations (right).