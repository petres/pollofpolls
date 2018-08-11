# pollofpolls
R package for poll retrieval from https://pollofpolls.eu and poll aggregation (trends) calculations.

## Install

The development version can be easily installed with the ``devtools`` r-package:

```r
devtools::install_git('https://github.com/petres/pollofpolls.git')
```

## Usage

```r
library(pollofpolls)

# Get overview of all available polls
popGetInfo()

# Read Austrian National Council polls
at = popRead('AT-parliament')
# Plot the polls
plot(at)
plot(at, xlim=as.Date(c('2017-06-01', '2018-06-01')))

# Add trend to polls
at = popAddTrend(at, name='Kalman 0.003', type='kalman', args=list(sd = 0.003))
at = popAddTrend(at, name='Kalman 0.003 Raw', type='kalman', args=list(sd = 0.003), 
                 interpolations=list(lastInterpolation = list()))
plot(at)

# Get description of the functions
?popAddTrend
```
