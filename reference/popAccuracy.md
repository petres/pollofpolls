# Accuracy at Past Elections

Compares trends and polling firms with the results of the elections in
`x$elections`: how far was each trend, and the last poll of each firm,
from the result shortly before the election?

## Usage

``` r
popAccuracy(
  x,
  trends = list(kalman = list(type = "kalman")),
  days = 1,
  firms = TRUE,
  window = 30
)
```

## Arguments

- x:

  A `popPolls` object.

- trends:

  Named list of trends to evaluate, each a list of arguments for
  [`popAddTrend()`](https://petres.github.io/pollofpolls/reference/popAddTrend.md):
  `type`, `args`, `interpolations` and `houseEffects`.

- days:

  Days before the election that are evaluated.

- firms:

  Whether the polling firms should be evaluated as well.

- window:

  Only the polls of a firm published in this many days before the
  election are taken into account.

## Value

A `data.table` with one row per election, trend or firm and party, and
the columns `election` (date), `kind` (`"trend"` or `"firm"`), `source`
(name of the trend or firm), `party`, `estimate`, `result` and `error`
(`estimate - result`).

## Details

For every election the trends are calculated from scratch, using only
the polls published at least `days` before the election and the earlier
elections. The comparison is therefore out of sample: even smoothed
trends do not know the result, which makes it possible to compare trend
types and their arguments, e.g. different values of `sd`.

## Examples

``` r
if (FALSE) { # \dontrun{
de = popRead('DE-parliament')
accuracy = popAccuracy(de, trends = list(
    'sd 0.001' = list(type = 'kalman', args = list(sd = 0.001)),
    'sd 0.003' = list(type = 'kalman', args = list(sd = 0.003)),
    'mean 30d' = list(type = 'weightedMeanLastDays')
))
# mean absolute error, in percentage points
accuracy[, .(elections = uniqueN(election), mae = 100*mean(abs(error))),
         by = .(kind, source)][order(mae)]
} # }
```
