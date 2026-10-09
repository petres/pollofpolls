# House Effects

Estimates how much each polling firm deviates from the consensus: the
mean difference between its polls and a trend, by party. A positive
`effect` means that the firm sees the party stronger than the other
firms.

## Usage

``` r
popHouseEffects(x, trend = NULL, minPolls = 5)
```

## Arguments

- x:

  A `popPolls` object.

- trend:

  Name of the trend in `x$trends` the polls are compared to. By default
  a smoothed Kalman trend is calculated.

- minPolls:

  Firms with fewer polls of a party are left out.

## Value

A `data.table` with the columns `firm`, `party`, `polls` (number of
polls), `effect` (mean difference to the trend) and `se` (its standard
error).

## Details

The trend is calculated from the polls of all firms, including the one
evaluated, so the effects of firms that publish a large share of the
polls are underestimated.

## Examples

``` r
if (FALSE) { # \dontrun{
de = popRead('DE-parliament')
effects = popHouseEffects(de)
# one row per firm, one column per party
data.table::dcast(effects, firm ~ party, value.var = 'effect')
} # }
```
