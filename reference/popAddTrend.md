# Add Trend to Polls

Calculates a poll aggregation and stores it in the `popPolls` object,
where it is picked up by
[`plot.popPolls()`](https://petres.github.io/pollofpolls/reference/plot.popPolls.md).

## Usage

``` r
popAddTrend(
  data,
  name = NULL,
  type = "kalman",
  args = list(),
  interpolations = list(),
  houseEffects = FALSE
)
```

## Arguments

- data:

  A `popPolls` object.

- name:

  Name of the trend. Defaults to a name built from `type` and the
  applied interpolations.

- type:

  Name of a trend function (see details) or a function.

- args:

  Arguments passed on to the trend function.

- interpolations:

  Named list of interpolations that should be applied to the trend, see
  details.

- houseEffects:

  Whether the polls should be corrected by the house effects of their
  firms (see
  [`popHouseEffects()`](https://petres.github.io/pollofpolls/reference/popHouseEffects.md))
  before the trend is calculated. The correction removes the differences
  between the firms, not the errors they share, so it does not
  necessarily bring the trend closer to election results; see
  [`popAccuracy()`](https://petres.github.io/pollofpolls/reference/popAccuracy.md).

## Value

The `popPolls` object with the trend added to `$trends`.

## Details

Available trend functions are:

- `kalman`:

  Kalman filter, arguments: `sd = 0.003`, the daily standard deviation
  of the true support on the share scale, `smoothing = FALSE` and
  `missingSampleSize = "firm"`. With `smoothing = TRUE` every estimate
  takes the later polls into account as well (Rauch-Tung-Striebel
  smoother). The estimates are calculated for the dates with polls only;
  together with `linearInterpolation` the smoothed trend reproduces
  POLITICO's daily `kalmanSmooth` trend. Polls are weighted by their
  sample size; polls without one get the median sample size of their
  firm (or of all polls), or the number given as `missingSampleSize`.
  POLITICO uses 400, so `missingSampleSize = 400` reproduces its trends
  exactly.

- `kalmanKFAS`:

  Kalman filter based on the KFAS package, arguments: `sd = 0.003`,
  `smoothing = TRUE`, `missingSampleSize = "firm"`.

- `weightedMeanLastDays`:

  Linearly weighted rolling mean, arguments: `days = 30`,
  `maxObs = Inf`.

- `ident`:

  Plain mean of all polls published on the same day, no arguments.

Instead of a name, `type` can be a function. It is called with the
`popPolls` object as argument `data` and the elements of `args`, and has
to return a `data.frame` with the columns `date`, `party` (the codes of
`data$parties`) and `value`, plus optionally `variance`, which is used
for the uncertainty bands.

Available interpolations are:

- `lastInterpolation`:

  Carries the last value forward, no arguments.

- `linearInterpolation`:

  Linear interpolation, no arguments.

- `bernoulliConvInterpolation`:

  Binomial smoothing over consecutive trend values, arguments: `n = 20`,
  `k = 6`.

Interpolations keep the `variance` of a trend, which is interpolated
(and smoothed) like the values; between two dates with polls it is
therefore an approximation.

## Examples

``` r
if (FALSE) { # \dontrun{
de = popRead('DE-parliament')
de = popAddTrend(de, name = 'Kalman 0.003', type = 'kalman', args = list(sd = 0.003))
de = popAddTrend(de, name = 'Kalman smoothed', type = 'kalman',
                 args = list(smoothing = TRUE),
                 interpolations = list('linearInterpolation' = list()))
plot(de)

# a custom trend: the median of the polls of the last 14 days
rollingMedian = function(data, days = 14) {
    polls = popLong(data)
    dates = seq(min(polls$date), max(polls$date), by = 'day')
    polls[, .(date = dates,
              value = vapply(dates, function(d) median(value[date > d - days & date <= d]),
                             numeric(1))), by = party]
}
de = popAddTrend(de, type = rollingMedian, args = list(days = 21))
} # }
```
