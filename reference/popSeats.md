# Seat Projection

Converts vote shares into seats with a highest averages (D'Hondt,
Sainte-Laguë) or largest remainder (Hare-Niemeyer) method. With
`simulations`, the uncertainty of the trend is turned into a range of
seats.

## Usage

``` r
popSeats(
  x,
  seats,
  threshold = 0,
  method = c("dhondt", "sainte-lague", "hare"),
  simulations = 0,
  level = 0.9,
  trend = NULL,
  date = NULL
)
```

## Arguments

- x:

  A `popPolls` object, whose shares are taken from a trend (see
  [`popLatest()`](https://petres.github.io/pollofpolls/reference/popLatest.md)),
  or a named numeric vector of vote shares.

- seats:

  Number of seats to distribute.

- threshold:

  Minimum share a party needs to get seats.

- method:

  `"dhondt"`, `"sainte-lague"` or `"hare"`.

- simulations:

  Number of simulations, `0` for none. Needs a trend with a variance,
  such as `kalman`.

- level:

  Coverage of the seat range given by `lower` and `upper`.

- trend:

  Name of the trend in `x$trends`, defaults to the one added last.

- date:

  Date of the projection, defaults to the last date of the trend.

## Value

A `data.table` with the columns `party`, `name` (if `x` is a `popPolls`
object), `share` and `seats`, sorted by seats. With simulations also
`lower` and `upper`, the range of seats, and `pSeats`, the share of
simulations in which the party wins seats.

## Details

This is a projection on the national level only: regional
constituencies, direct mandates, overhang seats and exceptions from the
threshold (such as the basic mandate clauses in Germany and Austria) are
not taken into account. For example,
`seats = 630, threshold = 0.05, method = "sainte-lague"` approximates
the German Bundestag, `seats = 183, threshold = 0.04, method = "dhondt"`
the Austrian Nationalrat.

The simulations draw the share of every party independently from a
normal distribution with the value and variance of the trend. Shares of
different parties are in fact negatively correlated, so the ranges are
approximate. Use [`set.seed()`](https://rdrr.io/r/base/Random.html) for
reproducible results.

## Examples

``` r
popSeats(c(A = 0.35, B = 0.30, C = 0.20, D = 0.10, E = 0.05),
         seats = 100, threshold = 0.06)
#>     party share seats
#>    <char> <num> <int>
#> 1:      A  0.35    37
#> 2:      B  0.30    32
#> 3:      C  0.20    21
#> 4:      D  0.10    10
#> 5:      E  0.05     0

if (FALSE) { # \dontrun{
de = popRead('DE-parliament')
de = popAddTrend(de, name = 'kalman', type = 'kalman')
popSeats(de, seats = 630, threshold = 0.05, method = 'sainte-lague',
         simulations = 2000)
} # }
```
