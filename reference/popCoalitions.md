# Coalitions

Projects the seats of coalitions and the probability that they reach a
majority, based on the simulations described in
[`popSeats()`](https://petres.github.io/pollofpolls/reference/popSeats.md).

## Usage

``` r
popCoalitions(
  x,
  seats,
  coalitions = NULL,
  threshold = 0,
  method = c("dhondt", "sainte-lague", "hare"),
  simulations = 2000,
  majority = NULL,
  level = 0.9,
  size = 3,
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

- coalitions:

  List of coalitions, each a character vector of party codes, e.g.
  `list(c("OEVP", "SPOE", "NEOS"), c("FPOE", "OEVP"))`. The names of the
  list are used as names of the coalitions.

- threshold:

  Minimum share a party needs to get seats.

- method:

  `"dhondt"`, `"sainte-lague"` or `"hare"`.

- simulations:

  Number of simulations, `0` for none. Needs a trend with a variance,
  such as `kalman`.

- majority:

  Seats needed for a majority, by default more than half.

- level:

  Coverage of the seat range given by `lower` and `upper`.

- size:

  Largest number of parties in a coalition if `coalitions` is not given.

- trend:

  Name of the trend in `x$trends`, defaults to the one added last.

- date:

  Date of the projection, defaults to the last date of the trend.

## Value

A `data.table` with the columns `coalition`, `parties` (the party codes,
separated by `+`), `seats` (projected from the trend values), `lower`,
`upper` (the range of seats) and `pMajority` (the share of simulations
in which the coalition reaches a majority).

## Details

Without `coalitions`, every combination of up to `size` parties is
considered that reaches a majority in at least 1 % of the simulations,
unless it contains a smaller combination that has a majority in at least
half of them.

## Examples

``` r
if (FALSE) { # \dontrun{
at = popRead('AT-parliament')
at = popAddTrend(at, name = 'kalman', type = 'kalman', args = list(smoothing = TRUE))
set.seed(1)
popCoalitions(at, seats = 183, threshold = 0.04)
popCoalitions(at, seats = 183, threshold = 0.04,
              coalitions = list(Zuckerl = c('OEVP', 'SPOE', 'NEOS'),
                                'Blau-Schwarz' = c('FPOE', 'OEVP')))
} # }
```
