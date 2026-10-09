# Seat Projection

Converts vote shares into seats with a highest averages (D'Hondt,
Sainte-Laguë) or largest remainder (Hare-Niemeyer) method.

## Usage

``` r
popSeats(
  x,
  seats,
  threshold = 0,
  method = c("dhondt", "sainte-lague", "hare"),
  ...
)
```

## Arguments

- x:

  A `popPolls` object, whose shares are taken from
  [`popLatest()`](https://petres.github.io/pollofpolls/reference/popLatest.md),
  or a named numeric vector of vote shares.

- seats:

  Number of seats to distribute.

- threshold:

  Minimum share a party needs to get seats.

- method:

  `"dhondt"`, `"sainte-lague"` or `"hare"`.

- ...:

  Passed on to
  [`popLatest()`](https://petres.github.io/pollofpolls/reference/popLatest.md),
  e.g. `trend` or `date`.

## Value

A `data.table` with the columns `party`, `name` (if `x` is a `popPolls`
object), `share` and `seats`, sorted by seats.

## Details

This is a projection on the national level only: regional
constituencies, direct mandates, overhang seats and exceptions from the
threshold (such as the basic mandate clauses in Germany and Austria) are
not taken into account. For example,
`seats = 630, threshold = 0.05, method = "sainte-lague"` approximates
the German Bundestag, `seats = 183, threshold = 0.04, method = "dhondt"`
the Austrian Nationalrat.

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
popSeats(de, seats = 630, threshold = 0.05, method = 'sainte-lague')
} # }
```
