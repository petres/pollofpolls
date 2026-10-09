# Plot polls

Draws the individual polls as points and every trend added with
[`popAddTrend()`](https://petres.github.io/pollofpolls/reference/popAddTrend.md)
(or already published by POLITICO) as a line. Trends that come with a
variance, such as `kalman`, are drawn with an uncertainty band.

## Usage

``` r
# S3 method for class 'popPolls'
plot(x, ..., bands = TRUE, level = 0.95)
```

## Arguments

- x:

  A `popPolls` object.

- ...:

  Passed on to
  [`graphics::plot()`](https://rdrr.io/r/graphics/plot.default.html),
  e.g. `xlim` to limit the date range. `xlim` takes dates or ISO date
  strings, `NA` keeps the respective end of the data range. The y axis
  is scaled to the polls inside `xlim`.

- bands:

  Whether uncertainty bands should be drawn.

- level:

  Coverage of the uncertainty bands.

## Value

Invisibly `x`.

## Examples

``` r
if (FALSE) { # \dontrun{
de = popRead('DE-parliament')
de = popAddTrend(de, name = 'kalman', type = 'kalman')
plot(de)
plot(de, xlim = c('2024-01-01', NA), level = 0.9)
} # }
```
