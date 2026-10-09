# Plot polls with ggplot2

The ggplot2 counterpart of
[`plot.popPolls()`](https://petres.github.io/pollofpolls/reference/plot.popPolls.md):
polls as points, trends as lines, for trends with a variance,
uncertainty bands and the events in `object$events` as vertical lines.
The result is an ordinary ggplot object that can be extended with
further layers, scales and themes.

## Usage

``` r
# S3 method for class 'popPolls'
autoplot(object, ..., bands = TRUE, level = 0.95, xlim = NULL, events = TRUE)
```

## Arguments

- object:

  A `popPolls` object.

- ...:

  Ignored.

- bands:

  Whether uncertainty bands should be drawn.

- level:

  Coverage of the uncertainty bands.

- xlim:

  Date range to show, dates or ISO date strings; `NA` keeps the
  respective end of the data range.

- events:

  Whether events should be marked.

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
de = popRead('DE-parliament')
de = popAddTrend(de, name = 'kalman', type = 'kalman')
autoplot(de, xlim = c('2024-01-01', NA)) + theme_minimal()
} # }
```
