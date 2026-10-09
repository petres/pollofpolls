# Polls and Trends in Long Format

Returns the polls, election results or trends of a `popPolls` object
with one row per date and party, the format expected by most plotting
and modelling tools.

## Usage

``` r
popLong(x, what = c("polls", "elections", "trends"))
```

## Arguments

- x:

  A `popPolls` object.

- what:

  Which part to return: `"polls"`, `"elections"` or `"trends"`.

## Value

A `data.table` with the columns `date`, `party` and `value`. Polls come
with `dateFrom`, `firm` and `n` (sample size), trends with the name of
the `trend` and, if available, its `variance`.

## Examples

``` r
if (FALSE) { # \dontrun{
de = popRead('DE-parliament')
popLong(de)
popLong(de, 'trends')
} # }
```
