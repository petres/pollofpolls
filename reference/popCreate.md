# Create pop object

Bundles polls, parties, elections and trends into the `popPolls` object
used by
[`plot.popPolls()`](https://petres.github.io/pollofpolls/reference/plot.popPolls.md)
and
[`popAddTrend()`](https://petres.github.io/pollofpolls/reference/popAddTrend.md).
Normally created by
[`popRead()`](https://petres.github.io/pollofpolls/reference/popRead.md).

## Usage

``` r
popCreate(
  polls = data.table(),
  options = list(measure = "p"),
  parties = data.table(),
  trends = list(),
  name = NULL,
  elections = data.table(),
  code = NULL,
  retrieved = NULL
)
```

## Arguments

- polls:

  `data.table` of polls, one row per poll and one column per party.

- options:

  List of options as published by the endpoint, notably `measure` (`"p"`
  for percentages, `"s"` for seats).

- parties:

  `data.table` with the columns `code`, `name` and `color`.

- trends:

  Named list of trends, each a long `data.table` with the columns
  `date`, `party`, `value` and optionally `variance`.

- name:

  Name of the poll, used as plot title.

- elections:

  `data.table` of election results in the same shape as `polls`.

- code:

  Poll code the data was read for.

- retrieved:

  Time the data was downloaded.

## Value

An object of class `popPolls`.

## Examples

``` r
popCreate()
#> 
#> Polls:
#> 
#> Null data.table (0 rows and 0 cols)
#> 
#> 
```
