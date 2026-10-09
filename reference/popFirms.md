# Polling Firms

Lists the polling firms of a `popPolls` object.

## Usage

``` r
popFirms(x)
```

## Arguments

- x:

  A `popPolls` object.

## Value

A `data.table` with one row per firm and the columns `firm`, `polls`
(number of polls), `first` and `last` (date of the first and the last
poll) and `sampleSize` (median sample size), sorted by the number of
polls.

## Examples

``` r
if (FALSE) { # \dontrun{
popFirms(popRead('DE-parliament'))
} # }
```
