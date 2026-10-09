# Current Standings

Summarises a trend at a given date: the estimated support of every
party, its uncertainty, the change over the preceding days and the
result of the most recent election.

## Usage

``` r
popLatest(x, trend = NULL, date = NULL, compare = 30, level = 0.95)
```

## Arguments

- x:

  A `popPolls` object.

- trend:

  Name of the trend in `x$trends`, defaults to the one added last.

- date:

  Reference date, defaults to the last date of the trend.

- compare:

  Number of days the `change` is calculated over.

- level:

  Coverage of the interval given by `lower` and `upper`.

## Value

A `data.table` sorted by support with the columns `party`, `name`,
`date` (of the trend value used), `value`, `lower` and `upper` (`NA` for
trends without variance), `change` and `election`. Parties without a
trend value in the 90 days before `date` are left out.

## Examples

``` r
if (FALSE) { # \dontrun{
de = popRead('DE-parliament')
de = popAddTrend(de, name = 'kalman', type = 'kalman')
popLatest(de)
popLatest(de, date = '2025-01-01', compare = 90)
} # }
```
