# Read Poll Data

Downloads a single Poll of Polls data set from POLITICO, or reads one
saved by
[`popDownload()`](https://petres.github.io/pollofpolls/reference/popDownload.md).

## Usage

``` r
popRead(
  code,
  load = c("polls", "elections", "trends", "events"),
  metadata = TRUE,
  dir = NULL,
  firms = getOption("pollofpolls.firms")
)
```

## Arguments

- code:

  Code of the poll data, e.g. `"DE-parliament"`. See
  [`popGetInfo()`](https://petres.github.io/pollofpolls/reference/popGetInfo.md)
  for the available codes.

- load:

  Which parts to load: any of `"polls"`, `"elections"`, `"trends"` (the
  trends already published by POLITICO) and `"events"`.

- metadata:

  Whether party colours and the descriptive name should be looked up on
  the website.

- dir:

  Directory with the files written by
  [`popDownload()`](https://petres.github.io/pollofpolls/reference/popDownload.md).
  If given, the data is read from `<dir>/<code>.json` instead of being
  downloaded.

- firms:

  Named character vector of firms to rename, see
  [`popRenameFirms()`](https://petres.github.io/pollofpolls/reference/popRenameFirms.md).

  Firm names that only differ in case, accents, punctuation and white
  space are merged under their most frequent spelling.

## Value

A `popPolls` object. `$polls` holds one row per poll with the columns
`date`, `dateFrom`, `firm`, `firmRaw` (the firm as published), `n`
(sample size) and one column per party, `$elections` the same for
election results, `$parties` the party codes, names and colours,
`$trends` the published trends in long format and `$events` the events
POLITICO marks in its charts.

## Details

Party colours and the descriptive name are not part of the data endpoint
and are looked up on the corresponding website. That lookup needs one
additional request, is cached (see
[`popCacheClear()`](https://petres.github.io/pollofpolls/reference/popCacheClear.md))
and can be switched off with `metadata = FALSE`.

Downloaded data is not cached unless `options(pollofpolls.dataMaxAge)`
is set to the number of seconds a download may be reused.

## Examples

``` r
if (FALSE) { # \dontrun{
de = popRead('DE-parliament')
plot(de)

# read data saved by popDownload()
popDownload('polls', codes = 'AT-parliament')
at = popRead('AT-parliament', dir = 'polls', metadata = FALSE)
} # }
```
