# Get Info About Available Polls

Lists the polls published by POLITICO. Building the list means visiting
every country page once, which takes a while; the result is therefore
cached for a day (see
[`popCacheClear()`](https://petres.github.io/pollofpolls/reference/popCacheClear.md)).

## Usage

``` r
popGetInfo(refresh = FALSE)
```

## Arguments

- refresh:

  Whether the cached list should be rebuilt.

## Value

A `data.table` with the columns `code`, `iso2`, `name`, `page` and
`endpoint`.

## Examples

``` r
if (FALSE) { # \dontrun{
popGetInfo()
} # }
```
