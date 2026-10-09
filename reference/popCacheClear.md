# Clear the Cache

[`popGetInfo()`](https://petres.github.io/pollofpolls/reference/popGetInfo.md)
and
[`popRead()`](https://petres.github.io/pollofpolls/reference/popRead.md)
remember which polls exist and which colours belong to which party. If
`options(pollofpolls.dataMaxAge)` is set, the downloaded poll data is
kept as well. Everything is kept for the running session and, unless
`options(pollofpolls.cache = FALSE)` is set, in
`tools::R_user_dir("pollofpolls", "cache")`. Use this function to drop
it.

## Usage

``` r
popCacheClear()
```

## Value

Invisibly `TRUE` if cache files were removed, `FALSE` otherwise.

## Examples

``` r
if (FALSE) { # \dontrun{
popCacheClear()
} # }
```
