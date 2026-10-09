# Download Poll Data

Saves the data of several polls as JSON files, exactly as published by
POLITICO, e.g. to keep a snapshot or to work offline. The files can be
read with `popRead(code, dir = dir)`.

## Usage

``` r
popDownload(dir, codes = NULL, overwrite = TRUE, quiet = FALSE)
```

## Arguments

- dir:

  Directory the files are written to, created if necessary.

- codes:

  Codes of the polls to download, by default all polls listed by
  [`popGetInfo()`](https://petres.github.io/pollofpolls/reference/popGetInfo.md).

- overwrite:

  Whether existing files should be replaced.

- quiet:

  Whether progress messages should be suppressed.

## Value

Invisibly a `data.table` with the columns `code`, `file`, `status`
(`"downloaded"`, `"skipped"` or `"failed"`) and `error`.

## Details

Requests are sent one after the other with a delay in between (see the
`pollofpolls.requestDelay` option). A failed download does not stop the
others, see the `status` column of the result.

## Examples

``` r
if (FALSE) { # \dontrun{
popDownload('polls', codes = c('AT-parliament', 'DE-parliament'))
at = popRead('AT-parliament', dir = 'polls')

# everything
popDownload('polls')
} # }
```
