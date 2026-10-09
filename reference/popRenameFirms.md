# Rename Polling Firms

Merges polling firms that are published under different names. Spellings
that only differ in case, accents, punctuation and white space (such as
`"INSA/YouGov"` and `"INSA YouGov"`) are merged by
[`popRead()`](https://petres.github.io/pollofpolls/reference/popRead.md)
already; this function is for everything that needs knowledge about the
firms, e.g. a firm that has been renamed or polls published under the
name of a partner.

## Usage

``` r
popRenameFirms(x, firms)
```

## Arguments

- x:

  A `popPolls` object.

- firms:

  Named character vector: the names are the firm names to replace, the
  values the names to use instead, e.g. `c("Peter Hajek" = "Hajek")`.
  The names are matched ignoring case, accents, punctuation and white
  space.

## Value

`x` with the `firm` column of `$polls` renamed. The names as published
are kept in the `firmRaw` column.

## Details

Renamings that should apply to every
[`popRead()`](https://petres.github.io/pollofpolls/reference/popRead.md)
can be set once with `options(pollofpolls.firms = c(...))`. Renamings
are not chained: with `c(A = "B", B = "C")` firm `A` becomes `B`, not
`C`.

## Examples

``` r
if (FALSE) { # \dontrun{
at = popRead('AT-parliament')
popFirms(at)
at = popRenameFirms(at, c('Peter Hajek' = 'Hajek', 'Market' = 'Market/Lazarsfeld'))

# for all polls read from now on
options(pollofpolls.firms = c('Peter Hajek' = 'Hajek'))
} # }
```
