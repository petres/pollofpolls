# The articles use live data, which POLITICO refuses to serve to the cloud
# machines GitHub Actions runs on (HTTP 403). They are therefore knitted locally
# from the .Rmd.orig sources, and pkgdown only renders the result.
#
# Run from the package root whenever the code or the data changed noticeably:
#
#   Rscript vignettes/articles/precompute.R
#
# and commit the .Rmd files together with the figures/ directory.

pkgload::load_all(quiet = TRUE)

old = setwd('vignettes/articles')
on.exit(setwd(old), add = TRUE)

unlink('figures', recursive = TRUE)
for (source in list.files(pattern = '\\.Rmd\\.orig$'))
    knitr::knit(source, sub('\\.orig$', '', source), quiet = TRUE)
