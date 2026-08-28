# pollofpolls

<!-- badges: start -->
[![R-CMD-check](https://github.com/petres/pollofpolls/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/petres/pollofpolls/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

R package for retrieving the national voting intention polls published by
[POLITICO's Poll of Polls](https://www.politico.eu/europe-poll-of-polls/) and
for calculating poll aggregations (trends).

## Install

```r
# install.packages('devtools')
devtools::install_github('petres/pollofpolls')
```

## Usage

```r
library(pollofpolls)

# Overview of all available polls (visits every country page once, cached afterwards)
popGetInfo()

# Read German national parliament polls
de = popRead('DE-parliament')
de

# Plot the polls
plot(de)
plot(de, xlim = as.Date(c('2025-01-01', '2026-01-01')))

# Add trends
de = popAddTrend(de, name = 'Kalman 0.003', type = 'kalman', args = list(sd = 0.003))
de = popAddTrend(de, name = 'Rolling mean 30d', type = 'weightedMeanLastDays',
                 args = list(days = 30))
plot(de)

# Get a description of the available trends and interpolations
?popAddTrend
```

`popRead()` returns a `popPolls` object, a list of `data.table`s:

| Element      | Content                                                                        |
| ------------ | ------------------------------------------------------------------------------ |
| `$polls`     | one row per poll: `date`, `dateFrom`, `firm`, `n` and one column per party      |
| `$elections` | election results in the same shape                                              |
| `$parties`   | party `code`, `name` and `color`                                                |
| `$trends`    | named list of trends in long format (`date`, `party`, `value`)                  |
| `$options`   | options as published by the endpoint, notably `measure` (`"p"` or `"s"`)        |

Shares are stored as fractions (`0.27` = 27 %); seat projections such as
`EU-parliament` are stored as absolute numbers.

## Trends

| Trend                  | Arguments                       |
| ---------------------- | ------------------------------- |
| `kalman`               | `sd = 0.003`                    |
| `kalmanKFAS`           | `sd = 0.003`, `smoothing = TRUE` (needs the `KFAS` package) |
| `weightedMeanLastDays` | `days = 30`, `maxObs = Inf`     |
| `ident`                | –                               |

Trends can be post-processed with `lastInterpolation`, `linearInterpolation` and
`bernoulliConvInterpolation`:

```r
de = popAddTrend(de, type = 'kalman', args = list(sd = 0.003),
                 interpolations = list(lastInterpolation = list()))
```

## Plotting with ggplot2

`plot()` is built in, but the object is made of plain `data.table`s, so
[ggplot2](https://ggplot2.tidyverse.org/) works just as well. Polls are stored
wide (one column per party) and trends long, so only the polls have to be
melted:

```r
library(ggplot2)
library(data.table)

de = popRead('DE-parliament')
de = popAddTrend(de, name = 'Kalman', type = 'kalman', args = list(sd = 0.003))

polls = melt(de$polls, id.vars = 'date', measure.vars = de$parties$code,
             variable.name = 'party', na.rm = TRUE)

colors = setNames(de$parties$color, de$parties$code)
labels = setNames(de$parties$name, de$parties$code)

ggplot(mapping = aes(date, value, colour = party)) +
    geom_point(data = polls, alpha = 0.2, size = 0.6) +
    geom_line(data = de$trends$Kalman, linewidth = 0.7) +
    scale_colour_manual(name = NULL, values = colors, labels = labels) +
    scale_y_continuous(labels = scales::percent) +
    coord_cartesian(xlim = as.Date(c('2021-09-26', NA))) +
    labs(title = de$name, x = NULL, y = NULL) +
    theme_minimal()
```

For a seat projection such as `EU-parliament` drop the `scale_y_continuous()`
line, the values are absolute numbers rather than shares.

ggplot2 is not a dependency of the package, install it separately.

## Caching and rate limits

The list of available polls, the party colours and the poll titles are not part
of the data endpoint and have to be read from the website. `pollofpolls` keeps
what it has seen for the running session and in
`tools::R_user_dir("pollofpolls", "cache")`, waits between requests and retries
rate limited ones. Options:

| Option                        | Default | Meaning                                  |
| ----------------------------- | ------- | ---------------------------------------- |
| `pollofpolls.cache`           | `TRUE`  | use the on-disk cache                    |
| `pollofpolls.cacheMaxAge`     | `86400` | maximum age of the cached index, seconds |
| `pollofpolls.requestDelay`    | `0.5`   | delay between requests, seconds          |
| `pollofpolls.attempts`        | `3`     | attempts per request                     |
| `pollofpolls.timeout`         | `60`    | request timeout, seconds                 |

`popCacheClear()` drops the cache, `popRead(code, metadata = FALSE)` skips the
website lookup completely.

## Data source

The data is published by POLITICO Europe at
<https://www.politico.eu/europe-poll-of-polls/>. This is an unofficial client;
it is neither affiliated with nor endorsed by POLITICO. Please check their terms
of use before redistributing the data, and be considerate with the number of
requests you send.

## License

GPL (>= 3)
