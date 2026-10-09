# pollofpolls

<!-- badges: start -->
[![R-CMD-check](https://github.com/petres/pollofpolls/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/petres/pollofpolls/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

R package for retrieving the national voting intention polls published by
[POLITICO's Poll of Polls](https://www.politico.eu/europe-poll-of-polls/), for
calculating poll aggregations (trends), seat projections and house effects.
See the [Get started](https://petres.github.io/pollofpolls/articles/pollofpolls.html)
article for a walk-through.

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
plot(de, xlim = c('2025-01-01', NA))

# Add trends
de = popAddTrend(de, name = 'Kalman', type = 'kalman', args = list(sd = 0.003))
de = popAddTrend(de, name = 'Rolling mean 30d', type = 'weightedMeanLastDays',
                 args = list(days = 30))
plot(de)

# Get a description of the available trends and interpolations
?popAddTrend
```

`popRead()` returns a `popPolls` object, a list of `data.table`s:

| Element      | Content                                                                        |
| ------------ | ------------------------------------------------------------------------------ |
| `$polls`     | one row per poll: `date`, `dateFrom`, `firm`, `firmRaw`, `n` and one column per party |
| `$elections` | election results in the same shape                                              |
| `$parties`   | party `code`, `name` and `color`                                                |
| `$trends`    | named list of trends in long format (`date`, `party`, `value`, `variance`)      |
| `$events`    | events POLITICO marks in its charts (`date`, `name`), shown by the plots        |
| `$options`   | options as published by the endpoint, notably `measure` (`"p"` or `"s"`)        |

Shares are stored as fractions (`0.27` = 27 %); seat projections such as
`NL-parliament` are stored as absolute numbers. `popLong()` returns polls,
elections or trends in long format.

## Trends

| Trend                  | Arguments                                                   |
| ---------------------- | ----------------------------------------------------------- |
| `kalman`               | `sd = 0.003`, `smoothing = FALSE`, `missingSampleSize = "firm"` |
| `kalmanKFAS`           | `sd = 0.003`, `smoothing = TRUE`, `missingSampleSize = "firm"` (needs the `KFAS` package) |
| `weightedMeanLastDays` | `days = 30`, `maxObs = Inf`                                 |
| `ident`                | –                                                           |

Trends can be post-processed with `lastInterpolation`, `linearInterpolation` and
`bernoulliConvInterpolation`. The Kalman trends come with a variance, which
`plot()` shows as an uncertainty band (`bands = FALSE` to switch it off).

With `smoothing = TRUE` every estimate of the Kalman filter also takes the later
polls into account. Interpolated to daily values, this reproduces the
`kalmanSmooth` trend POLITICO publishes:

```r
de = popAddTrend(de, name = 'Kalman smoothed', type = 'kalman',
                 args = list(smoothing = TRUE),
                 interpolations = list(linearInterpolation = list()))
```

Polls without a sample size are given the median sample size of their firm.
POLITICO counts them as 400 respondents instead; `missingSampleSize = 400`
reproduces its trends exactly.

`popAddTrend(..., houseEffects = TRUE)` corrects every poll by the house effect
of its firm (see below) before the trend is calculated. This removes the
differences between firms, not the errors they share, so at past elections the
corrected trends were not more accurate (see `popAccuracy()`).

`type` also takes a function, which gets the `popPolls` object as `data` and
returns a `data.frame` with the columns `date`, `party` and `value` (optionally
`variance`):

```r
lastPoll = function(data) popLong(data)[, .(value = data.table::last(value)), by = .(date, party)]
de = popAddTrend(de, type = lastPoll)
```

## Standings, seats and coalitions

```r
# Support, uncertainty and change over the last 30 days according to the trend added last
popLatest(de)

# Seat projection (national level only, no direct mandates or overhang seats),
# with seat ranges simulated from the uncertainty of the trend
popSeats(de, seats = 630, threshold = 0.05, method = 'sainte-lague', simulations = 2000)

# Probability of a majority, for given coalitions or all plausible ones
popCoalitions(de, seats = 630, threshold = 0.05, method = 'sainte-lague',
              coalitions = list(c('Union', 'SPD'), c('Union', 'GRUENE')))
popCoalitions(de, seats = 630, threshold = 0.05, method = 'sainte-lague')
```

## Polling firms

Firm names that only differ in case, accents, punctuation or white space
(`INSA/YouGov` and `INSA YouGov`) are merged when the data is read; the name as
published is kept in `firmRaw`. Everything else, such as a renamed firm, can be
merged with `popRenameFirms()` or, for every `popRead()`, with
`options(pollofpolls.firms = c('Peter Hajek' = 'Hajek'))`.

```r
# Firms, number of polls and the spellings they are published under
popFirms(de)

# Deviation of every firm from the average firm, by party
popHouseEffects(de)
```

## Accuracy at past elections

`popAccuracy()` evaluates trends and firms against the election results in the
data. For every election, the trends are calculated only from the polls
published before it, so the comparison is out of sample:

```r
accuracy = popAccuracy(de, trends = list(
    kalman = list(type = 'kalman'),
    adjusted = list(type = 'kalman', houseEffects = TRUE),
    'mean 30d' = list(type = 'weightedMeanLastDays')
))
# mean absolute error per party, in percentage points
accuracy[, .(mae = 100*mean(abs(error))), by = .(kind, source)][order(mae)]
```

## Plotting with ggplot2

`autoplot()` draws the same as `plot()` with
[ggplot2](https://ggplot2.tidyverse.org/) and returns an ordinary ggplot object:

```r
library(ggplot2)

de = popRead('DE-parliament')
de = popAddTrend(de, name = 'Kalman', type = 'kalman', args = list(smoothing = TRUE))

autoplot(de, xlim = c('2021-09-26', NA)) +
    theme_minimal()
```

ggplot2 is not a dependency of the package, install it separately.

## Caching, rate limits and offline use

The list of available polls, the party colours and the poll titles are not part
of the data endpoint and have to be read from the website. `pollofpolls` keeps
what it has seen for the running session and in
`tools::R_user_dir("pollofpolls", "cache")`, waits between requests and retries
rate limited ones as long as the server asks it to (`Retry-After`). Options:

| Option                        | Default | Meaning                                              |
| ----------------------------- | ------- | ---------------------------------------------------- |
| `pollofpolls.cache`           | `TRUE`  | use the on-disk cache                                |
| `pollofpolls.cacheMaxAge`     | `86400` | maximum age of the cached index, seconds             |
| `pollofpolls.dataMaxAge`      | `0`     | maximum age of cached poll data, seconds (0: no cache) |
| `pollofpolls.requestDelay`    | `0.5`   | delay between requests, seconds                      |
| `pollofpolls.attempts`        | `3`     | attempts per request                                 |
| `pollofpolls.maxRetryDelay`   | `60`    | longest wait before a retry, seconds                 |
| `pollofpolls.timeout`         | `60`    | request timeout, seconds                             |
| `pollofpolls.firms`           | `NULL`  | firms to rename in every `popRead()`, see `popRenameFirms()` |

`popCacheClear()` drops the cache, `popRead(code, metadata = FALSE)` skips the
website lookup completely.

`popDownload()` saves the data of several polls (by default all of them) as
JSON files, exactly as published, and `popRead(dir = ...)` reads them back
without sending a request:

```r
popDownload('polls', codes = c('AT-parliament', 'DE-parliament'))
at = popRead('AT-parliament', dir = 'polls')
```

POLITICO's CDN refuses requests from many cloud servers (HTTP 403), e.g. CI
runners. Download the data on a local machine with `popDownload()`, copy the
files to the server and read them there with `popRead(dir = ...)`.

## Data source

The data is published by POLITICO Europe at
<https://www.politico.eu/europe-poll-of-polls/>. This is an unofficial client;
it is neither affiliated with nor endorsed by POLITICO. Please check their terms
of use before redistributing the data, and be considerate with the number of
requests you send.

## License

GPL (>= 3)
