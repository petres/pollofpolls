# Changelog

## pollofpolls 0.7.0

### New features

- Firm names that only differ in case, accents, punctuation or white
  space (`"INSA/YouGov"` and `"INSA YouGov"`, `"Tecnè"` and `"Tecne"`)
  are merged under their most frequent spelling; the published name is
  kept in the new `firmRaw` column.
  [`popRenameFirms()`](https://petres.github.io/pollofpolls/reference/popRenameFirms.md)
  and `options(pollofpolls.firms)` merge firms beyond that.
  [`popFirms()`](https://petres.github.io/pollofpolls/reference/popFirms.md)
  lists the spellings of every firm.
- Polls without a sample size (a quarter of all polls, more than half in
  IT, NL and SE) are weighted with the median sample size of their firm
  instead of 400 respondents in the Kalman trends. Out of sample, on 110
  elections, this is slightly more accurate. `missingSampleSize = 400`
  reproduces POLITICO’s trends exactly.
- [`popRead()`](https://petres.github.io/pollofpolls/reference/popRead.md)
  reads the events POLITICO marks in its charts into `$events`;
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  show them as dotted lines (`events = FALSE` to switch them off).
- `popSeats(simulations = )` turns the uncertainty of the trend into
  seat ranges and the probability of passing the threshold.
  [`popCoalitions()`](https://petres.github.io/pollofpolls/reference/popCoalitions.md)
  gives the seats and the probability of a majority of coalitions,
  either given ones or every plausible combination of up to three
  parties.
- [`popAccuracy()`](https://petres.github.io/pollofpolls/reference/popAccuracy.md)
  evaluates trends and firms against past election results, out of
  sample: the trends only see the polls published before each election.
- [`popHouseEffects()`](https://petres.github.io/pollofpolls/reference/popHouseEffects.md)
  estimates trend and house effects together, so that a firm publishing
  many polls can no longer pull the trend towards itself. The effects
  are relative to the average firm. `popAddTrend(houseEffects = TRUE)`
  corrects the polls by them before calculating a trend. This removes
  the differences between firms, not their common errors: out of sample,
  the corrected trends were not closer to election results.
- A refused request (HTTP 403) explains that POLITICO blocks many cloud
  servers and how to work around it.

### Bug fixes

- A poll share of 0 no longer turns a poll into an exact observation in
  the Kalman trends.

## pollofpolls 0.6.0

### New features

- `kalman()` gained `smoothing = TRUE`, a Rauch-Tung-Striebel smoother
  that also takes later polls into account. Together with
  `linearInterpolation` it reproduces POLITICO’s `kalmanSmooth` trend,
  without needing KFAS.
- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws
  uncertainty bands for trends with a variance (`bands`, `level`) and
  takes `xlim` as date strings, with `NA` for an open end.
- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  method for ggplot2 (suggested, not required).
- [`popAddTrend()`](https://petres.github.io/pollofpolls/reference/popAddTrend.md)
  takes a function as `type`, for custom trends.
- [`popLong()`](https://petres.github.io/pollofpolls/reference/popLong.md)
  returns polls, elections or trends in long format.
- [`popLatest()`](https://petres.github.io/pollofpolls/reference/popLatest.md)
  summarises a trend at a date: support, uncertainty, change and the
  last election result.
- [`popSeats()`](https://petres.github.io/pollofpolls/reference/popSeats.md)
  projects seats with the D’Hondt, Sainte-Laguë or Hare-Niemeyer method
  and a threshold.
- [`popFirms()`](https://petres.github.io/pollofpolls/reference/popFirms.md)
  lists the polling firms,
  [`popHouseEffects()`](https://petres.github.io/pollofpolls/reference/popHouseEffects.md)
  estimates how much each of them deviates from the consensus.
- [`popDownload()`](https://petres.github.io/pollofpolls/reference/popDownload.md)
  saves the data of several polls as JSON files, which
  `popRead(dir = ...)` reads back without a request.
- Downloaded poll data can be cached with
  `options(pollofpolls.dataMaxAge)`.
- A pkgdown site with a Get started article.

### Improvements

- [`popRead()`](https://petres.github.io/pollofpolls/reference/popRead.md)
  is about five times faster; the published trends are parsed
  column-wise instead of one date at a time.
- Firm names are cleaned of stray and invisible white space, so that
  `"Forsa"`, `"Forsa "` and `"Forsa\t"` count as one firm.
- Requests are sent with curl: responses are compressed, HTTP status
  codes are read directly and `Retry-After` is respected. A server
  asking for a break longer than `pollofpolls.maxRetryDelay` stops the
  request instead of being retried too early.
- Interpolations keep the `variance` of a trend and the usual column
  order.
- Polls without a publication date are dated by the start of their
  fieldwork, or dropped with a warning if that is missing as well.
- [`popRead()`](https://petres.github.io/pollofpolls/reference/popRead.md)
  explains unknown codes, unreadable responses and vector input.
- The objects returned by
  [`popRead()`](https://petres.github.io/pollofpolls/reference/popRead.md)
  record the `code` and the time the data was `retrieved`.
- [`popCacheClear()`](https://petres.github.io/pollofpolls/reference/popCacheClear.md)
  removes all cache files.

### Bug fixes

- The `variance` of Kalman trends of seat based polls is given in seats;
  it was on the share scale before.
- `linearInterpolation` no longer fails for parties with a single value.
- The plot legends leave out parties without data in the shown date
  range.
- `Rplots.pdf` no longer ends up in the built package.

## pollofpolls 0.5.0

The data source moved from `pollofpolls.eu` to POLITICO’s Poll of Polls
endpoints, which required a rewrite of the reading code.

### Breaking changes

- [`popRead()`](https://petres.github.io/pollofpolls/reference/popRead.md)
  now reads from
  `https://www.politico.eu/wp-json/politico/v1/poll-of-polls/`. Poll
  codes are unchanged (e.g. `DE-parliament`), but the codes of some
  parties differ from the ones used by the old website.
- `$polls` now has the columns `date`, `dateFrom`, `firm` and `n`. The
  `sd`, `source`, `media` and `method` columns of the old CSV endpoint
  are not published any more.
- [`popGetInfo()`](https://petres.github.io/pollofpolls/reference/popGetInfo.md)
  gained a `refresh` argument and returns the columns `code`, `iso2`,
  `name`, `page` and `endpoint`.

### New features

- [`popRead()`](https://petres.github.io/pollofpolls/reference/popRead.md)
  also loads the trends published by POLITICO (`load = "trends"`) and
  takes `metadata = FALSE` to skip the website lookup for party colours
  and titles.
- `weightedMeanLastDays()`, which was documented but not shipped, is
  available again as a trend type.
  [`popAddTrend()`](https://petres.github.io/pollofpolls/reference/popAddTrend.md)
  now rejects unknown trend and interpolation names instead of failing
  with `could not find function`.
- [`popCacheClear()`](https://petres.github.io/pollofpolls/reference/popCacheClear.md)
  empties the cached poll index. The index is now stored in
  `tools::R_user_dir("pollofpolls", "cache")` and reused across
  sessions.
- Requests identify the package, are throttled, retried on rate limiting
  and configurable through the `pollofpolls.*` options.

### Bug fixes

- [`popRead()`](https://petres.github.io/pollofpolls/reference/popRead.md)
  no longer fails on polls whose date is delivered as the string `"NA"`
  and no longer trips over sample sizes delivered as strings.
- Poll codes are read correctly from the website again. Before, the
  surrounding markup ended up in the code, which meant every read
  triggered two full scrapes of all country pages, ran into HTTP 429 and
  lost the party colours. A poll code is now looked up on its country
  page only.
- Seat based data such as `EU-parliament` is no longer divided by 100,
  and can be plotted even though it has no raw polls.
- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) scales the y
  axis to the date range given in `xlim` and only lists parties in the
  legend that actually appear in the data.
- HTML entities in poll titles (`&nbsp;`, `&#8212;`, …) are decoded.
