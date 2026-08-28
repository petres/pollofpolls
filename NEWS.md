# pollofpolls 0.5.0

The data source moved from `pollofpolls.eu` to POLITICO's Poll of Polls
endpoints, which required a rewrite of the reading code.

## Breaking changes

* `popRead()` now reads from
  `https://www.politico.eu/wp-json/politico/v1/poll-of-polls/`. Poll codes are
  unchanged (e.g. `DE-parliament`), but the codes of some parties differ from
  the ones used by the old website.
* `$polls` now has the columns `date`, `dateFrom`, `firm` and `n`. The `sd`,
  `source`, `media` and `method` columns of the old CSV endpoint are not
  published any more.
* `popGetInfo()` gained a `refresh` argument and returns the columns `code`,
  `iso2`, `name`, `page` and `endpoint`.

## New features

* `popRead()` also loads the trends published by POLITICO (`load = "trends"`)
  and takes `metadata = FALSE` to skip the website lookup for party colours and
  titles.
* `weightedMeanLastDays()`, which was documented but not shipped, is available
  again as a trend type. `popAddTrend()` now rejects unknown trend and
  interpolation names instead of failing with `could not find function`.
* `popCacheClear()` empties the cached poll index. The index is now stored in
  `tools::R_user_dir("pollofpolls", "cache")` and reused across sessions.
* Requests identify the package, are throttled, retried on rate limiting and
  configurable through the `pollofpolls.*` options.

## Bug fixes

* `popRead()` no longer fails on polls whose date is delivered as the string
  `"NA"` and no longer trips over sample sizes delivered as strings.
* Poll codes are read correctly from the website again. Before, the surrounding
  markup ended up in the code, which meant every read triggered two full scrapes
  of all country pages, ran into HTTP 429 and lost the party colours. A poll code
  is now looked up on its country page only.
* Seat based data such as `EU-parliament` is no longer divided by 100, and can
  be plotted even though it has no raw polls.
* `plot()` scales the y axis to the date range given in `xlim` and only lists
  parties in the legend that actually appear in the data.
* HTML entities in poll titles (`&nbsp;`, `&#8212;`, ...) are decoded.
