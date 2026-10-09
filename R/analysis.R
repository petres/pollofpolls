#' @import data.table

# Parties without a trend value in this many days before the reference date are
# treated as no longer polled.
.activeDays <- 90

pickTrend = function(x, trend = NULL) {
    if (length(x$trends) == 0)
        stop('No trends, add one with popAddTrend() or load the published ones with popRead()',
             call. = FALSE)
    if (is.null(trend))
        trend = names(x$trends)[length(x$trends)]
    if (!trend %in% names(x$trends))
        stop(sprintf("Unknown trend '%s', available are: %s", trend,
                     paste(names(x$trends), collapse = ', ')), call. = FALSE)

    x$trends[[trend]]
}

# Last value of every party at or before `at`, leaving out parties whose last
# value is older than `maxAge` days.
valuesAt = function(values, at, maxAge = .activeDays) {
    result = values[date <= at][order(date), .SD[.N], by = party]
    result[as.integer(at - date) <= maxAge]
}

# Value and variance of every party at `date` (by default the last date of the
# trend), see valuesAt().
trendAt = function(x, trend = NULL, date = NULL) {
    values = pickTrend(x, trend)
    if (!'variance' %in% names(values))
        values = copy(values)[, variance := NA_real_]

    at = if (is.null(date)) max(values$date) else as.Date(date)
    current = valuesAt(values, at)
    if (nrow(current) == 0)
        stop(sprintf('The trend has no values in the %d days before %s', .activeDays, format(at)),
             call. = FALSE)

    list(values = values, current = current, at = at)
}

lastElection = function(x, at) {
    elections = toLong(x, 'elections')[date <= at]
    if (nrow(elections) == 0)
        return(data.table(party = character(), election = numeric()))

    elections[date == max(date), .(party, election = value)]
}


#' Polls and Trends in Long Format
#'
#' Returns the polls, election results or trends of a `popPolls` object with one
#' row per date and party, the format expected by most plotting and modelling
#' tools.
#'
#' @param x A `popPolls` object.
#' @param what Which part to return: `"polls"`, `"elections"` or `"trends"`.
#'
#' @return A `data.table` with the columns `date`, `party` and `value`. Polls
#'   come with `dateFrom`, `firm` and `n` (sample size), trends with the name of
#'   the `trend` and, if available, its `variance`.
#' @export
#'
#' @examples
#' \dontrun{
#' de = popRead('DE-parliament')
#' popLong(de)
#' popLong(de, 'trends')
#' }
popLong = function(x, what = c('polls', 'elections', 'trends')) {
    checkPopPolls(x, 'x')
    what = match.arg(what)

    if (what == 'trends') {
        if (length(x$trends) == 0)
            return(data.table(trend = character(), emptyLong()))
        return(rbindlist(x$trends, idcol = 'trend', fill = TRUE))
    }

    setorder(toLong(x, what), 'date')
}

#' Current Standings
#'
#' Summarises a trend at a given date: the estimated support of every party,
#' its uncertainty, the change over the preceding days and the result of the
#' most recent election.
#'
#' @param x A `popPolls` object.
#' @param trend Name of the trend in `x$trends`, defaults to the one added last.
#' @param date Reference date, defaults to the last date of the trend.
#' @param compare Number of days the `change` is calculated over.
#' @param level Coverage of the interval given by `lower` and `upper`.
#'
#' @return A `data.table` sorted by support with the columns `party`, `name`,
#'   `date` (of the trend value used), `value`, `lower` and `upper` (`NA` for
#'   trends without variance), `change` and `election`. Parties without a trend
#'   value in the 90 days before `date` are left out.
#' @export
#'
#' @examples
#' \dontrun{
#' de = popRead('DE-parliament')
#' de = popAddTrend(de, name = 'kalman', type = 'kalman')
#' popLatest(de)
#' popLatest(de, date = '2025-01-01', compare = 90)
#' }
popLatest = function(x, trend = NULL, date = NULL, compare = 30, level = 0.95) {
    checkPopPolls(x, 'x')
    # `date` is also a column name, so the reference date is called `at`
    standing = trendAt(x, trend, date)
    at = standing$at
    current = trendBounds(standing$current, level)

    previous = valuesAt(standing$values, at - compare)[, .(party, previous = value)]
    result = merge(current, previous, by = 'party', all.x = TRUE)
    result = merge(result, lastElection(x, at), by = 'party', all.x = TRUE)
    result = merge(result, x$parties[, .(party = code, name)], by = 'party', all.x = TRUE)

    result[order(-value), .(party, name, date, value, lower, upper,
                            change = value - previous, election)]
}

#' Polling Firms
#'
#' Lists the polling firms of a `popPolls` object.
#'
#' @param x A `popPolls` object.
#'
#' @return A `data.table` with one row per firm and the columns `firm`,
#'   `polls` (number of polls), `first` and `last` (date of the first and the
#'   last poll), `sampleSize` (median sample size) and `spellings` (the names
#'   the firm is published under, see [popRenameFirms()]), sorted by the number
#'   of polls.
#' @export
#'
#' @examples
#' \dontrun{
#' popFirms(popRead('DE-parliament'))
#' }
popFirms = function(x) {
    checkPopPolls(x, 'x')
    if (nrow(x$polls) == 0)
        return(data.table(firm = character(), polls = integer(), first = as.Date(character()),
                          last = as.Date(character()), sampleSize = integer(),
                          spellings = character()))
    if (!'firm' %in% names(x$polls))
        stop('The polls have no firm column', call. = FALSE)

    polls = copy(x$polls)
    if (!'n' %in% names(polls))
        polls[, n := NA_integer_]
    if (!'firmRaw' %in% names(polls))
        polls[, firmRaw := firm]
    # the most frequent spelling first
    spellings = function(names) {
        names = cleanText(names)
        paste(names(sort(table(names), decreasing = TRUE)), collapse = ' | ')
    }

    firms = polls[, .(polls = .N, first = min(date), last = max(date),
                      sampleSize = as.integer(round(stats::median(n, na.rm = TRUE))),
                      spellings = spellings(firmRaw)),
                  by = firm]
    firms[order(-polls, firm)]
}

#' House Effects
#'
#' Estimates how much each polling firm deviates from the consensus: the mean
#' difference between its polls and a trend, by party. A positive `effect`
#' means that the firm sees the party stronger than the other firms.
#'
#' By default the effects are estimated together with the trend they are
#' measured against: a smoothed Kalman trend is calculated from the polls
#' corrected by the current effects, the effects are updated with the remaining
#' deviations, and so on until they no longer change. Otherwise a firm that
#' publishes a large share of the polls would pull the trend towards itself and
#' its effect would be underestimated. The effects are measured relative to the
#' average of the firms, each counting once however many polls it publishes:
#' an error all firms share, as seen at some elections, cannot be told apart
#' from the trend.
#'
#' The effects are assumed to be constant over time, so for long series it can
#' be worth restricting the polls to the last years first.
#'
#' @param x A `popPolls` object.
#' @param trend Name of a trend in `x$trends` the polls are compared to as they
#'   are, instead of estimating trend and effects together.
#' @param minPolls Firms with fewer polls of a party are left out.
#' @param sd Daily standard deviation of the Kalman trend, see [popAddTrend()].
#' @param iterations Maximum number of iterations.
#'
#' @return A `data.table` with the columns `firm`, `party`, `polls` (number of
#'   polls), `effect` (mean difference to the trend) and `se` (its standard
#'   error).
#' @export
#'
#' @examples
#' \dontrun{
#' de = popRead('DE-parliament')
#' de$polls = de$polls[date >= as.Date('2022-01-01')]
#' effects = popHouseEffects(de)
#' # one row per firm, one column per party
#' data.table::dcast(effects, firm ~ party, value.var = 'effect')
#'
#' # a trend of polls corrected by the house effects
#' de = popAddTrend(de, type = 'kalman', houseEffects = TRUE)
#' }
popHouseEffects = function(x, trend = NULL, minPolls = 5, sd = 0.003, iterations = 20) {
    checkPopPolls(x, 'x')
    if (nrow(x$polls) > 0 && !'firm' %in% names(x$polls))
        stop('The polls have no firm column', call. = FALSE)

    if (!is.null(trend))
        effects = deviations(x, pickTrend(x, trend))
    else
        effects = estimateHouseEffects(x, sd = sd, minPolls = minPolls, iterations = iterations)

    effects[polls >= minPolls][order(firm, party)]
}

# Mean deviation of the polls of every firm from `reference`, by party.
deviations = function(x, reference) {
    polls = toLong(x)
    if (nrow(polls) == 0 || nrow(reference) == 0 || !'firm' %in% names(polls))
        return(data.table(firm = character(), party = character(), polls = integer(),
                          effect = numeric(), se = numeric()))

    polls[, expected := NA_real_]
    for (p in unique(polls$party)) {
        r = reference[party == p & !is.na(value)]
        if (nrow(r) >= 2)
            polls[party == p, expected := stats::approx(r$date, r$value, xout = date, ties = mean)$y]
    }

    polls[!is.na(expected) & !is.na(firm),
          .(polls = .N, effect = mean(value - expected), se = stats::sd(value - expected)/sqrt(.N)),
          by = .(firm, party)]
}

# Subtracts the house effects from the polls. Shares cannot become negative.
adjustPolls = function(x, effects) {
    if (nrow(effects) == 0 || nrow(x$polls) == 0)
        return(x)

    polls = copy(x$polls)
    for (i in seq_len(nrow(effects))) {
        rows = which(polls$firm == effects$firm[i])
        p = effects$party[i]
        if (length(rows) > 0 && p %in% names(polls))
            set(polls, i = rows, j = p, value = pmax(polls[[p]][rows] - effects$effect[i], 0))
    }

    x$polls = polls
    x
}

# Backfitting of trend and house effects, see popHouseEffects().
estimateHouseEffects = function(x, sd = 0.003, minPolls = 5, iterations = 20, tolerance = 1e-5) {
    effects = data.table(firm = character(), party = character(), polls = integer(),
                         effect = numeric())

    for (i in seq_len(iterations)) {
        adjusted = adjustPolls(x, effects)
        reference = kalman(adjusted, sd = sd, smoothing = TRUE)
        remaining = deviations(adjusted, reference)[polls >= minPolls]

        updated = merge(effects[, .(firm, party, effect)],
                        remaining[, .(firm, party, polls, remaining = effect)],
                        by = c('firm', 'party'), all.y = TRUE)
        updated[is.na(effect), effect := 0]
        updated[, effect := effect + remaining]
        # a shift of all effects and the opposite shift of the trend fit the
        # polls equally well, so the effects are centred on the average firm
        updated[, effect := effect - mean(effect), by = party]

        change = merge(updated, effects[, .(firm, party, before = effect)], by = c('firm', 'party'),
                       all.x = TRUE)[, max(abs(effect - ifelse(is.na(before), 0, before)), 0)]
        effects = updated[, .(firm, party, polls, effect)]

        if (change < tolerance)
            break
    }

    # number of polls and standard errors of the polls as published
    result = merge(deviations(x, reference)[, .(firm, party, polls, se)],
                   effects[, .(firm, party, effect)], by = c('firm', 'party'))
    result[, .(firm, party, polls, effect, se)]
}
