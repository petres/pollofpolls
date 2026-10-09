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
    values = trendBounds(pickTrend(x, trend), level)
    # `date` is also a column name, so the reference date gets its own symbol
    at = if (is.null(date)) max(values$date) else as.Date(date)

    current = valuesAt(values, at)
    if (nrow(current) == 0)
        stop(sprintf('The trend has no values in the %d days before %s', .activeDays, format(at)),
             call. = FALSE)

    previous = valuesAt(values, at - compare)[, .(party, previous = value)]
    result = merge(current, previous, by = 'party', all.x = TRUE)
    result = merge(result, lastElection(x, at), by = 'party', all.x = TRUE)
    result = merge(result, x$parties[, .(party = code, name)], by = 'party', all.x = TRUE)

    result[order(-value), .(party, name, date, value, lower, upper,
                            change = value - previous, election)]
}

#' Seat Projection
#'
#' Converts vote shares into seats with a highest averages (D'Hondt,
#' Sainte-Laguë) or largest remainder (Hare-Niemeyer) method.
#'
#' This is a projection on the national level only: regional constituencies,
#' direct mandates, overhang seats and exceptions from the threshold (such as
#' the basic mandate clauses in Germany and Austria) are not taken into
#' account. For example, `seats = 630, threshold = 0.05, method = "sainte-lague"`
#' approximates the German Bundestag, `seats = 183, threshold = 0.04,
#' method = "dhondt"` the Austrian Nationalrat.
#'
#' @param x A `popPolls` object, whose shares are taken from [popLatest()], or
#'   a named numeric vector of vote shares.
#' @param seats Number of seats to distribute.
#' @param threshold Minimum share a party needs to get seats.
#' @param method `"dhondt"`, `"sainte-lague"` or `"hare"`.
#' @param ... Passed on to [popLatest()], e.g. `trend` or `date`.
#'
#' @return A `data.table` with the columns `party`, `name` (if `x` is a
#'   `popPolls` object), `share` and `seats`, sorted by seats.
#' @export
#'
#' @examples
#' popSeats(c(A = 0.35, B = 0.30, C = 0.20, D = 0.10, E = 0.05),
#'          seats = 100, threshold = 0.06)
#'
#' \dontrun{
#' de = popRead('DE-parliament')
#' de = popAddTrend(de, name = 'kalman', type = 'kalman')
#' popSeats(de, seats = 630, threshold = 0.05, method = 'sainte-lague')
#' }
popSeats = function(x, seats, threshold = 0, method = c('dhondt', 'sainte-lague', 'hare'), ...) {
    method = match.arg(method)
    if (!is.numeric(seats) || length(seats) != 1 || is.na(seats) || seats < 1 || seats %% 1 != 0)
        stop('`seats` must be a positive whole number', call. = FALSE)

    if (inherits(x, 'popPolls')) {
        if (identical(x$options$measure, 's'))
            stop('The polls are seat projections already', call. = FALSE)
        latest = popLatest(x, ...)
        result = latest[, .(party, name, share = value)]
    } else {
        if (!is.numeric(x) || is.null(names(x)) || anyNA(names(x)))
            stop('`x` must be a popPolls object or a named numeric vector of shares', call. = FALSE)
        result = data.table(party = names(x), share = unname(x))
    }

    votes = result$share
    votes[is.na(votes) | votes < threshold] = 0
    allocation = allocateSeats(votes, seats, method)
    result[, seats := allocation]

    result[order(-seats, -share)]
}

allocateSeats = function(votes, seats, method) {
    if (sum(votes) <= 0)
        return(integer(length(votes)))

    if (method == 'hare') {
        quotas = votes/sum(votes)*seats
        allocation = floor(quotas)
        remaining = seats - sum(allocation)
        largest = order(quotas - allocation, votes, decreasing = TRUE)[seq_len(remaining)]
        allocation[largest] = allocation[largest] + 1
        return(as.integer(allocation))
    }

    divisors = if (method == 'dhondt') seq_len(seats) else 2*seq_len(seats) - 1
    quotients = outer(votes, divisors, '/')
    # ties are resolved in favour of the larger party
    winners = order(quotients, rep(votes, length(divisors)), decreasing = TRUE)[seq_len(seats)]
    tabulate(row(quotients)[winners], nbins = length(votes))
}

#' Polling Firms
#'
#' Lists the polling firms of a `popPolls` object.
#'
#' @param x A `popPolls` object.
#'
#' @return A `data.table` with one row per firm and the columns `firm`,
#'   `polls` (number of polls), `first` and `last` (date of the first and the
#'   last poll) and `sampleSize` (median sample size), sorted by the number of
#'   polls.
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
                          last = as.Date(character()), sampleSize = integer()))
    if (!'firm' %in% names(x$polls))
        stop('The polls have no firm column', call. = FALSE)

    polls = copy(x$polls)
    if (!'n' %in% names(polls))
        polls[, n := NA_integer_]

    firms = polls[, .(polls = .N, first = min(date), last = max(date),
                      sampleSize = as.integer(round(stats::median(n, na.rm = TRUE)))),
                  by = firm]
    firms[order(-polls, firm)]
}

#' House Effects
#'
#' Estimates how much each polling firm deviates from the consensus: the mean
#' difference between its polls and a trend, by party. A positive `effect`
#' means that the firm sees the party stronger than the other firms.
#'
#' The trend is calculated from the polls of all firms, including the one
#' evaluated, so the effects of firms that publish a large share of the polls
#' are underestimated.
#'
#' @param x A `popPolls` object.
#' @param trend Name of the trend in `x$trends` the polls are compared to. By
#'   default a smoothed Kalman trend is calculated.
#' @param minPolls Firms with fewer polls of a party are left out.
#'
#' @return A `data.table` with the columns `firm`, `party`, `polls` (number of
#'   polls), `effect` (mean difference to the trend) and `se` (its standard
#'   error).
#' @export
#'
#' @examples
#' \dontrun{
#' de = popRead('DE-parliament')
#' effects = popHouseEffects(de)
#' # one row per firm, one column per party
#' data.table::dcast(effects, firm ~ party, value.var = 'effect')
#' }
popHouseEffects = function(x, trend = NULL, minPolls = 5) {
    checkPopPolls(x, 'x')
    reference = if (is.null(trend)) kalman(x, smoothing = TRUE) else pickTrend(x, trend)

    polls = toLong(x)
    if (nrow(polls) > 0 && !'firm' %in% names(polls))
        stop('The polls have no firm column', call. = FALSE)
    if (nrow(polls) == 0 || nrow(reference) == 0)
        return(data.table(firm = character(), party = character(), polls = integer(),
                          effect = numeric(), se = numeric()))

    polls[, expected := NA_real_]
    for (p in unique(polls$party)) {
        r = reference[party == p & !is.na(value)]
        if (nrow(r) >= 2)
            polls[party == p, expected := stats::approx(r$date, r$value, xout = date, ties = mean)$y]
    }

    effects = polls[!is.na(expected) & !is.na(firm),
                    .(polls = .N, effect = mean(value - expected),
                      se = stats::sd(value - expected)/sqrt(.N)),
                    by = .(firm, party)]
    effects[polls >= minPolls][order(firm, party)]
}
