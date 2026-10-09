#' @import data.table

# Applies `f` to the values of every party, sorted by date, and restores the
# column order shared by all trends. `f` gets the columns of one party and
# returns a list with `date`, `value` and, if the trend has one, `variance`.
byParty = function(trend, f) {
    result = trend[order(date), f(.SD), by = party]
    setcolorder(result, intersect(c('date', 'party', 'value', 'variance'), names(result)))
    result
}

# Every day from the first to the last of `dates`. Built by date arithmetic,
# because seq() returns integer based dates, which data.table refuses to
# combine with the double based dates of other groups.
dailyDates = function(dates)
    min(dates) + 0:as.integer(max(dates) - min(dates))

# Binomial smoothing of an existing trend. The filter runs over consecutive
# trend values, so trends with gaps should be interpolated first.
bernoulliConvInterpolation = function(trend, n = 20, k = 6) {
    weights = choose(n, (n/2-k):(n/2+k))
    smooth = function(x) {
        t = c(rep(first(x), k), x, rep(last(x), k))
        as.numeric(stats::filter(t, weights/sum(weights))[(1+k):(length(t)-k)])
    }

    byParty(trend, function(a) {
        result = list(date = a$date, value = smooth(a$value))
        # neighbouring estimates are strongly correlated, so the standard
        # deviation is smoothed like the values themselves
        if (!is.null(a$variance))
            result$variance = smooth(sqrt(a$variance))**2
        result
    })
}

# Fills the gaps between two trend values linearly.
linearInterpolation = function(trend) {
    byParty(trend, function(a) {
        dates = dailyDates(a$date)
        # approx() needs two values, a single one is just kept
        interpolate = function(y)
            if (nrow(a) < 2) y else stats::approx(a$date, y, xout = dates, rule = 2)$y

        result = list(date = dates, value = interpolate(a$value))
        if (!is.null(a$variance))
            result$variance = interpolate(a$variance)
        result
    })
}

# Carries the last value forward until the next date with a value.
lastInterpolation = function(trend) {
    byParty(trend, function(a) {
        dates = dailyDates(a$date)
        index = findInterval(dates, a$date)
        result = list(date = dates, value = a$value[index])
        if (!is.null(a$variance))
            result$variance = a$variance[index]
        result
    })
}
