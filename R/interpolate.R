#' @import data.table

# Binomial smoothing of an existing trend.
bernoulliConvInterpolation = function(trend, n = 20, k = 6) {
    f = function(a) {
        t = c(rep(first(a$value), k), a$value, rep(last(a$value), k))
        weights = choose(n, (n/2-k):(n/2+k))
        list(date = a$date, value = stats::filter(t, weights/sum(weights))[(1+k):(length(t)-k)])
    }

    return (trend[, f(.SD), by=party])
}

# Fills the gaps between two trend values linearly.
linearInterpolation = function(trend) {
    f = function(a) {
        dates = min(a$date):max(a$date)
        list(date = as.date(dates), value = stats::approx(a[, .(date, value)], xout=dates, rule=2)$y)
    }

    return (trend[, f(.SD), by=party])
}

# Carries the last value forward until the next date with a value.
lastInterpolation = function(trend) {
    f = function(a) {
        dates = min(a$date):max(a$date)
        list(date = as.date(dates), value = rep(a$value, times = diff(c(as.integer(a$date), last(dates) + 1))))
    }

    return (trend[, f(.SD), by=party])
}
