
bernoulliConvInterpolation = function(trend, n = 20, k = 6) {
    f = function(a) {
        t = c(rep(first(a$value), k), a$value, rep(last(a$value), k))
        weights = choose(n, (n/2-k):(n/2+k))
        list(date = a$date, value = filter(t, weights/sum(weights))[(1+k):(length(t)-k)])
    }

    return (trend[, f(.SD), by=party])
}


linearInterpolation = function(trend) {
    f = function(a) {
        dates = min(a$date):max(a$date)
        list(date = as.date(dates), value = approx(a[, .(date, value)], xout=dates, rule=2)$y)
    }

    return (trend[, f(.SD), by=party])
}

lastInterpolation = function(trend) {
    f = function(a) {
        dates = min(a$date):max(a$date)
        list(date = as.date(dates), value = rep(a$value, times = diff(c(as.integer(a$date), last(dates) + 1))))
    }

    return (trend[, f(.SD), by=party])
}
