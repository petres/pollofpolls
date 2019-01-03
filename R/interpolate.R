#' @import data.table

bernoulliConvInterpolation = function(trend, n = 20, k = 6) {
    party = NULL
    f = function(a) {
        t = c(rep(first(a$value), k), a$value, rep(last(a$value), k))
        weights = choose(n, (n/2-k):(n/2+k))
        list(date = a$date, value = stats::filter(t, weights/sum(weights))[(1+k):(length(t)-k)])
    }

    return (trend[, f(.SD), by=party])
}

linearInterpolation = function(trend) {
    party = NULL; . = NULL; value = NULL # WARNINGS
    f = function(a) {
        dates = min(a$date):max(a$date)
        list(date = as.date(dates), value = stats::approx(a[, .(date, value)], xout=dates, rule=2)$y)
    }

    return (trend[, f(.SD), by=party])
}

extendLastNotNAEntry = function(trend) {
    party = NULL; . = NULL; value = NULL # WARNINGS

    dates = sort(unique(trend$date))
    f = function(a, b) {
        if (last(a$date) != max(dates)) {
            lValue = last(a$value)
            lDate = last(a$date)
            nDate = dates[which(lDate == dates) + 1] - 1
            if (nDate > lDate)
                a = rbind(a, list(date = nDate, value = last(a$value), variance = NA))

        }
        a
    }
    return (trend[, f(.SD, .BY), by=party])
}

#' @import data.table
lastInterpolation = function(trend) {
    party = NULL
    f = function(a) {
        dates = min(a$date):max(a$date)
        list(date = as.date(dates), value = rep(a$value, times = diff(c(as.integer(a$date), last(dates) + 1))))
    }

    return (trend[, f(.SD), by=party])
}
