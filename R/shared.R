#' @import data.table

.trendFunctions <- c('kalman', 'kalmanKFAS', 'weightedMeanLastDays', 'ident')
.interpolationFunctions <- c('lastInterpolation', 'linearInterpolation',
                             'bernoulliConvInterpolation')

as.date = function(x, origin='1970-01-01')
    as.Date(x, origin=origin)

emptyLong = function()
    data.table(date = as.Date(character()), party = character(), value = numeric())

# Turns the wide poll/election tables into one row per poll and party.
toLong = function(data, what='polls') {
    table = data[[what]]
    measures = intersect(data$parties$code, colnames(table))
    if (is.null(table) || nrow(table) == 0 || length(measures) == 0)
        return(emptyLong())

    long = melt(table, variable.name = "party", measure.vars = measures)[!is.na(value)]
    long[, party := as.character(party)]
    long
}

# Variance of a share estimated from a sample of size n. Elections are passed in
# with n = Inf and therefore treated as exact.
getPollVar = function(p, n)
    p*(1-p)/n

# Polls are stored as shares for percentage based polls and as absolute numbers
# for seat based ones; the latter have to be scaled before getPollVar() is used.
toProbFactor = function(data, pollData) {
    if (!identical(data$options$measure, 's'))
        return(1)

    normalize = safeNumeric(data$options$normalize)
    if (!is.na(normalize) && normalize > 0)
        return(normalize)

    total = stats::median(pollData[, sum(value, na.rm = TRUE), by = 'date']$V1)
    if (is.na(total) || total <= 0) 1 else total
}

checkPopPolls = function(x, arg = 'x') {
    if (!inherits(x, 'popPolls'))
        stop(sprintf('`%s` must be a popPolls object, see popRead()', arg), call. = FALSE)
    invisible(x)
}

# Trend functions, including user supplied ones, have to return the long format
# used throughout the package.
checkTrend = function(trend, name) {
    if (!is.data.frame(trend) || !all(c('date', 'party', 'value') %in% names(trend)))
        stop(sprintf("Trend '%s' must return a data.frame with the columns date, party and value",
                     name), call. = FALSE)

    trend = copy(as.data.table(trend))
    trend[, `:=`(date = as.date(date), party = as.character(party), value = as.numeric(value))]
    setcolorder(trend, intersect(c('date', 'party', 'value', 'variance'), names(trend)))
    trend
}

# Date range of a plot: the range of `dates`, overridden by the non-missing
# elements of a user supplied `xlim` (dates or ISO date strings).
dateLimits = function(dates, xlim = NULL) {
    limits = as.date(range(dates, na.rm = TRUE))
    if (is.null(xlim))
        return(limits)

    if (length(xlim) != 2)
        stop('`xlim` must have two elements', call. = FALSE)
    xlim = as.Date(xlim)
    limits[!is.na(xlim)] = xlim[!is.na(xlim)]
    limits
}

# Adds the bounds of the central `level` interval to a trend; NA for trends
# without variance.
trendBounds = function(trend, level = 0.95) {
    z = stats::qnorm(1 - (1 - level)/2)
    bounds = copy(trend)
    if (!'variance' %in% names(bounds))
        bounds[, variance := NA_real_]

    bounds[, `:=`(lower = pmax(0, value - z*sqrt(variance)), upper = value + z*sqrt(variance))]
    bounds
}
