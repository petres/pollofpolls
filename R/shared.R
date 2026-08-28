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
