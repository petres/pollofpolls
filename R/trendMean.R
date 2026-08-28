#' @import data.table

# Mean of all polls published on the same day.
ident = function(data) {
    long = toLong(data)
    if (nrow(long) == 0)
        return(emptyLong())

    long[!is.na(value), .(value = mean(value)), by=.(date, party)][order(date, party)]
}

# Rolling mean over the last `days` days, weighted linearly by the age of the
# poll. `maxObs` limits how many of the most recent polls are taken into
# account, counting polls published on the same day by the same firm as one.
weightedMeanLastDays = function(data, days = 30, maxObs = Inf) {
    long = toLong(data)
    if (nrow(long) == 0)
        return(emptyLong())

    if (!'firm' %in% names(long))
        long[, firm := NA_character_]
    long = long[!is.na(value), .(date, firm, party, value)]

    dates = as.date(min(long$date):(max(long$date) + 1))
    window = data.table(target = dates, from = dates - days, to = dates)

    joined = long[window, on = .(date > from, date <= to),
                  .(target = i.target, date = x.date, firm, party, value),
                  allow.cartesian = TRUE, nomatch = NULL]
    if (nrow(joined) == 0)
        return(emptyLong())

    setorder(joined, target, -date, firm)
    joined[, id := rleid(date, firm), by = target]
    joined = joined[id <= maxObs]

    joined[, weight := days - as.integer(target - date)]
    trend = joined[weight > 0, .(value = sum(value*weight)/sum(weight)), by = .(date = target, party)]

    trend[order(date, party)]
}
