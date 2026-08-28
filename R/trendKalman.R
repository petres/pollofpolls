#' @import data.table

# Sample size assumed for polls that do not report one.
.defaultSampleSize <- 400

# Prepares polls and election results for the Kalman filters: one row per date,
# party and observation, with the sampling variance attached.
observations = function(data) {
    pollData = toLong(data)
    if (!'n' %in% names(pollData))
        pollData[, n := NA_real_]
    pollData = pollData[, .(date, party, value, n = as.numeric(n))]
    pollData[is.na(n), n := .defaultSampleSize]

    electionData = toLong(data, 'elections')
    electionData[, n := Inf]
    electionData = electionData[, .(date, party, value, n)]

    rbind(pollData, electionData, fill = TRUE)[order(date)]
}

kalmanTime = function(partyData, sd) {
    tState = NULL
    tDate = NULL

    k = function(a, d) {
        if (!is.null(tDate))
            tState <<- k_predict_days(tState, sd, as.integer(d$date) - tDate)

        for (j in seq(nrow(a))) {
            if (is.null(tState)) {
                tState <<- c(a$value[j], a$var[j])
            } else {
                tState <<- k_update(tState, c(a$value[j], a$var[j]))
            }
        }
        tDate <<- as.integer(d$date)
        list('value' = tState[1], 'variance' = tState[2])
    }

    return (partyData[, k(.SD, .BY), by=date])
}

#' @import data.table
kalman = function(data, sd = 0.003) {
    pollData = observations(data)
    if (nrow(pollData) == 0)
        return(emptyLong())

    toProb = toProbFactor(data, pollData)
    pollData[, var := getPollVar(value/toProb, n)]

    trendData = list()
    for (p in data$parties$code) {
        partyData = pollData[party == p & !is.na(value)]
        if (nrow(partyData) == 0)
            next

        result = kalmanTime(partyData, sd)
        if (sum(!is.na(result$value)) < 2)
            next

        trendData[[p]] = result[, .(date = as.date(date), party = p, value, variance)]
    }

    rbindlist(trendData, fill = TRUE)
}


g_multiply = function(g1, g2)
    c((g1[2]*g2[1] + g2[2]*g1[1]) / (g1[2] + g2[2]), (g1[2] * g2[2]) / (g1[2] + g2[2]))

k_update = function(prior, likelihood)
    g_multiply(likelihood, prior)

k_predict_days = function(state, sd, days = 1)
    c(state[1], state[2] + days*sd**2)
