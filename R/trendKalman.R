#' @import data.table

# Polls in long format with a sample size for every poll, see fillSampleSizes().
pollObservations = function(data, missingSampleSize = 'firm') {
    polls = copy(data$polls)
    if (nrow(polls) > 0)
        polls[, n := fillSampleSizes(polls, missingSampleSize)]

    pollData = toLong(list(polls = polls, parties = data$parties))
    if (!'n' %in% names(pollData))
        pollData[, n := NA_real_]
    pollData[, .(date, party, value, n = as.numeric(n))]
}

# Prepares polls and election results for the Kalman filters: one row per date,
# party and observation, with the sample size attached. Elections are treated as
# exact.
observations = function(data, missingSampleSize = 'firm') {
    pollData = pollObservations(data, missingSampleSize)

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

# Rauch-Tung-Striebel smoother for the random walk filtered by kalmanTime().
# Runs backwards over the observation dates. The filtered state is overwritten
# step by step, so m[i + 1] and P[i + 1] are already smoothed when step i reads
# them, while m[i] and P[i] still hold the filtered state.
kalmanSmooth = function(filtered, sd) {
    m = filtered$value
    P = filtered$variance
    steps = diff(as.integer(filtered$date))*sd**2

    for (i in rev(seq_along(steps))) {
        predicted = P[i] + steps[i]
        gain = if (predicted > 0) P[i]/predicted else 0
        m[i] = m[i] + gain*(m[i + 1] - m[i])
        P[i] = P[i] + gain**2*(P[i + 1] - predicted)
    }

    data.table(date = filtered$date, value = m, variance = P)
}

#' @import data.table
kalman = function(data, sd = 0.003, smoothing = FALSE, missingSampleSize = 'firm') {
    pollData = observations(data, missingSampleSize)
    if (nrow(pollData) == 0)
        return(emptyLong())

    # the variances are calculated on the share scale, so for seat based polls
    # they have to be scaled back to seats in the end
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
        if (smoothing)
            result = kalmanSmooth(result, sd)

        trendData[[p]] = result[, .(date = as.date(date), party = p, value,
                                    variance = variance*toProb**2)]
    }

    rbindlist(trendData, fill = TRUE)
}


g_multiply = function(g1, g2)
    c((g1[2]*g2[1] + g2[2]*g1[1]) / (g1[2] + g2[2]), (g1[2] * g2[2]) / (g1[2] + g2[2]))

k_update = function(prior, likelihood)
    g_multiply(likelihood, prior)

k_predict_days = function(state, sd, days = 1)
    c(state[1], state[2] + days*sd**2)
