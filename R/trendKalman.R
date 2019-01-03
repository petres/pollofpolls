#' @import data.table
kalmanTime = function(partyData, sd) {
    date = NULL
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
        #cat(tDate)
        list('value' = tState[1], 'variance' = tState[2])
    }

    return (partyData[, k(.SD, .BY), by=date])
}

#' @import data.table
kalman = function(data, sd = 0.003) {
    n = NULL; value = NULL; party = NULL; changed = NULL; . = NULL; variance = NULL; # WARNINGS
    pollData = toLong(data)[, `:=`(
            firm = NULL,
            sd = NULL,
            n = as.numeric(n)
        )]
    pollData[is.na(n), n := 400]

    electionData = toLong(data, 'elections')[, n := Inf]
    pollData = rbind(pollData, electionData)

    pollData = pollData[order(date)]

    toProb = 1
    if (!is.null(data$options) && data$options$measure == "s")
        toProb = data$options$normalize

    pollData[, `:=`(var = getPollVar(value/toProb, n),
                    n = NULL)]

    trendData = data.table()

    for (p in data$parties$code) {
        partyData = pollData[party == p & !is.na(value)]
        if (nrow(partyData) > 0) {
            result = kalmanTime(partyData, sd)
            if (sum(!is.na(result$value)) < 2)
                next
            trendData = rbind(trendData, result[, .(date = as.date(date), party = p, value, variance)])
        }
    }

    return (trendData)
}


g_multiply = function(g1, g2)
    c((g1[2]*g2[1] + g2[2]*g1[1]) / (g1[2] + g2[2]), (g1[2] * g2[2]) / (g1[2] + g2[2]))

# g_sum = function(g1, g2)
#     c(g1[1] + g2[1], g1[2] + g2[2])

k_update = function(prior, likelihood)
    g_multiply(likelihood, prior)

# k_predict = function(state, sd)
#     g_sum(state, c(0, sd**2))

k_predict_days = function(state, sd, days = 1)
    c(state[1], state[2] + days*sd**2)
