#' @import data.table

kalmanKFAS = function(data, sd = 0.003, smoothing = TRUE) {
    if (!requireNamespace("KFAS", quietly = TRUE))
        stop("Package \"KFAS\" is needed. Please install it.", call. = FALSE)

    n = NULL; value = NULL; party = NULL; . = NULL; variance = NULL; # WARNINGS
    # get polls
    pollData = toLong(data)[, `:=`(
            firm = NULL,
            sd = NULL,
            n = as.numeric(n)
        )]
    # set n to min if missing
    m = min(pollData$n, na.rm = TRUE)
    if (!is.finite(m))
        m = 400
    pollData[is.na(n), n := m]
    rm(m)
    # combine polls on same day
    pollData = pollData[, .(n = sum(n), value = sum(n*value)/sum(n)), by=.(date, party)]
    # get elections
    electionData = toLong(data, 'elections')[, n := Inf]
    # remove polls on election date
    pollData = pollData[!date %in% electionData$date]
    # merge polls and elections
    pollData = rbind(pollData, electionData)

    pollData = pollData[order(date)]

    toProb = 1
    if (!is.null(data$options) && data$options$measure == "s")
        toProb = data$options$normalize

    pollData[, `:=`(variance = getPollVar(value/toProb, n),
                    n = NULL)]

    trendData = data.table()

    SSMcustom = KFAS::SSMcustom

    for (p in data$parties$code) {
        partyData = pollData[party == p & !is.na(value)]

        if (nrow(partyData) > 0) {
            dates = as.date(min(partyData$date):(max(partyData$date) + 1))
            fullData = merge(partyData, data.table(date = dates), by="date", all=T)

            d1 = fullData[1, date]
            a1 = fullData[1, value]
            P1 = fullData[1, variance]

            modelData = fullData[, .(value, variance)]
            modelData[1, `:=`(value = NA, variance = NA)]
            modelData[is.na(variance), variance := 0]
            m = KFAS::SSModel(modelData$value ~ -1 + SSMcustom(Z = 1, T = 1, R = 1, Q = (sd)**2, a1 = a1, P1 = P1),
                               H = array(modelData$variance, c(1, 1, nrow(modelData))))

            k = KFAS::KFS(m, return_model = FALSE)

            if (smoothing) {
                value = k$alphahat
                variance = k$V
            } else {
                value = k$att
                variance = k$Ptt
            }

            #trendData = rbind(trendData, data.table(date = dates, party = p, value = c(a1, value), variance = c(P1, variance)))
            trendData = rbind(trendData, data.table(date = dates, party = p, value = c(value), variance = c(variance)))
            #cbind(trendData, fullData)
        }
    }

    return (trendData)
}

