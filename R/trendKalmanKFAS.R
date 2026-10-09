#' @import data.table

kalmanKFAS = function(data, sd = 0.003, smoothing = TRUE, missingSampleSize = 'firm') {
    if (!requireNamespace("KFAS", quietly = TRUE))
        stop("Package \"KFAS\" is needed. Please install it.", call. = FALSE)

    pollData = pollObservations(data, missingSampleSize)
    if (nrow(pollData) == 0)
        return(emptyLong())

    # combine polls published on the same day
    pollData = pollData[, .(n = sum(n), value = sum(n*value)/sum(n)), by=.(date, party)]

    electionData = toLong(data, 'elections')
    electionData[, n := Inf]
    electionData = electionData[, .(date, party, n, value)]

    # elections replace the polls published on the same day
    pollData = pollData[!date %in% electionData$date]
    pollData = rbind(pollData, electionData, fill = TRUE)[order(date)]

    toProb = toProbFactor(data, pollData)
    pollData[, variance := getPollVar(value/toProb, n)]

    trendData = list()
    SSMcustom = KFAS::SSMcustom

    for (p in data$parties$code) {
        partyData = pollData[party == p & !is.na(value)]
        if (nrow(partyData) < 2)
            next

        dates = as.date(min(partyData$date):(max(partyData$date) + 1))
        fullData = merge(partyData, data.table(date = dates), by="date", all=TRUE)

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

        # the variances are on the share scale, see kalman()
        trendData[[p]] = data.table(date = dates, party = p, value = c(value),
                                    variance = c(variance)*toProb**2)
    }

    rbindlist(trendData, fill = TRUE)
}
