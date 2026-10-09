#' Accuracy at Past Elections
#'
#' Compares trends and polling firms with the results of the elections in
#' `x$elections`: how far was each trend, and the last poll of each firm, from
#' the result shortly before the election?
#'
#' For every election the trends are calculated from scratch, using only the
#' polls published at least `days` before the election and the earlier
#' elections. The comparison is therefore out of sample: even smoothed trends do
#' not know the result, which makes it possible to compare trend types and
#' their arguments, e.g. different values of `sd`.
#'
#' @param x A `popPolls` object.
#' @param trends Named list of trends to evaluate, each a list of arguments for
#'   [popAddTrend()]: `type`, `args`, `interpolations` and `houseEffects`.
#' @param days Days before the election that are evaluated.
#' @param firms Whether the polling firms should be evaluated as well.
#' @param window Only the polls of a firm published in this many days before
#'   the election are taken into account.
#'
#' @return A `data.table` with one row per election, trend or firm and party,
#'   and the columns `election` (date), `kind` (`"trend"` or `"firm"`),
#'   `source` (name of the trend or firm), `party`, `estimate`, `result` and
#'   `error` (`estimate - result`).
#' @export
#'
#' @examples
#' \dontrun{
#' de = popRead('DE-parliament')
#' accuracy = popAccuracy(de, trends = list(
#'     'sd 0.001' = list(type = 'kalman', args = list(sd = 0.001)),
#'     'sd 0.003' = list(type = 'kalman', args = list(sd = 0.003)),
#'     'mean 30d' = list(type = 'weightedMeanLastDays')
#' ))
#' # mean absolute error, in percentage points
#' accuracy[, .(elections = uniqueN(election), mae = 100*mean(abs(error))),
#'          by = .(kind, source)][order(mae)]
#' }
popAccuracy = function(x, trends = list(kalman = list(type = 'kalman')), days = 1,
                       firms = TRUE, window = 30) {
    checkPopPolls(x, 'x')
    checkTrendSpecifications(trends)

    empty = data.table(election = as.Date(character()), kind = character(), source = character(),
                       party = character(), estimate = numeric(), result = numeric(),
                       error = numeric())
    if (nrow(x$polls) == 0)
        return(empty)

    elections = toLong(x, 'elections')
    rows = list()
    add = function(compared, ...)
        if (nrow(compared) > 0)
            rows[[length(rows) + 1]] <<- data.table(..., compared)
    for (election in sort(unique(elections$date))) {
        election = as.date(election)
        cutoff = election - days
        result = elections[date == election, .(party, result = value)]

        before = x
        before$polls = x$polls[date <= cutoff]
        before$elections = x$elections[date < election]
        before$trends = list()
        recent = before$polls[date > cutoff - .activeDays]
        if (nrow(recent) == 0)
            next

        for (name in names(trends)) {
            trend = tryCatch(do.call(popAddTrend, c(list(data = before, name = 'trend'), trends[[name]])),
                             error = function(e) NULL)
            if (is.null(trend))
                next

            estimate = valuesAt(trend$trends$trend, cutoff)[, .(party, estimate = value)]
            add(merge(estimate, result, by = 'party'), election = election, kind = 'trend',
                source = name)
        }

        if (firms && 'firm' %in% names(recent)) {
            polls = toLong(before)[date > cutoff - window & !is.na(firm)]
            if (nrow(polls) > 0) {
                # the last poll of every firm, averaged if it published several that day
                polls = polls[polls[, .I[date == max(date)], by = firm]$V1]
                estimate = polls[, .(estimate = mean(value)), by = .(source = firm, party)]
                add(merge(estimate, result, by = 'party'), election = election, kind = 'firm')
            }
        }
    }

    if (length(rows) == 0)
        return(empty)

    accuracy = rbindlist(rows, use.names = TRUE)
    accuracy[, error := estimate - result]
    setcolorder(accuracy, c('election', 'kind', 'source', 'party', 'estimate', 'result', 'error'))
    accuracy[order(election, kind, source, party)]
}

checkTrendSpecifications = function(trends) {
    allowed = c('type', 'args', 'interpolations', 'houseEffects')
    valid = is.list(trends) && length(trends) > 0 && !is.null(names(trends)) &&
        all(nzchar(names(trends))) && !anyDuplicated(names(trends)) &&
        all(vapply(trends, function(t) is.list(t) && all(names(t) %in% allowed), logical(1)))
    if (!valid)
        stop(sprintf('`trends` must be a named list of lists with the elements %s',
                     paste(allowed, collapse = ', ')), call. = FALSE)
}
