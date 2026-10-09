#' @import data.table

# Seat projections --------------------------------------------------------------

allocateSeats = function(votes, seats, method) {
    if (sum(votes) <= 0)
        return(integer(length(votes)))

    if (method == 'hare') {
        quotas = votes/sum(votes)*seats
        allocation = floor(quotas)
        remaining = seats - sum(allocation)
        largest = order(quotas - allocation, votes, decreasing = TRUE)[seq_len(remaining)]
        allocation[largest] = allocation[largest] + 1
        return(as.integer(allocation))
    }

    divisors = if (method == 'dhondt') seq_len(seats) else 2*seq_len(seats) - 1
    quotients = outer(votes, divisors, '/')
    # ties are resolved in favour of the larger party
    winners = order(quotients, rep(votes, length(divisors)), decreasing = TRUE)[seq_len(seats)]
    tabulate(row(quotients)[winners], nbins = length(votes))
}

# Seats of every party (columns) in every simulation (rows). The shares are
# drawn independently from normal distributions given by the trend; drawn
# shares below zero count as zero.
simulateSeats = function(shares, variances, seats, threshold, method, simulations) {
    parties = length(shares)
    draws = matrix(stats::rnorm(simulations*parties, shares, sqrt(variances)),
                   nrow = simulations, byrow = TRUE)
    draws[draws < threshold] = 0

    allocations = vapply(seq_len(simulations), function(i) allocateSeats(draws[i, ], seats, method),
                         integer(parties))
    matrix(allocations, nrow = simulations, byrow = TRUE)
}

checkSeatArguments = function(seats, simulations) {
    if (!is.numeric(seats) || length(seats) != 1 || is.na(seats) || seats < 1 || seats %% 1 != 0)
        stop('`seats` must be a positive whole number', call. = FALSE)
    if (!is.numeric(simulations) || length(simulations) != 1 || is.na(simulations) ||
        simulations < 0 || simulations %% 1 != 0)
        stop('`simulations` must be a whole number', call. = FALSE)
}

# Shares (and their variances) seats are projected from: the trend values of
# a popPolls object or a named vector of shares.
seatShares = function(x, trend, date, simulations) {
    if (inherits(x, 'popPolls')) {
        if (identical(x$options$measure, 's'))
            stop('The polls are seat projections already', call. = FALSE)

        current = trendAt(x, trend, date)$current
        shares = merge(current, x$parties[, .(party = code, name)], by = 'party', all.x = TRUE)
        shares = shares[order(-value), .(party, name, share = value, variance)]
    } else {
        if (!is.numeric(x) || is.null(names(x)) || anyNA(names(x)))
            stop('`x` must be a popPolls object or a named numeric vector of shares', call. = FALSE)
        if (simulations > 0)
            stop('Simulations need a popPolls object with a trend that has a variance', call. = FALSE)
        shares = data.table(party = names(x), share = unname(x), variance = NA_real_)
    }

    if (simulations > 0 && anyNA(shares$variance))
        stop(paste('The trend has no variance, which simulations need. Use a trend such as',
                   'popAddTrend(type = "kalman") or simulations = 0'), call. = FALSE)

    shares[is.na(share), share := 0]
}

#' Seat Projection
#'
#' Converts vote shares into seats with a highest averages (D'Hondt,
#' Sainte-Laguë) or largest remainder (Hare-Niemeyer) method. With
#' `simulations`, the uncertainty of the trend is turned into a range of seats.
#'
#' This is a projection on the national level only: regional constituencies,
#' direct mandates, overhang seats and exceptions from the threshold (such as
#' the basic mandate clauses in Germany and Austria) are not taken into
#' account. For example, `seats = 630, threshold = 0.05, method = "sainte-lague"`
#' approximates the German Bundestag, `seats = 183, threshold = 0.04,
#' method = "dhondt"` the Austrian Nationalrat.
#'
#' The simulations draw the share of every party independently from a normal
#' distribution with the value and variance of the trend. Shares of different
#' parties are in fact negatively correlated, so the ranges are approximate.
#' Use [set.seed()] for reproducible results.
#'
#' @param x A `popPolls` object, whose shares are taken from a trend (see
#'   [popLatest()]), or a named numeric vector of vote shares.
#' @param seats Number of seats to distribute.
#' @param threshold Minimum share a party needs to get seats.
#' @param method `"dhondt"`, `"sainte-lague"` or `"hare"`.
#' @param simulations Number of simulations, `0` for none. Needs a trend with a
#'   variance, such as `kalman`.
#' @param level Coverage of the seat range given by `lower` and `upper`.
#' @param trend Name of the trend in `x$trends`, defaults to the one added last.
#' @param date Date of the projection, defaults to the last date of the trend.
#'
#' @return A `data.table` with the columns `party`, `name` (if `x` is a
#'   `popPolls` object), `share` and `seats`, sorted by seats. With
#'   simulations also `lower` and `upper`, the range of seats, and `pSeats`,
#'   the share of simulations in which the party wins seats.
#' @export
#'
#' @examples
#' popSeats(c(A = 0.35, B = 0.30, C = 0.20, D = 0.10, E = 0.05),
#'          seats = 100, threshold = 0.06)
#'
#' \dontrun{
#' de = popRead('DE-parliament')
#' de = popAddTrend(de, name = 'kalman', type = 'kalman')
#' popSeats(de, seats = 630, threshold = 0.05, method = 'sainte-lague',
#'          simulations = 2000)
#' }
popSeats = function(x, seats, threshold = 0, method = c('dhondt', 'sainte-lague', 'hare'),
                    simulations = 0, level = 0.9, trend = NULL, date = NULL) {
    method = match.arg(method)
    checkSeatArguments(seats, simulations)

    shares = seatShares(x, trend, date, simulations)
    votes = shares$share
    votes[votes < threshold] = 0
    allocation = allocateSeats(votes, seats, method)

    result = shares[, setdiff(names(shares), 'variance'), with = FALSE]
    result[, seats := allocation]

    if (simulations > 0) {
        draws = simulateSeats(shares$share, shares$variance, seats, threshold, method, simulations)
        quantiles = apply(draws, 2, stats::quantile, probs = c((1 - level)/2, 1 - (1 - level)/2),
                          type = 1, names = FALSE)
        result[, `:=`(lower = as.integer(quantiles[1, ]), upper = as.integer(quantiles[2, ]),
                      pSeats = colMeans(draws > 0))]
    }

    result[order(-seats, -share)]
}

#' Coalitions
#'
#' Projects the seats of coalitions and the probability that they reach a
#' majority, based on the simulations described in [popSeats()].
#'
#' Without `coalitions`, every combination of up to `size` parties is
#' considered that reaches a majority in at least 1 % of the simulations,
#' unless it contains a smaller combination that has a majority in at least
#' half of them.
#'
#' @inheritParams popSeats
#' @param coalitions List of coalitions, each a character vector of party
#'   codes, e.g. `list(c("OEVP", "SPOE", "NEOS"), c("FPOE", "OEVP"))`. The names
#'   of the list are used as names of the coalitions.
#' @param majority Seats needed for a majority, by default more than half.
#' @param size Largest number of parties in a coalition if `coalitions` is not
#'   given.
#'
#' @return A `data.table` with the columns `coalition`, `parties` (the party
#'   codes, separated by `+`), `seats` (projected from the trend values),
#'   `lower`, `upper` (the range of seats) and `pMajority` (the share of
#'   simulations in which the coalition reaches a majority).
#' @export
#'
#' @examples
#' \dontrun{
#' at = popRead('AT-parliament')
#' at = popAddTrend(at, name = 'kalman', type = 'kalman', args = list(smoothing = TRUE))
#' set.seed(1)
#' popCoalitions(at, seats = 183, threshold = 0.04)
#' popCoalitions(at, seats = 183, threshold = 0.04,
#'               coalitions = list(Zuckerl = c('OEVP', 'SPOE', 'NEOS'),
#'                                 'Blau-Schwarz' = c('FPOE', 'OEVP')))
#' }
popCoalitions = function(x, seats, coalitions = NULL, threshold = 0,
                         method = c('dhondt', 'sainte-lague', 'hare'), simulations = 2000,
                         majority = NULL, level = 0.9, size = 3, trend = NULL, date = NULL) {
    method = match.arg(method)
    checkPopPolls(x, 'x')
    checkSeatArguments(seats, simulations)
    if (simulations == 0)
        stop('Coalitions need simulations', call. = FALSE)
    if (is.null(majority))
        majority = floor(seats/2) + 1

    shares = seatShares(x, trend, date, simulations)
    votes = shares$share
    votes[votes < threshold] = 0
    projected = allocateSeats(votes, seats, method)
    draws = simulateSeats(shares$share, shares$variance, seats, threshold, method, simulations)

    automatic = is.null(coalitions)
    if (automatic) {
        candidates = shares$party[colMeans(draws > 0) > 0]
        coalitions = unlist(lapply(seq_len(min(size, length(candidates))), function(k)
            utils::combn(candidates, k, simplify = FALSE)), recursive = FALSE)
    } else {
        if (!is.list(coalitions) || !all(vapply(coalitions, is.character, logical(1))))
            stop('`coalitions` must be a list of character vectors of party codes', call. = FALSE)
        unknown = setdiff(unlist(coalitions), shares$party)
        if (length(unknown) > 0)
            stop(sprintf('Unknown or no longer polled parties: %s', paste(unknown, collapse = ', ')),
                 call. = FALSE)
    }

    if (length(coalitions) == 0)
        return(data.table(coalition = character(), parties = character(), seats = integer(),
                          lower = integer(), upper = integer(), pMajority = numeric()))

    labels = vapply(coalitions, paste, character(1), collapse = '+')
    coalitionNames = labels
    if (!is.null(names(coalitions)))
        coalitionNames = ifelse(nzchar(names(coalitions)), names(coalitions), labels)
    result = rbindlist(lapply(seq_along(coalitions), function(i) {
        members = match(coalitions[[i]], shares$party)
        total = rowSums(draws[, members, drop = FALSE])
        range = stats::quantile(total, c((1 - level)/2, 1 - (1 - level)/2), type = 1, names = FALSE)
        data.table(coalition = coalitionNames[i], parties = labels[i], seats = sum(projected[members]),
                   lower = as.integer(range[1]), upper = as.integer(range[2]),
                   pMajority = mean(total >= majority))
    }))

    if (automatic) {
        # leave out coalitions that contain a smaller one with a likely majority
        likely = coalitions[result$pMajority >= 0.5]
        redundant = vapply(coalitions, function(members)
            any(vapply(likely, function(smaller) length(smaller) < length(members) &&
                           all(smaller %in% members), logical(1))), logical(1))
        result = result[pMajority >= 0.01 & !redundant][order(-pMajority, -seats)]
    }

    result
}
