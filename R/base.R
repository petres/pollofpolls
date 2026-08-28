#' @import data.table
#' @import graphics


# Parsing helpers -------------------------------------------------------------

isMissing = function(value) {
    if (is.null(value) || length(value) == 0)
        return(TRUE)
    if (length(value) > 1)
        return(FALSE)
    if (is.na(value))
        return(TRUE)

    is.character(value) && (!nzchar(value) || value %in% c('NA', 'null', 'NULL'))
}

# The endpoint encodes missing dates in several ways ('', 'NA', missing key),
# so anything that is not an ISO date is treated as unknown.
safeDate = function(value) {
    if (isMissing(value))
        return(as.Date(NA))

    suppressWarnings(as.Date(as.character(value), format = '%Y-%m-%d'))
}

safeInteger = function(value) {
    if (isMissing(value))
        return(NA_integer_)

    suppressWarnings(as.integer(value))
}

safeNumeric = function(value) {
    if (isMissing(value))
        return(NA_real_)

    suppressWarnings(as.numeric(value))
}

# Percentages are stored as 0-100, seats as absolute numbers.
measureScale = function(pollOptions)
    if (identical(pollOptions$measure, 's')) 1 else 100

partyValues = function(entry, partyCodes, valueScale) {
    values = entry$parties
    result = lapply(partyCodes, function(p) safeNumeric(values[[p]])/valueScale)
    stats::setNames(result, partyCodes)
}


#' Create pop object
#'
#' Bundles polls, parties, elections and trends into the `popPolls` object used
#' by [plot.popPolls()] and [popAddTrend()]. Normally created by [popRead()].
#'
#' @param polls `data.table` of polls, one row per poll and one column per party.
#' @param options List of options as published by the endpoint, notably
#'   `measure` (`"p"` for percentages, `"s"` for seats).
#' @param parties `data.table` with the columns `code`, `name` and `color`.
#' @param trends Named list of trends, each a long `data.table` with the
#'   columns `date`, `party` and `value`.
#' @param name Name of the poll, used as plot title.
#' @param elections `data.table` of election results in the same shape as `polls`.
#'
#' @return An object of class `popPolls`.
#' @export
#'
#' @examples
#' popCreate()
popCreate = function(polls = data.table(), options = list(measure = 'p'), parties = data.table(),
                     trends = list(), name = NULL, elections = data.table()) {
    r = list(
        polls = polls,
        options = options,
        parties = parties,
        trends = trends,
        name = name,
        elections = elections
    )

    class(r) <- c("popPolls", class(r))

    return (r)
}

#' Read Poll Data
#'
#' Downloads a single Poll of Polls data set from POLITICO.
#'
#' Party colours and the descriptive name are not part of the data endpoint and
#' are looked up on the corresponding website. That lookup needs one additional
#' request, is cached (see [popCacheClear()]) and can be switched off with
#' `metadata = FALSE`.
#'
#' @param code Code of the poll data, e.g. `"DE-parliament"`. See [popGetInfo()]
#'   for the available codes.
#' @param load Which parts to load: any of `"polls"`, `"elections"` and
#'   `"trends"` (the trends already published by POLITICO).
#' @param metadata Whether party colours and the descriptive name should be
#'   looked up on the website.
#'
#' @return A `popPolls` object. `$polls` holds one row per poll with the columns
#'   `date`, `dateFrom`, `firm`, `n` (sample size) and one column per party,
#'   `$elections` the same for election results, `$parties` the party codes,
#'   names and colours and `$trends` the published trends in long format.
#' @export
#'
#' @examples
#' \dontrun{
#' de = popRead('DE-parliament')
#' plot(de)
#' }
popRead = function(code, load = c('polls', 'elections', 'trends'), metadata = TRUE) {
    load = unique(match.arg(load, several.ok = TRUE))
    raw = jsonlite::fromJSON(fetchUrl(sprintf(.baseEndpoint, code)), simplifyVector = FALSE)

    if (!is.list(raw) || is.null(raw$parties))
        stop(sprintf("No poll data available for code '%s', see popGetInfo()", code), call. = FALSE)

    pollOptions = if (is.null(raw$options)) list(measure = 'p') else raw$options
    valueScale = measureScale(pollOptions)

    partyCodes = names(raw$parties)
    ordered = unlist(pollOptions$parties)
    if (!is.null(ordered))
        partyCodes = c(intersect(ordered, partyCodes), setdiff(partyCodes, ordered))

    parties = data.table(code = partyCodes,
                         name = unname(unlist(raw$parties)[partyCodes]),
                         color = NA_character_)

    metaRow = emptyMetadata()
    if (metadata)
        metaRow = tryCatch(getCodeMetadata(code), error = function(e) {
            warning(sprintf('Could not look up party colours: %s', conditionMessage(e)),
                    call. = FALSE)
            emptyMetadata()
        })

    if (nrow(parties) > 0 && nrow(metaRow) > 0) {
        colors = metaRow$colors[[1]]
        if (length(colors) > 0)
            parties[, color := unname(colors[code])]
    }
    noColor = is.na(parties$color) | !nzchar(parties$color)
    if (any(noColor))
        parties$color[noColor] = grDevices::hcl.colors(sum(noColor), palette = 'Dark 3')

    polls = data.table()
    if ('polls' %in% load && length(raw$polls) > 0) {
        polls = rbindlist(lapply(raw$polls, function(entry) {
            date = safeDate(entry$date)
            dateFrom = safeDate(entry$date_from)
            c(list(date = date,
                   dateFrom = if (is.na(dateFrom)) date else dateFrom,
                   firm = if (isMissing(entry$firm)) NA_character_ else as.character(entry$firm),
                   n = safeInteger(entry$sample_size)),
              partyValues(entry, partyCodes, valueScale))
        }), fill = TRUE)
        setorder(polls, 'date')
    }

    elections = data.table()
    if ('elections' %in% load && length(raw$results) > 0) {
        elections = rbindlist(lapply(raw$results, function(entry)
            c(list(date = safeDate(entry$date)),
              partyValues(entry, partyCodes, valueScale))), fill = TRUE)
        setorder(elections, 'date')
    }

    trends = list()
    if ('trends' %in% load && length(raw$trends) > 0) {
        for (trendName in names(raw$trends)) {
            entries = raw$trends[[trendName]]
            trend = rbindlist(lapply(entries, function(entry) {
                if (length(entry$parties) == 0)
                    return(NULL)
                values = unlist(entry$parties)
                data.table(date = safeDate(entry$date),
                           party = names(values),
                           value = safeNumeric(values)/valueScale)
            }), fill = TRUE)

            if (nrow(trend) > 0)
                trends[[trendName]] = setorder(trend, 'date', 'party')
        }
    }

    name = code
    if (nrow(metaRow) > 0 && !isMissing(metaRow$title[[1]]))
        name = metaRow$title[[1]]
    else if (!isMissing(pollOptions$iso2))
        name = paste(pollOptions$iso2, code, sep = ' - ')

    popCreate(polls, pollOptions, parties, trends, name = name, elections = elections)
}

#' Get Info About Available Polls
#'
#' Lists the polls published by POLITICO. Building the list means visiting every
#' country page once, which takes a while; the result is therefore cached for a
#' day (see [popCacheClear()]).
#'
#' @param refresh Whether the cached list should be rebuilt.
#'
#' @return A `data.table` with the columns `code`, `iso2`, `name`, `page` and
#'   `endpoint`.
#' @export
#'
#' @examples
#' \dontrun{
#' popGetInfo()
#' }
popGetInfo = function(refresh = FALSE) {
    metadata = getPollMetadata(refresh = refresh)
    if (nrow(metadata) == 0)
        return(data.table())

    info = metadata[, .(code = code,
                        iso2 = sub('-.*', '', code),
                        name = ifelse(is.na(title) | !nzchar(title), code, title),
                        page = url,
                        endpoint = sprintf(.baseEndpoint, code))]
    setorder(info, 'code')
    info
}

#' Plot polls
#'
#' Draws the individual polls as points and every trend added with
#' [popAddTrend()] (or already published by POLITICO) as a line.
#'
#' @param x A `popPolls` object.
#' @param ... Passed on to [graphics::plot()], e.g. `xlim` to limit the date
#'   range. The y axis is scaled to the polls inside `xlim`.
#'
#' @return Invisibly `x`.
#' @export
#'
#' @examples
#' \dontrun{
#' de = popRead('DE-parliament')
#' plot(de)
#' plot(de, xlim = as.Date(c('2024-01-01', '2025-01-01')))
#' }
plot.popPolls = function(x, ...) {
    data = x
    pollsExisting = nrow(data$polls) > 0
    trendsExisting = length(data$trends) > 0
    if (!pollsExisting && !trendsExisting)
        stop('No trend and no polls to plot', call. = FALSE)

    dots = list(...)
    pollsLong = if (pollsExisting) toLong(data) else NULL
    trendsLong = if (trendsExisting) rbindlist(data$trends, fill = TRUE) else NULL

    # c() drops the Date class when the first argument is NULL, so the range is
    # converted back explicitly
    dates = c(pollsLong$date, trendsLong$date)
    xlim = if (is.null(dots$xlim)) as.date(range(dates)) else as.Date(dots$xlim)

    inRange = function(d) if (is.null(d)) NULL else d[d$date >= xlim[1] & d$date <= xlim[2]]
    values = c(inRange(pollsLong)$value, inRange(trendsLong)$value)
    if (length(values) == 0 || all(is.na(values)))
        values = c(pollsLong$value, trendsLong$value)
    ylim = c(0, max(values, na.rm = TRUE)*1.25)

    stdPlotArgs = list(NULL,
        type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
        xlim = xlim, ylim = ylim, main = data$name
    )

    args = utils::modifyList(stdPlotArgs, dots)
    do.call(plot, args)

    xLabelsCount = 8
    diff = (args$xlim[2] - args$xlim[1])/xLabelsCount
    xLabels = args$xlim[1] + 0:xLabelsCount*diff

    if (identical(data$options$measure, 's')) {
        axis(2, at = pretty(args$ylim), labels = pretty(args$ylim), las = TRUE)
    } else {
        axis(2, at = pretty(args$ylim), labels = paste(pretty(args$ylim) * 100, '%'), las = TRUE)
    }
    axis(1, at = xLabels, labels = format(xLabels, "%d. %b '%y"), cex.axis = .7, las = 2)

    alphaPoints = 'AA'
    if (trendsExisting) {
        alphaPoints = '55'
        for (i in seq_along(data$trends)) {
            trend = data$trends[[i]]
            for (p in data$parties$code) {
                linePoints = trend[party == p, .(date, value)]
                if (nrow(linePoints) > 0)
                    lines(linePoints, col = data$parties[code == p]$color, lty = i)
            }
        }

        if (length(data$trends) > 1)
            legend('topright', legend = names(data$trends), lty = seq_along(data$trends),
                   bty = 'n', cex = 0.75, ncol = 1)
    }

    if (pollsExisting) {
        for (p in data$parties$code) {
            pollPoints = pollsLong[party == p, .(date, value)]
            if (nrow(pollPoints) > 0)
                points(pollPoints, col = paste0(data$parties[code == p]$color, alphaPoints),
                       pch = 20, cex = 0.5)
        }
    }

    shown = data$parties$code %in% c(as.character(pollsLong$party), as.character(trendsLong$party))
    if (any(shown))
        legend('topleft', legend = data$parties$name[shown], fill = data$parties$color[shown],
               bty = 'n', cex = 0.75, ncol = 2)

    invisible(x)
}


#' Print popPolls object
#'
#' @param x A `popPolls` object.
#' @param ... Ignored.
#'
#' @return Invisibly `x`.
#' @export
#'
#' @examples
#' print(popCreate())
print.popPolls = function(x, ...) {
    if (!is.null(x$name))
        cat(x$name, '\n\n')

    cat('Polls:\n\n')
    print(x$polls, row.names = FALSE)
    cat('\n')
    if (nrow(x$elections) > 0)
        cat('Elections:', format(x$elections$date), '\n')
    if (length(x$trends) > 0)
        cat('Trends:', paste(names(x$trends), collapse = ', '), '\n')

    cat('\n')
    invisible(x)
}


#' Add Trend to Polls
#'
#' Calculates a poll aggregation and stores it in the `popPolls` object, where
#' it is picked up by [plot.popPolls()].
#'
#' @param data A `popPolls` object.
#' @param name Name of the trend. Defaults to a name built from `type` and the
#'   applied interpolations.
#' @param type Trend function, see details.
#' @param args Arguments passed on to the trend function.
#' @param interpolations Named list of interpolations that should be applied to
#'   the trend, see details.
#'
#' @return The `popPolls` object with the trend added to `$trends`.
#' @export
#'
#' @details
#' Available trend functions are:
#'
#' \describe{
#'   \item{`kalman`}{Kalman filter, arguments: `sd = 0.003`.}
#'   \item{`kalmanKFAS`}{Kalman filter based on the \pkg{KFAS} package,
#'     arguments: `sd = 0.003`, `smoothing = TRUE`.}
#'   \item{`weightedMeanLastDays`}{Linearly weighted rolling mean, arguments:
#'     `days = 30`, `maxObs = Inf`.}
#'   \item{`ident`}{Plain mean of all polls published on the same day, no
#'     arguments.}
#' }
#'
#' Available interpolations are:
#'
#' \describe{
#'   \item{`lastInterpolation`}{Carries the last value forward, no arguments.}
#'   \item{`linearInterpolation`}{Linear interpolation, no arguments.}
#'   \item{`bernoulliConvInterpolation`}{Binomial smoothing, arguments:
#'     `n = 20`, `k = 6`.}
#' }
#'
#' @examples
#' \dontrun{
#' de = popRead('DE-parliament')
#' de = popAddTrend(de, name = 'Kalman 0.003', type = 'kalman', args = list(sd = 0.003))
#' de = popAddTrend(de, name = 'Kalman Raw', type = 'kalman', args = list(sd = 0.003),
#'                  interpolations = list('lastInterpolation' = list()))
#' plot(de)
#' }
popAddTrend = function(data, name = NULL,
                       type = 'kalman', args = list(),
                       interpolations = list()) {
    if (!inherits(data, 'popPolls'))
        stop('`data` must be a popPolls object, see popRead()', call. = FALSE)
    if (!type %in% .trendFunctions)
        stop(sprintf("Unknown trend type '%s', available are: %s",
                     type, paste(.trendFunctions, collapse = ', ')), call. = FALSE)

    unknown = setdiff(names(interpolations), .interpolationFunctions)
    if (length(unknown) > 0)
        stop(sprintf('Unknown interpolation(s): %s, available are: %s',
                     paste(unknown, collapse = ', '),
                     paste(.interpolationFunctions, collapse = ', ')), call. = FALSE)

    if (is.null(args$data))
        args$data = data

    if ((nrow(args$data$polls) + nrow(args$data$elections)) == 0)
        stop('No polls', call. = FALSE)

    trendName = type
    trend = do.call(type, args)

    for (i in seq_along(interpolations)) {
        trendName = paste(trendName, names(interpolations)[i], sep = "-")
        interpolationArgs = interpolations[[i]]
        interpolationArgs$trend = trend
        trend = do.call(names(interpolations)[i], interpolationArgs)
    }

    if (is.null(name))
        name = trendName

    if (nrow(trend) == 0)
        stop(sprintf("Trend '%s' could not be calculated from the given polls", type), call. = FALSE)

    data$trends[[name]] = trend[order(date)]
    return (data)
}
