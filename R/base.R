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

# Single JSON value as a string, NA if missing.
textValue = function(value)
    if (isMissing(value)) NA_character_ else as.character(value)[[1]]

# Removes invisible characters (soft hyphens, zero width spaces), collapses runs
# of white space (including tabs and non-breaking spaces) and trims the result.
# Firm names are published as "Forsa", "Forsa " and "Forsa\t", which would
# otherwise count as three different firms.
cleanText = function(x) {
    # removed literally: PCRE rejects code points above 255 in patterns unless
    # the input happens to contain non-ASCII characters
    for (invisible in c('\u00ad', '\u200b', '\u200c', '\u200d', '\ufeff'))
        x = gsub(invisible, '', x, fixed = TRUE)
    x = trimws(gsub('[\\h\\v]+', ' ', x, perl = TRUE))
    x[!is.na(x) & !nzchar(x)] = NA_character_
    x
}

# Percentages are stored as 0-100, seats as absolute numbers.
measureScale = function(pollOptions)
    if (identical(pollOptions$measure, 's')) 1 else 100

# The parsers below build whole columns at once: the payloads hold up to
# several hundred thousand values, which is too many to go through
# data.table() or rbindlist() one entry at a time.

fieldValues = function(entries, name)
    vapply(entries, function(entry) textValue(entry[[name]]), character(1))

partyColumns = function(entries, partyCodes, valueScale) {
    columns = lapply(partyCodes, function(p)
        vapply(entries, function(entry) safeNumeric(entry$parties[[p]]), numeric(1))/valueScale)
    stats::setNames(columns, partyCodes)
}

parsePolls = function(entries, partyCodes, valueScale) {
    if (length(entries) == 0)
        return(data.table())

    polls = data.table(date = safeDate(fieldValues(entries, 'date')),
                       dateFrom = safeDate(fieldValues(entries, 'date_from')),
                       firm = cleanText(fieldValues(entries, 'firm')),
                       n = safeInteger(fieldValues(entries, 'sample_size')))
    if (length(partyCodes) > 0)
        polls[, (partyCodes) := partyColumns(entries, partyCodes, valueScale)]

    # a poll without publication date is dated by the start of its fieldwork
    polls[is.na(date), date := dateFrom]
    polls[is.na(dateFrom), dateFrom := date]

    undated = is.na(polls$date)
    if (any(undated)) {
        warning(sprintf('Dropped %d poll(s) without a date', sum(undated)), call. = FALSE)
        polls = polls[!undated]
    }

    setorder(polls, 'date')
}

parseElections = function(entries, partyCodes, valueScale) {
    if (length(entries) == 0)
        return(data.table())

    elections = data.table(date = safeDate(fieldValues(entries, 'date')))
    if (length(partyCodes) > 0)
        elections[, (partyCodes) := partyColumns(entries, partyCodes, valueScale)]

    setorder(elections[!is.na(date)], 'date')
}

parseTrend = function(entries, valueScale) {
    values = lapply(entries, function(entry) unlist(entry$parties))
    counts = lengths(values)
    if (sum(counts) == 0)
        return(emptyLong())

    values = unlist(unname(values))
    trend = data.table(date = rep(safeDate(fieldValues(entries, 'date')), counts),
                       party = names(values),
                       value = safeNumeric(unname(values))/valueScale)

    setorder(trend[!is.na(date)], 'date', 'party')
}

readPayload = function(code, dir) {
    file = file.path(dir, paste0(code, '.json'))
    if (!file.exists(file))
        stop(sprintf("No file '%s', see popDownload()", file), call. = FALSE)

    content = readChar(file, file.size(file), useBytes = TRUE)
    Encoding(content) = 'UTF-8'
    list(content = content, retrieved = file.mtime(file))
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
#'   columns `date`, `party`, `value` and optionally `variance`.
#' @param name Name of the poll, used as plot title.
#' @param elections `data.table` of election results in the same shape as `polls`.
#' @param code Poll code the data was read for.
#' @param retrieved Time the data was downloaded.
#'
#' @return An object of class `popPolls`.
#' @export
#'
#' @examples
#' popCreate()
popCreate = function(polls = data.table(), options = list(measure = 'p'), parties = data.table(),
                     trends = list(), name = NULL, elections = data.table(),
                     code = NULL, retrieved = NULL) {
    r = list(
        polls = polls,
        options = options,
        parties = parties,
        trends = trends,
        name = name,
        elections = elections,
        code = code,
        retrieved = retrieved
    )

    class(r) <- c("popPolls", class(r))

    return (r)
}

#' Read Poll Data
#'
#' Downloads a single Poll of Polls data set from POLITICO, or reads one saved
#' by [popDownload()].
#'
#' Party colours and the descriptive name are not part of the data endpoint and
#' are looked up on the corresponding website. That lookup needs one additional
#' request, is cached (see [popCacheClear()]) and can be switched off with
#' `metadata = FALSE`.
#'
#' Downloaded data is not cached unless `options(pollofpolls.dataMaxAge)` is set
#' to the number of seconds a download may be reused.
#'
#' @param code Code of the poll data, e.g. `"DE-parliament"`. See [popGetInfo()]
#'   for the available codes.
#' @param load Which parts to load: any of `"polls"`, `"elections"` and
#'   `"trends"` (the trends already published by POLITICO).
#' @param metadata Whether party colours and the descriptive name should be
#'   looked up on the website.
#' @param dir Directory with the files written by [popDownload()]. If given,
#'   the data is read from `<dir>/<code>.json` instead of being downloaded.
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
#'
#' # read data saved by popDownload()
#' popDownload('polls', codes = 'AT-parliament')
#' at = popRead('AT-parliament', dir = 'polls', metadata = FALSE)
#' }
popRead = function(code, load = c('polls', 'elections', 'trends'), metadata = TRUE, dir = NULL) {
    if (!is.character(code) || length(code) != 1 || is.na(code) || !nzchar(code))
        stop('`code` must be a single poll code such as "DE-parliament", use lapply() to read several',
             call. = FALSE)
    load = unique(match.arg(load, several.ok = TRUE))

    payload = if (!is.null(dir)) readPayload(code, dir) else tryCatch(fetchData(code),
        pollofpolls_http_error = function(e) {
            if (identical(e$status, 404L))
                stop(sprintf("No poll data available for code '%s', see popGetInfo()", code), call. = FALSE)
            stop(e)
        })

    raw = tryCatch(jsonlite::parse_json(payload$content, simplifyVector = FALSE),
                   error = function(e)
                       stop(sprintf("Could not parse the data of '%s': %s", code, conditionMessage(e)),
                            call. = FALSE))
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
    if ('polls' %in% load)
        polls = parsePolls(raw$polls, partyCodes, valueScale)

    elections = data.table()
    if ('elections' %in% load)
        elections = parseElections(raw$results, partyCodes, valueScale)

    trends = list()
    if ('trends' %in% load && length(raw$trends) > 0) {
        for (trendName in names(raw$trends)) {
            trend = parseTrend(raw$trends[[trendName]], valueScale)
            if (nrow(trend) > 0)
                trends[[trendName]] = trend
        }
    }

    name = code
    if (nrow(metaRow) > 0 && !isMissing(metaRow$title[[1]]))
        name = metaRow$title[[1]]
    else if (!isMissing(pollOptions$iso2))
        name = paste(pollOptions$iso2, code, sep = ' - ')

    popCreate(polls, pollOptions, parties, trends, name = name, elections = elections,
              code = code, retrieved = payload$retrieved)
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
                        endpoint = endpointUrl(code))]
    setorder(info, 'code')
    info
}

#' Plot polls
#'
#' Draws the individual polls as points and every trend added with
#' [popAddTrend()] (or already published by POLITICO) as a line. Trends that
#' come with a variance, such as `kalman`, are drawn with an uncertainty band.
#'
#' @param x A `popPolls` object.
#' @param ... Passed on to [graphics::plot()], e.g. `xlim` to limit the date
#'   range. `xlim` takes dates or ISO date strings, `NA` keeps the respective
#'   end of the data range. The y axis is scaled to the polls inside `xlim`.
#' @param bands Whether uncertainty bands should be drawn.
#' @param level Coverage of the uncertainty bands.
#'
#' @return Invisibly `x`.
#' @export
#'
#' @examples
#' \dontrun{
#' de = popRead('DE-parliament')
#' de = popAddTrend(de, name = 'kalman', type = 'kalman')
#' plot(de)
#' plot(de, xlim = c('2024-01-01', NA), level = 0.9)
#' }
plot.popPolls = function(x, ..., bands = TRUE, level = 0.95) {
    data = x
    pollsExisting = nrow(data$polls) > 0
    trendsExisting = length(data$trends) > 0
    if (!pollsExisting && !trendsExisting)
        stop('No trend and no polls to plot', call. = FALSE)

    dots = list(...)
    pollsLong = if (pollsExisting) toLong(data) else NULL
    trendsLong = if (trendsExisting) rbindlist(data$trends, fill = TRUE) else NULL

    xlim = dateLimits(c(pollsLong$date, trendsLong$date), dots$xlim)
    dots$xlim = xlim

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

    partyColor = function(p, alpha = 1)
        grDevices::adjustcolor(data$parties[code == p]$color, alpha.f = alpha)

    alphaPoints = 0.67
    if (trendsExisting) {
        alphaPoints = 0.33

        # all bands first, so that they do not cover the lines of other trends
        if (bands) {
            for (trend in data$trends) {
                bounds = trendBounds(trend, level)
                for (p in data$parties$code) {
                    band = bounds[party == p & !is.na(lower)]
                    if (nrow(band) > 1)
                        polygon(c(band$date, rev(band$date)), c(band$upper, rev(band$lower)),
                                col = partyColor(p, 0.2), border = NA)
                }
            }
        }

        for (i in seq_along(data$trends)) {
            trend = data$trends[[i]]
            for (p in data$parties$code) {
                linePoints = trend[party == p, .(date, value)]
                if (nrow(linePoints) > 0)
                    lines(linePoints, col = partyColor(p), lty = i)
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
                points(pollPoints, col = partyColor(p, alphaPoints), pch = 20, cex = 0.5)
        }
    }

    # parties that are not polled any more are left out of the legend
    visible = c(inRange(pollsLong)$party, inRange(trendsLong)$party)
    shown = data$parties$code %in% visible
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
        cat(x$name, '\n')
    if (!is.null(x$retrieved))
        cat('Retrieved:', format(x$retrieved, '%Y-%m-%d %H:%M'), '\n')

    cat('\nPolls:\n\n')
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
#' @param type Name of a trend function (see details) or a function.
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
#'   \item{`kalman`}{Kalman filter, arguments: `sd = 0.003`, the daily standard
#'     deviation of the true support on the share scale, and
#'     `smoothing = FALSE`. With `smoothing = TRUE` every estimate takes the
#'     later polls into account as well (Rauch-Tung-Striebel smoother). The
#'     estimates are calculated for the dates with polls only; together with
#'     `linearInterpolation` the smoothed trend reproduces POLITICO's daily
#'     `kalmanSmooth` trend.}
#'   \item{`kalmanKFAS`}{Kalman filter based on the \pkg{KFAS} package,
#'     arguments: `sd = 0.003`, `smoothing = TRUE`.}
#'   \item{`weightedMeanLastDays`}{Linearly weighted rolling mean, arguments:
#'     `days = 30`, `maxObs = Inf`.}
#'   \item{`ident`}{Plain mean of all polls published on the same day, no
#'     arguments.}
#' }
#'
#' Instead of a name, `type` can be a function. It is called with the
#' `popPolls` object as argument `data` and the elements of `args`, and has to
#' return a `data.frame` with the columns `date`, `party` (the codes of
#' `data$parties`) and `value`, plus optionally `variance`, which is used for
#' the uncertainty bands.
#'
#' Available interpolations are:
#'
#' \describe{
#'   \item{`lastInterpolation`}{Carries the last value forward, no arguments.}
#'   \item{`linearInterpolation`}{Linear interpolation, no arguments.}
#'   \item{`bernoulliConvInterpolation`}{Binomial smoothing over consecutive
#'     trend values, arguments: `n = 20`, `k = 6`.}
#' }
#'
#' Interpolations keep the `variance` of a trend, which is interpolated (and
#' smoothed) like the values; between two dates with polls it is therefore an
#' approximation.
#'
#' @examples
#' \dontrun{
#' de = popRead('DE-parliament')
#' de = popAddTrend(de, name = 'Kalman 0.003', type = 'kalman', args = list(sd = 0.003))
#' de = popAddTrend(de, name = 'Kalman smoothed', type = 'kalman',
#'                  args = list(smoothing = TRUE),
#'                  interpolations = list('linearInterpolation' = list()))
#' plot(de)
#'
#' # a custom trend: the median of the polls of the last 14 days
#' rollingMedian = function(data, days = 14) {
#'     polls = popLong(data)
#'     dates = seq(min(polls$date), max(polls$date), by = 'day')
#'     polls[, .(date = dates,
#'               value = vapply(dates, function(d) median(value[date > d - days & date <= d]),
#'                              numeric(1))), by = party]
#' }
#' de = popAddTrend(de, type = rollingMedian, args = list(days = 21))
#' }
popAddTrend = function(data, name = NULL,
                       type = 'kalman', args = list(),
                       interpolations = list()) {
    checkPopPolls(data, 'data')

    if (is.function(type)) {
        expression = substitute(type)
        trendName = if (is.symbol(expression)) as.character(expression) else 'custom'
    } else {
        if (!is.character(type) || length(type) != 1 || !type %in% .trendFunctions)
            stop(sprintf("Unknown trend type '%s', available are: %s, or pass a function",
                         paste(format(type), collapse = ' '),
                         paste(.trendFunctions, collapse = ', ')), call. = FALSE)
        trendName = type
    }

    unknown = setdiff(names(interpolations), .interpolationFunctions)
    if (length(unknown) > 0)
        stop(sprintf('Unknown interpolation(s): %s, available are: %s',
                     paste(unknown, collapse = ', '),
                     paste(.interpolationFunctions, collapse = ', ')), call. = FALSE)

    if (is.null(args$data))
        args$data = data

    if ((nrow(args$data$polls) + nrow(args$data$elections)) == 0)
        stop('No polls', call. = FALSE)

    trend = checkTrend(do.call(type, args), trendName)

    for (i in seq_along(interpolations)) {
        trendName = paste(trendName, names(interpolations)[i], sep = "-")
        interpolationArgs = interpolations[[i]]
        interpolationArgs$trend = trend
        trend = do.call(names(interpolations)[i], interpolationArgs)
    }

    if (is.null(name))
        name = trendName

    if (nrow(trend) == 0)
        stop(sprintf("Trend '%s' could not be calculated from the given polls", trendName), call. = FALSE)

    data$trends[[name]] = trend[order(date)]
    return (data)
}
