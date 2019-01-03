#' @import data.table
#' @import graphics

# URLs
baseUrl = 'https://pollofpolls.eu'
basePollsLoadUrl = paste(baseUrl, "get/polls/%CODE%/format/csv", sep="/")
baseInfoLoadUrl = paste(baseUrl, "get/info/%CODE%", sep="/")


#' Create pop object
#'
#' @param polls polls
#' @param options options
#' @param parties parties
#' @param trends trends
#' @param name name
#' @param elections elections
#'
#' @return pop object
#' @export
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
#' @param code Code of the poll data.
#' @param load what to load.
#'
#' @return poll data
#' @export
#'
#' @examples
#' popRead('DE-parliament')
popRead = function(code, load = c('polls', 'elections')) {
    n = NULL; sd = NULL # WARNINGS
    info = rjson::fromJSON(readLines(gsub("%CODE%", code, baseInfoLoadUrl), warn=FALSE))

    parties = data.table()
    for (c in names(info$parties)) {
        e = info$parties[[c]]
        e$code = c
        parties = rbind(parties, as.data.table(e), fill=TRUE)
    }

    polls = data.table()
    if ('polls' %in% load) {
        if (info$count > 0) {
            t = gsub("%CODE%", code, basePollsLoadUrl)
            polls = as.data.table(utils::read.csv(url(t), as.is='source', colClasses=c('date' = 'Date')))
            polls[, `:=`(source = NULL,
                         n = as.integer(n),
                         sd = as.numeric(sd))]
            for (p in parties$code) {
                if (p %in% colnames(polls))
                    polls[[p]] = as.numeric(polls[[p]]/100)
                else
                    polls[[p]] = as.numeric(NA)
            }
        }
    }

    elections = data.table()
    if ('elections' %in% load) {
        for (e in info$elections) {
            elections = rbind(elections, cbind(date = as.Date(e$date), as.data.table(e$parties)), fill=TRUE)
        }
        for (p in intersect(parties$code, colnames(elections))) {
            elections[[p]] = as.numeric(elections[[p]]/100)
        }
    }

    return (popCreate(polls, info$options, parties, name=paste(info$iso2, info$name, sep=" - "), elections=elections))
}

#' Get Info About Available Polls
#'
#'
#' @return data table with infos
#' @export
popGetInfo = function() {
    code = NULL # WARNINGS
    t = rjson::fromJSON(readLines(paste(baseUrl, 'get/info', sep="/"), warn=FALSE))
    r = data.table()
    for (c in names(t)) {
        e = t[[c]]
        e$parties = NULL
        e$code = c
        r = rbind(r, as.data.table(e), fill=TRUE)
    }
    return (r[order(code)])
}

#' Plot polls
#'
#' @param x Polls Data
#' @param ... xlim: Limit date range and plot default args
#'
#' @export
#'
#' @examples
#' t = popRead('DE-parliament')
#' plot(t)
#' plot(t, xlim=as.Date(c('2018-01-01', '2018-05-01')))
plot.popPolls = function(x, ...) {
    party = NULL; code = NULL; . = NULL; value = NULL # WARNINGS
    data = x
    pollsExisting = nrow(data$polls) > 0
    trendsExisting = length(data$trends) > 0
    dots = list(...)
    xlim = dots$xlim

    if (pollsExisting) {
        pollsLong = toLong(data)
        xlim = c(min(data$polls$date), max(data$polls$date))
        ylim = c(0, max(pollsLong$value, na.rm=TRUE)*1.25)
    } else {
        if (trendsExisting) {
            trend = data$trends[[1]]
            xlim = c(min(trend$date), max(trend$date))
            ylim = c(0, max(trend$value, na.rm=TRUE)*1.25)
        } else {
            stop('No trend and no polls to plot')
        }
    }

    stdPlotArgs = list(NULL,
        type="n", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
        xlim = xlim, ylim = ylim, main = data$name
    )

    args = utils::modifyList(stdPlotArgs, dots)
    do.call(plot, args)

    xLabelsCount = 8
    diff = (args$xlim[2] - args$xlim[1])/xLabelsCount
    xLabels = args$xlim[1] + 0:xLabelsCount*diff

    if (data$options$measure == 's') {
        axis(2, at=pretty(args$ylim), labels=pretty(args$ylim), las=TRUE)
    } else {
        axis(2, at=pretty(args$ylim), labels=paste(pretty(args$ylim) * 100, '%'), las=TRUE)
    }
    axis(1, at=xLabels, labels=format(xLabels, "%d. %b '%y"), cex.axis = .7, las = 2)


    alphaPoints = 'AA'
    if (trendsExisting) {
        alphaPoints = '55'
        for (i in 1:length(data$trends)) {
            t = names(data$trends)[i]
            for (p in  data$parties$code) {
                linePoints = data$trends[[t]][party == p, c('date', 'value'), with=FALSE]
                lines(linePoints,
                      col=data$parties[code == p]$color,
                      lty=i)
            }
        }

        if (length(data$trends) > 1) {
            legend('topright', legend=names(data$trends), lty=1:length(data$trends), bty='n', cex=0.75, ncol=1)
        }
    }

    if (pollsExisting) {
        for (p in data$parties$code) {
            points(pollsLong[party == p, .(date, value=value)], col=paste0(data$parties[code == p]$color, alphaPoints), pch=20, cex=0.5)
        }
    }

    legend('topleft', legend=data$parties$name, fill=data$parties$color, bty='n', cex=0.75, ncol=2)
}



#' Print popPolls object
#'
#' @param x popPolls object
#' @param ... ignored
#'
#' @export
print.popPolls = function(x, ...) {
    if (!is.null(x$name))
        cat(x$name, '\n\n')

    cat('Polls:\n\n')
    print(x$polls, row.names=FALSE)
    cat('\n')
    if (length(x$trends) > 0)
        cat(paste('Calculated Trends:', paste(names(x$trends), collapse=', ')), '\n')

    cat('\n')
}




#' Add Trend to Polls
#'
#' @param data polls object
#' @param name trend name
#' @param type trend function name
#' @param args trend function arguments
#' @param interpolations list of interpolations that should be applied to the trend
#'
#' @return polls object
#' @export
#'
#' @details
#' Available trend function names are:
#'
#' \code{kalman} - arguments: sd = 0.03
#'
#' \code{weightedMeanLastDays} - arguments: days = 30, maxObs = Inf
#'
#'
#' Available interpolations are:
#'
#' \code{lastInterpolation} - no arguments.
#'
#' \code{linearInterpolation} - no arguments.
#'
#' \code{bernoulliConvInterpolation} - arguments: n = 20, k = 6
#'
#' @examples
#' t = popRead('DE-parliament')
#' t = popAddTrend(t, name='Kalman 0.003', type='kalman', args=list(sd = 0.003))
#' t = popAddTrend(t, name='Kalman Raw', type='kalman', args=list(sd = 0.003),
#'                 interpolations=list('lastInterpolation' = list()))
#' plot(t)
popAddTrend = function(data, name=NULL,
                       type='kalman', args=list(),
                       interpolations=list()) {
    if (is.null(args$data))
        args$data = data

    if ((nrow(args$data$polls) + nrow(args$data$elections))  == 0)
        stop('No polls')

    if (is.null(name))
        tName = type

    trend = do.call(type, args)

    if (length(interpolations) > 0) {
        for (i in 1:length(interpolations)) {
            if (is.null(name))
                tName = paste(tName, names(interpolations)[i], sep="-")
            interpolationArgs = interpolations[[i]]
            interpolationArgs$trend = trend
            trend = do.call(names(interpolations)[i], interpolationArgs)
        }
    }

    if (is.null(name))
        name = tName

    data$trends[[name]] = trend[order(date)]
    return (data)
}


