#' Plot polls with ggplot2
#'
#' The \pkg{ggplot2} counterpart of [plot.popPolls()]: polls as points, trends
#' as lines and, for trends with a variance, uncertainty bands. The result is
#' an ordinary ggplot object that can be extended with further layers, scales
#' and themes.
#'
#' @param object A `popPolls` object.
#' @param ... Ignored.
#' @param bands Whether uncertainty bands should be drawn.
#' @param level Coverage of the uncertainty bands.
#' @param xlim Date range to show, dates or ISO date strings; `NA` keeps the
#'   respective end of the data range.
#'
#' @return A `ggplot` object.
#' @method autoplot popPolls
#' @exportS3Method ggplot2::autoplot
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' de = popRead('DE-parliament')
#' de = popAddTrend(de, name = 'kalman', type = 'kalman')
#' autoplot(de, xlim = c('2024-01-01', NA)) + theme_minimal()
#' }
autoplot.popPolls = function(object, ..., bands = TRUE, level = 0.95, xlim = NULL) {
    if (!requireNamespace('ggplot2', quietly = TRUE))
        stop('Package "ggplot2" is needed. Please install it.', call. = FALSE)

    polls = toLong(object)
    trends = popLong(object, 'trends')
    if (nrow(polls) == 0 && nrow(trends) == 0)
        stop('No trend and no polls to plot', call. = FALSE)

    parties = object$parties
    colors = stats::setNames(parties$color, parties$code)
    labels = stats::setNames(parties$name, parties$code)

    plot = ggplot2::ggplot(mapping = ggplot2::aes(date, value, colour = party))

    if (bands && nrow(trends) > 0) {
        bounds = trendBounds(trends, level)[!is.na(lower)]
        if (nrow(bounds) > 0)
            plot = plot + ggplot2::geom_ribbon(
                data = bounds,
                ggplot2::aes(ymin = lower, ymax = upper, fill = party,
                             group = interaction(trend, party)),
                colour = NA, alpha = 0.2)
    }

    if (nrow(polls) > 0)
        plot = plot + ggplot2::geom_point(data = polls, alpha = if (nrow(trends) > 0) 0.3 else 0.7,
                                          size = 0.6)

    if (nrow(trends) > 0) {
        multiple = length(unique(trends$trend)) > 1
        plot = plot +
            ggplot2::geom_line(data = trends,
                               ggplot2::aes(linetype = trend, group = interaction(trend, party)),
                               linewidth = 0.7) +
            ggplot2::scale_linetype_discrete(name = NULL, guide = if (multiple) 'legend' else 'none')
    }

    # like plot(), the y axis is scaled to the data inside the date range
    dates = dateLimits(c(polls$date, trends$date), xlim)
    visible = function(d) d[date >= dates[1] & date <= dates[2]]
    values = c(visible(polls)$value, visible(trends)$value)
    ylim = if (any(!is.na(values))) c(0, max(values, na.rm = TRUE)*1.1) else NULL
    # parties that are not polled any more are left out of the legend
    shown = parties$code[parties$code %in% c(visible(polls)$party, visible(trends)$party)]

    percent = function(x) paste0(format(x*100, trim = TRUE), '%')
    seats = identical(object$options$measure, 's')

    plot +
        ggplot2::scale_colour_manual(name = NULL, values = colors, breaks = shown,
                                     labels = unname(labels[shown])) +
        ggplot2::scale_fill_manual(values = colors, guide = 'none') +
        ggplot2::scale_y_continuous(labels = if (seats) ggplot2::waiver() else percent) +
        ggplot2::coord_cartesian(xlim = dates, ylim = ylim) +
        ggplot2::labs(title = object$name, x = NULL, y = NULL)
}
