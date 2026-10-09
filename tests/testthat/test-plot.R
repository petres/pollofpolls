test_that('plot draws polls and trends without error', {
    de = withFixtures(popRead('DE-parliament', metadata = FALSE))
    de = popAddTrend(de, name = 'kalman', type = 'kalman')

    file = withr::local_tempfile(fileext = '.pdf')
    grDevices::pdf(file)
    on.exit(grDevices::dev.off(), add = TRUE)

    expect_s3_class(plot(de), 'popPolls')
    expect_s3_class(plot(de, xlim = as.Date(c('2025-01-01', '2025-06-01'))), 'popPolls')
})

test_that('plot scales the y axis to the visible date range', {
    parties = data.table(code = 'A', name = 'A', color = '#000000')
    polls = data.table(date = as.Date(c('2024-01-01', '2024-06-01')), n = c(1000L, 1000L),
                       A = c(0.10, 0.80))
    data = popCreate(polls = polls, parties = parties)

    file = withr::local_tempfile(fileext = '.pdf')
    grDevices::pdf(file)
    on.exit(grDevices::dev.off(), add = TRUE)

    plot(data, xlim = as.Date(c('2023-12-01', '2024-02-01')))
    expect_lt(graphics::par('usr')[4], 0.8)
})

test_that('plot needs something to draw', {
    expect_error(plot(popCreate()), 'No trend and no polls')
})

test_that('print returns the object invisibly', {
    de = readTestPolls()
    expect_output(result <- withVisible(print(de)))

    expect_false(result$visible)
    expect_s3_class(result$value, 'popPolls')
})

test_that('plot works for polls that only have a trend', {
    # EU-parliament is published as a seat projection without any raw polls
    parties = data.table(code = c('A', 'B'), name = c('A', 'B'), color = c('#000000', '#ff0000'))
    trend = data.table(date = rep(as.Date(c('2024-01-01', '2024-02-01')), each = 2),
                       party = c('A', 'B'), value = c(100, 80, 110, 70))
    data = popCreate(parties = parties, trends = list(kalman = trend),
                     options = list(measure = 's'))

    file = withr::local_tempfile(fileext = '.pdf')
    grDevices::pdf(file)
    on.exit(grDevices::dev.off(), add = TRUE)

    expect_s3_class(plot(data), 'popPolls')
})

test_that('plot draws uncertainty bands and takes xlim as strings', {
    de = withFixtures(popRead('DE-parliament', metadata = FALSE))
    de = popAddTrend(de, name = 'kalman', type = 'kalman', args = list(smoothing = TRUE))

    file = withr::local_tempfile(fileext = '.pdf')
    grDevices::pdf(file)
    on.exit(grDevices::dev.off(), add = TRUE)

    expect_s3_class(plot(de, xlim = c('2026-07-01', NA)), 'popPolls')
    expect_gt(graphics::par('usr')[1], as.numeric(as.Date('2026-06-15')))
    expect_s3_class(plot(de, bands = FALSE, level = 0.5), 'popPolls')
    expect_error(plot(de, xlim = '2026-01-01'), 'two elements')
})

test_that('trendBounds adds the interval of a trend', {
    trend = data.table(date = as.Date('2024-01-01'), party = 'A', value = 0.01, variance = 1e-4)
    bounds = pollofpolls:::trendBounds(trend, 0.95)

    expect_equal(bounds$upper, 0.01 + stats::qnorm(0.975)*0.01)
    expect_equal(bounds$lower, 0)
    expect_true(is.na(pollofpolls:::trendBounds(trend[, .(date, party, value)])$lower))
})

test_that('autoplot builds a ggplot with bands, points and lines', {
    skip_if_not_installed('ggplot2')
    de = withFixtures(popRead('DE-parliament', metadata = FALSE))
    de = popAddTrend(de, name = 'kalman', type = 'kalman')
    geoms = function(plot)
        vapply(plot$layers, function(l) class(l$geom)[1], character(1), USE.NAMES = FALSE)

    plot = ggplot2::autoplot(de, xlim = c('2026-06-01', NA))
    expect_true(inherits(plot, 'ggplot'))
    expect_equal(geoms(plot), c('GeomRibbon', 'GeomPoint', 'GeomLine'))
    expect_no_error(ggplot2::ggplot_build(plot))

    expect_false('GeomRibbon' %in% geoms(ggplot2::autoplot(de, bands = FALSE)))

    # a party that is no longer polled is left out of the legend
    de$polls[date > as.Date('2026-06-01'), GRUENE := NA]
    de$trends = list()
    plot = ggplot2::autoplot(de, xlim = c('2026-07-01', NA))
    built = ggplot2::ggplot_build(plot)
    scale = built$plot$scales$get_scales('colour')
    expect_equal(scale$get_breaks(), c('Union', 'SPD', 'AfD'), ignore_attr = TRUE)
    expect_equal(scale$get_labels(), c('CDU/CSU', 'SPD', 'AfD'))
    expect_error(ggplot2::autoplot(popCreate()), 'No trend and no polls')
})
