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
