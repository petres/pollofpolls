eventPayload = function() list(
    options = list(measure = 'p'), parties = list(A = 'A'),
    polls = list(list(date = '2024-01-01', parties = list(A = 20)),
                 list(date = '2024-03-01', parties = list(A = 25))),
    events = list(list(date = '2024-02-01', name_short = 'Scandal', visible_max_month = 12),
                  list(date = '2024-01-15', name_short = 'Election', visible_max_month = setNames(list(), character())),
                  list(date = setNames(list(), character()), name_short = '')))

test_that('popRead reads the events and skips empty ones', {
    x = readJsonPayload(eventPayload())

    expect_equal(x$events$date, as.Date(c('2024-01-15', '2024-02-01')))
    expect_equal(x$events$name, c('Election', 'Scandal'))
    expect_equal(nrow(readJsonPayload(eventPayload(), load = 'polls')$events), 0)
    expect_output(print(x), 'Events: 2')
})

test_that('both plots mark the events in the shown range', {
    x = readJsonPayload(eventPayload())

    file = withr::local_tempfile(fileext = '.pdf')
    grDevices::pdf(file)
    on.exit(grDevices::dev.off(), add = TRUE)
    expect_s3_class(plot(x), 'popPolls')
    expect_s3_class(plot(x, events = FALSE), 'popPolls')

    skip_if_not_installed('ggplot2')
    geoms = function(plot)
        vapply(plot$layers, function(l) class(l$geom)[1], character(1), USE.NAMES = FALSE)
    expect_equal(geoms(ggplot2::autoplot(x)), c('GeomVline', 'GeomPoint', 'GeomText'))
    expect_equal(geoms(ggplot2::autoplot(x, events = FALSE)), 'GeomPoint')

    shown = ggplot2::autoplot(x, xlim = c('2024-01-20', NA))$layers[[3]]$data
    expect_equal(shown$name, 'Scandal')
})
