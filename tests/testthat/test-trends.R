test_that('kalman produces a trend for every party', {
    de = readTestPolls()
    de = popAddTrend(de, name = 'kalman', type = 'kalman', args = list(sd = 0.003))

    trend = de$trends$kalman
    expect_setequal(unique(trend$party), de$parties$code)
    expect_named(trend, c('date', 'party', 'value', 'variance'))
    expect_true(all(trend$value > 0 & trend$value < 1))
    expect_false(is.unsorted(trend$date))
})

test_that('kalman uses the election result', {
    de = readTestPolls()
    election = de$elections

    trend = popAddTrend(de, name = 'k', type = 'kalman')$trends$k
    expect_true(election$date %in% trend$date)
    expect_equal(trend[date == election$date & party == 'SPD']$value, election$SPD,
                 tolerance = 1e-8)
})

test_that('weightedMeanLastDays returns one value per day and party', {
    de = readTestPolls()
    de = popAddTrend(de, name = 'wm', type = 'weightedMeanLastDays',
                     args = list(days = 30))

    trend = de$trends$wm
    expect_named(trend, c('date', 'party', 'value'))
    expect_true(all(trend$value > 0 & trend$value < 1))
    expect_equal(uniqueN(trend$date), as.integer(diff(range(trend$date))) + 1)
})

test_that('weightedMeanLastDays weights recent polls higher', {
    parties = data.table(code = 'A', name = 'A', color = '#000000')
    polls = data.table(date = as.Date(c('2024-01-01', '2024-01-10')), n = c(1000L, 1000L),
                       A = c(0.20, 0.40))
    data = popCreate(polls = polls, parties = parties)

    trend = pollofpolls:::weightedMeanLastDays(data, days = 30)
    expect_true(trend[date == as.Date('2024-01-10')]$value > 0.30)
    expect_equal(trend[date == as.Date('2024-01-01')]$value, 0.20)
})

test_that('maxObs limits the number of polls taken into account', {
    parties = data.table(code = 'A', name = 'A', color = '#000000')
    polls = data.table(date = as.Date(c('2024-01-01', '2024-01-02', '2024-01-03')),
                       firm = c('a', 'b', 'c'), n = c(1000L, 1000L, 1000L),
                       A = c(0.10, 0.20, 0.30))
    data = popCreate(polls = polls, parties = parties)

    trend = pollofpolls:::weightedMeanLastDays(data, days = 30, maxObs = 1)
    expect_equal(trend[date == as.Date('2024-01-03')]$value, 0.30)
})

test_that('ident averages polls published on the same day', {
    parties = data.table(code = 'A', name = 'A', color = '#000000')
    polls = data.table(date = as.Date(c('2024-01-01', '2024-01-01')), n = c(1000L, 1000L),
                       A = c(0.20, 0.40))
    data = popCreate(polls = polls, parties = parties)

    trend = pollofpolls:::ident(data)
    expect_equal(nrow(trend), 1)
    expect_equal(trend$value, 0.30)
})

test_that('interpolations fill the gaps between trend values', {
    de = readTestPolls()
    de = popAddTrend(de, type = 'kalman',
                     interpolations = list(lastInterpolation = list()))

    trend = de$trends[['kalman-lastInterpolation']]
    dates = trend[party == 'SPD']$date
    expect_equal(as.integer(diff(dates)), rep(1L, length(dates) - 1))
})

test_that('popAddTrend rejects unknown trends and interpolations', {
    de = readTestPolls()

    expect_error(popAddTrend(de, type = 'nope'), 'Unknown trend type')
    expect_error(popAddTrend(de, interpolations = list(nope = list())),
                 'Unknown interpolation')
    expect_error(popAddTrend(list()), 'popPolls object')
    expect_error(popAddTrend(popCreate()), 'No polls')
})

test_that('trends of seat based polls are not rescaled', {
    parties = data.table(code = 'A', name = 'A', color = '#000000')
    polls = data.table(date = as.Date(c('2024-01-01', '2024-01-10')), n = c(1000L, 1000L),
                       A = c(100, 120))
    data = popCreate(polls = polls, parties = parties,
                     options = list(measure = 's'))

    trend = pollofpolls:::weightedMeanLastDays(data, days = 30)
    expect_true(all(trend$value >= 100 & trend$value <= 120))
})

test_that('kalmanKFAS agrees with the built in kalman filter', {
    skip_if_not_installed('KFAS')

    de = readTestPolls()
    de = popAddTrend(de, name = 'kalman', type = 'kalman', args = list(sd = 0.003))
    de = popAddTrend(de, name = 'kfas', type = 'kalmanKFAS', args = list(sd = 0.003))

    both = merge(de$trends$kalman[, .(date, party, kalman = value)],
                 de$trends$kfas[, .(date, party, kfas = value)],
                 by = c('date', 'party'))

    expect_gt(nrow(both), 0)
    expect_gt(stats::cor(both$kalman, both$kfas), 0.99)
})
