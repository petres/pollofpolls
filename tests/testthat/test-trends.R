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

test_that('the kalman filter reproduces the trend published by POLITICO', {
    ch = withFixtures(popRead('CH-parliament', metadata = FALSE))
    ours = popAddTrend(ch, name = 'ours', type = 'kalman')$trends$ours

    both = merge(ours, ch$trends$kalman, by = c('date', 'party'))
    expect_gt(nrow(both), 300)
    expect_lt(mean(abs(both$value.x - both$value.y)), 0.001)
})

test_that('the kalman smoother reproduces the smoothed trend published by POLITICO', {
    ch = withFixtures(popRead('CH-parliament', metadata = FALSE))
    ours = popAddTrend(ch, name = 'ours', type = 'kalman', args = list(smoothing = TRUE),
                       interpolations = list(linearInterpolation = list()))$trends$ours

    both = merge(ours, ch$trends$kalmanSmooth, by = c('date', 'party'))
    expect_gt(nrow(both), 1000)
    expect_lt(mean(abs(both$value.x - both$value.y)), 0.001)
})

test_that('smoothing takes later polls into account and reduces the uncertainty', {
    x = makePolls(data.table(date = as.Date('2024-01-01') + c(0, 10, 20, 30), n = 1000L,
                             A = c(0.20, 0.30, 0.25, 0.35)))
    filtered = pollofpolls:::kalman(x)
    smoothed = pollofpolls:::kalman(x, smoothing = TRUE)

    expect_equal(smoothed$date, filtered$date)
    # the last estimate has no later polls to learn from
    expect_equal(last(smoothed$value), last(filtered$value))
    expect_gt(smoothed$value[1], filtered$value[1])
    expect_true(all(smoothed$variance <= filtered$variance))
    expect_lt(smoothed$variance[2], filtered$variance[2])
})

test_that('the smoothed trend still runs through the election result', {
    de = readTestPolls()
    election = de$elections

    trend = popAddTrend(de, name = 'k', type = 'kalman', args = list(smoothing = TRUE))$trends$k
    expect_equal(trend[date == election$date & party == 'SPD']$value, election$SPD, tolerance = 1e-8)
})

test_that('the variance of seat based trends is given in seats', {
    x = makePolls(data.table(date = as.Date('2024-01-01') + c(0, 10, 20), n = 1000L,
                             A = c(30, 32, 31), B = c(120, 118, 119)),
                  options = list(measure = 's'))
    trend = pollofpolls:::kalman(x)

    # 30 of 150 seats is a share of 0.2, measured with a standard error of
    # sqrt(0.2*0.8/1000), i.e. about 1.9 seats
    expect_equal(sqrt(trend[party == 'A']$variance[1]), sqrt(0.2*0.8/1000)*150)

    skip_if_not_installed('KFAS')
    trend = pollofpolls:::kalmanKFAS(x, smoothing = FALSE)
    expect_equal(sqrt(trend[party == 'A']$variance[1]), sqrt(0.2*0.8/1000)*150)
})

test_that('interpolations keep the column order and the variance', {
    de = readTestPolls()

    for (interpolation in c('lastInterpolation', 'linearInterpolation', 'bernoulliConvInterpolation')) {
        trend = popAddTrend(de, name = 't',
                            interpolations = stats::setNames(list(list()), interpolation))$trends$t
        expect_named(trend, c('date', 'party', 'value', 'variance'))
        expect_false(anyNA(trend$variance))
        expect_true(all(trend$value > 0 & trend$value < 1))
    }
})

test_that('linearInterpolation fills daily values on a straight line', {
    trend = data.table(date = as.Date(c('2024-01-01', '2024-01-11')), party = 'A',
                       value = c(0.2, 0.3), variance = c(1e-4, 3e-4))
    result = pollofpolls:::linearInterpolation(trend)

    expect_equal(nrow(result), 11)
    expect_equal(result[date == as.Date('2024-01-06')]$value, 0.25)
    expect_equal(result[date == as.Date('2024-01-06')]$variance, 2e-4)
})

test_that('linearInterpolation copes with parties that have a single value', {
    trend = data.table(date = as.Date(c('2024-01-01', '2024-01-01', '2024-01-05')),
                       party = c('A', 'B', 'B'), value = c(0.1, 0.2, 0.3))
    result = pollofpolls:::linearInterpolation(trend)

    expect_equal(result[party == 'A']$value, 0.1)
    expect_equal(nrow(result[party == 'B']), 5)
})

test_that('lastInterpolation carries values forward', {
    trend = data.table(date = as.Date(c('2024-01-11', '2024-01-01')), party = 'A',
                       value = c(0.3, 0.2))
    result = pollofpolls:::lastInterpolation(trend)

    expect_equal(result[date == as.Date('2024-01-10')]$value, 0.2)
    expect_equal(result[date == as.Date('2024-01-11')]$value, 0.3)
})

test_that('bernoulliConvInterpolation smooths jumps and keeps the ends', {
    trend = data.table(date = as.Date('2024-01-01') + 0:29, party = 'A',
                       value = rep(c(0.2, 0.3), each = 15))
    result = pollofpolls:::bernoulliConvInterpolation(trend)

    expect_equal(nrow(result), 30)
    expect_equal(result$value[c(1, 30)], c(0.2, 0.3))
    expect_true(result$value[15] > 0.2 && result$value[15] < 0.3)
    expect_false(is.unsorted(result$value))
})

test_that('popAddTrend accepts trend functions', {
    de = readTestPolls()
    lastPoll = function(data, offset = 0)
        popLong(data)[, .(value = last(value) + offset), by = .(date, party)]

    de = popAddTrend(de, type = lastPoll, args = list(offset = 0.01))
    expect_named(de$trends$lastPoll, c('date', 'party', 'value'))
    first = de$trends$lastPoll[1]
    expect_equal(first$value,
                 last(popLong(de)[date == first$date & party == first$party]$value) + 0.01)

    de = popAddTrend(de, type = function(data) lastPoll(data))
    expect_true('custom' %in% names(de$trends))

    expect_error(popAddTrend(de, type = function(data) data.frame(x = 1)),
                 'must return a data.frame with the columns')
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
