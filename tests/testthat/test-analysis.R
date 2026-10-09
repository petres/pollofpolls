standings = function() {
    trend = data.table(date = as.Date(c('2023-01-01', rep(c('2024-01-01', '2024-01-31', '2024-03-01'), each = 2))),
                       party = c('C', rep(c('A', 'B'), 3)),
                       value = c(0.10, 0.30, 0.20, 0.32, 0.22, 0.35, 0.21),
                       variance = 1e-4)
    elections = data.table(date = as.Date('2023-09-01'), A = 0.28, B = 0.25, C = 0.05)
    parties = data.table(code = c('A', 'B', 'C'), name = c('Alpha', 'Beta', 'Gamma'),
                         color = c('#000000', '#ff0000', '#00ff00'))
    popCreate(parties = parties, trends = list(kalman = trend), elections = elections)
}

test_that('popLong returns polls, elections and trends in long format', {
    de = readTestPolls()

    polls = popLong(de)
    expect_true(all(c('date', 'firm', 'n', 'party', 'value') %in% names(polls)))
    expect_equal(nrow(polls), sum(!is.na(as.matrix(de$polls[, de$parties$code, with = FALSE]))))
    expect_false(is.unsorted(polls$date))

    expect_equal(nrow(popLong(de, 'elections')), 3)
    expect_equal(unique(popLong(de, 'trends')$trend), 'kalmanSmooth')
    expect_named(popLong(popCreate(), 'trends'), c('trend', 'date', 'party', 'value'))
})

test_that('popLatest summarises a trend at a date', {
    x = standings()

    latest = popLatest(x)
    # C was last seen more than a year ago
    expect_equal(latest$party, c('A', 'B'))
    expect_equal(latest$name, c('Alpha', 'Beta'))
    expect_equal(latest$value, c(0.35, 0.21))
    expect_equal(latest$change, c(0.03, -0.01))
    expect_equal(latest$election, c(0.28, 0.25))
    expect_equal(latest$lower, latest$value - stats::qnorm(0.975)*0.01)

    earlier = popLatest(x, date = '2024-02-15')
    expect_equal(earlier$value, c(0.32, 0.22))
    expect_equal(earlier$change, c(0.02, 0.02))
})

test_that('popLatest checks the requested trend', {
    expect_error(popLatest(popCreate()), 'No trends')
    expect_error(popLatest(standings(), trend = 'nope'), "Unknown trend 'nope'")
    expect_error(popLatest(standings(), date = '2020-01-01'), 'no values')
})

test_that('popSeats implements the common apportionment methods', {
    votes = c(A = 100000, B = 80000, C = 30000, D = 20000)/230000
    seats = function(...) {
        result = popSeats(votes, seats = 8, ...)
        stats::setNames(result$seats, result$party)[c('A', 'B', 'C', 'D')]
    }

    expect_equal(seats(method = 'dhondt'), c(A = 4L, B = 3L, C = 1L, D = 0L))
    expect_equal(seats(method = 'sainte-lague'), c(A = 3L, B = 3L, C = 1L, D = 1L))
    expect_equal(seats(method = 'hare'), c(A = 3L, B = 3L, C = 1L, D = 1L))
    expect_equal(seats(method = 'sainte-lague', threshold = 0.1)[['D']], 0L)
    expect_equal(sum(popSeats(votes, seats = 183)$seats), 183)
})

test_that('popSeats projects the latest values of a trend', {
    de = readTestPolls()
    seats = popSeats(de, seats = 630, threshold = 0.05, method = 'sainte-lague')

    expect_named(seats, c('party', 'name', 'share', 'seats'))
    expect_equal(sum(seats$seats), 630)
    expect_setequal(seats$party, de$parties$code)
    expect_false(is.unsorted(rev(seats$seats)))
})

test_that('popSeats rejects unsuitable input', {
    expect_error(popSeats(c(A = 0.5), seats = 0), 'positive whole number')
    expect_error(popSeats(c(0.5, 0.5), seats = 10), 'named numeric vector')
    expect_error(popSeats(popCreate(options = list(measure = 's')), seats = 10), 'seat projections')
})

test_that('popFirms lists the polling firms', {
    de = readTestPolls()
    firms = popFirms(de)

    expect_named(firms, c('firm', 'polls', 'first', 'last', 'sampleSize', 'spellings'))
    expect_equal(sum(firms$polls), nrow(de$polls))
    expect_false(is.unsorted(rev(firms$polls)))
    expect_true(all(firms$first <= firms$last))
    # one of its polls is published as " Forschungsgruppe Wahlen"
    raw = jsonlite::fromJSON(fixture('DE-parliament.json'))$polls$firm
    expect_equal(firms[firm == 'Forschungsgruppe Wahlen']$polls,
                 sum(trimws(raw) == 'Forschungsgruppe Wahlen'))
    expect_false(any(grepl('^\\s|\\s$', firms$firm)))
})

test_that('popHouseEffects finds a firm that overestimates a party', {
    polls = data.table(date = as.Date('2024-01-01') + 0:59, firm = c('High', 'Low', 'Fair'), n = 1000L)
    polls[, A := 0.30 + c(High = 0.02, Low = -0.01, Fair = 0)[firm]]
    polls[, B := 1 - A]
    effects = popHouseEffects(makePolls(polls))

    expect_named(effects, c('firm', 'party', 'polls', 'effect', 'se'))
    a = stats::setNames(effects[party == 'A']$effect, effects[party == 'A']$firm)
    expect_true(a[['High']] > a[['Fair']] && a[['Fair']] > a[['Low']])
    expect_lt(abs(a[['High']] - a[['Low']] - 0.03), 0.003)
    expect_equal(effects[party == 'B']$effect, -effects[party == 'A']$effect, tolerance = 1e-6)

    expect_equal(nrow(popHouseEffects(makePolls(polls), minPolls = 21)), 0)
})

test_that('popHouseEffects can compare with a published trend', {
    de = readTestPolls()
    effects = popHouseEffects(de, trend = 'kalmanSmooth', minPolls = 1)

    expect_gt(nrow(effects), 0)
    expect_true(all(effects$party %in% de$parties$code))
})

test_that('popHouseEffects is not fooled by a firm that publishes most polls', {
    polls = data.table(date = as.Date('2024-01-01') + 0:99, n = 1000L,
                       firm = c(rep('Big', 8), 'Small', 'Other'))
    polls[, A := 0.30 + ifelse(firm == 'Big', 0.02, 0)]
    polls[, B := 1 - A]
    x = makePolls(polls)
    x$trends = list(consensus = pollofpolls:::kalman(x, smoothing = TRUE))

    joint = popHouseEffects(x)[party == 'A']
    onePass = popHouseEffects(x, trend = 'consensus')[party == 'A']
    # relative to the average of the three firms, Big is 2/3 of 2 points too high
    expect_equal(joint[firm == 'Big']$effect, 0.02*2/3, tolerance = 0.05)
    expect_lt(onePass[firm == 'Big']$effect, 0.01)
    expect_equal(sum(joint$effect), 0, tolerance = 1e-6)
})

test_that('popAddTrend can correct the polls by the house effects', {
    polls = data.table(date = as.Date('2024-01-01') + 0:59, firm = c('High', 'Low'), n = 1000L)
    polls[, A := 0.30 + ifelse(firm == 'High', 0.03, -0.03) + 0.0005*seq_len(.N)]
    polls[, B := 1 - A]
    x = popAddTrend(makePolls(polls), houseEffects = TRUE)
    x = popAddTrend(x)

    expect_named(x$trends, c('kalman-adjusted', 'kalman'))
    roughness = function(trend) stats::sd(diff(trend[party == 'A']$value))
    expect_lt(roughness(x$trends[['kalman-adjusted']]), roughness(x$trends$kalman)/3)
})
