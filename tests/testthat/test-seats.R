# Five parties polled daily, so that the Kalman trend is quite certain.
seatPolls = function() {
    polls = data.table(date = as.Date('2024-01-01') + 0:59, firm = 'X', n = 1000L,
                       A = 0.47, B = 0.30, C = 0.13, D = 0.08, E = 0.02)
    popAddTrend(makePolls(polls), name = 'kalman', type = 'kalman', args = list(smoothing = TRUE))
}

test_that('popSeats turns the uncertainty of the trend into seat ranges', {
    set.seed(1)
    seats = popSeats(seatPolls(), seats = 100, threshold = 0.05, simulations = 500)

    expect_named(seats, c('party', 'name', 'share', 'seats', 'lower', 'upper', 'pSeats'))
    expect_true(all(seats$lower <= seats$seats & seats$seats <= seats$upper))
    expect_equal(seats[party == 'D']$pSeats, 1)
    expect_equal(seats[party == 'E']$pSeats, 0)
})

test_that('simulations need a trend with variance', {
    x = seatPolls()
    x$trends$kalman[, variance := NULL]

    expect_error(popSeats(x, seats = 100, simulations = 10), 'has no variance')
    expect_error(popSeats(c(A = 0.6, B = 0.4), seats = 10, simulations = 10), 'need a popPolls object')
    expect_named(popSeats(x, seats = 100), c('party', 'name', 'share', 'seats'))
})

test_that('popCoalitions gives the probability of a majority', {
    set.seed(1)
    coalitions = popCoalitions(seatPolls(), seats = 100, threshold = 0.05, simulations = 500,
                               coalitions = list(Big = c('A', 'B'), c('B', 'C')))

    expect_named(coalitions, c('coalition', 'parties', 'seats', 'lower', 'upper', 'pMajority'))
    expect_equal(coalitions$coalition, c('Big', 'B+C'))
    expect_equal(coalitions$pMajority, c(1, 0))
    expect_error(popCoalitions(seatPolls(), seats = 100, coalitions = list(c('A', 'Z'))), 'Z')
    expect_error(popCoalitions(seatPolls(), seats = 100, simulations = 0), 'need simulations')
})

test_that('popCoalitions lists the possible coalitions without redundant members', {
    set.seed(1)
    coalitions = popCoalitions(seatPolls(), seats = 100, threshold = 0.05, simulations = 500)

    expect_true(all(c('A+B', 'A+C') %in% coalitions$parties))
    expect_false('A+B+C' %in% coalitions$parties)
    expect_false(is.unsorted(rev(coalitions$pMajority)))
})
