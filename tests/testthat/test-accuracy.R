# Two firms on either side of the result, and polls after the election that
# must not influence the evaluation.
accuracyPolls = function() {
    before = data.table(date = as.Date('2024-01-01') + 0:59, firm = c('Low', 'High'), n = 1000L)
    before[, A := ifelse(firm == 'Low', 0.40, 0.44)]
    after = data.table(date = as.Date('2024-03-05') + 0:9, firm = 'Low', n = 1000L, A = 0.10)
    polls = rbind(before, after)
    polls[, B := 1 - A]
    makePolls(polls, elections = data.table(date = as.Date('2024-03-03'), A = 0.42, B = 0.58))
}

test_that('popAccuracy compares trends and firms with the result', {
    accuracy = popAccuracy(accuracyPolls())

    expect_named(accuracy, c('election', 'kind', 'source', 'party', 'estimate', 'result', 'error'))
    trend = accuracy[kind == 'trend' & party == 'A']
    expect_equal(trend$source, 'kalman')
    expect_lt(abs(trend$error), 0.01)

    firms = accuracy[kind == 'firm' & party == 'A']
    expect_equal(firms$source, c('High', 'Low'))
    expect_equal(firms$error, c(0.02, -0.02))
})

test_that('popAccuracy evaluates several trends', {
    accuracy = popAccuracy(accuracyPolls(), firms = FALSE, trends = list(
        flexible = list(type = 'kalman', args = list(sd = 0.01)),
        mean = list(type = 'weightedMeanLastDays', args = list(days = 10))))

    expect_setequal(unique(accuracy$source), c('flexible', 'mean'))
    expect_equal(unique(accuracy$kind), 'trend')
    expect_error(popAccuracy(accuracyPolls(), trends = list(list(type = 'kalman'))), 'named list')
})

test_that('popAccuracy copes with objects without polls or elections', {
    expect_equal(nrow(popAccuracy(popCreate())), 0)
    x = accuracyPolls()
    x$elections = data.table()
    expect_equal(nrow(popAccuracy(x)), 0)
})

test_that('popAccuracy skips firms without polls shortly before the election', {
    x = accuracyPolls()
    x$polls = x$polls[date < as.Date('2024-02-01') | date > as.Date('2024-03-03')]

    expect_no_warning(accuracy <- popAccuracy(x))
    expect_equal(unique(accuracy$kind), 'trend')
})
