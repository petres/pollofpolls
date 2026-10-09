test_that('spellings of a firm are merged under the most frequent one', {
    firms = c('INSA/YouGov', 'INSA YouGov', 'INSA/YouGov', 'Tecn\u00e8', 'Tecn\u00e8', 'Tecne',
              'TECN\u00c9', 'IPSOS', 'IPSOS', 'Ipsos', 'IMR/UNic', NA)

    expect_equal(pollofpolls:::unifyFirms(firms),
                 c(rep('INSA/YouGov', 3), rep('Tecn\u00e8', 4), rep('IPSOS', 3), 'IMR/UNic', NA))
})

test_that('names without letters are only merged with identical ones', {
    expect_equal(pollofpolls:::firmKey(c('//', '--', 'A-B')), c('//', '--', 'ab'))
})

test_that('popRead merges spellings and keeps the published names', {
    payload = list(options = list(measure = 'p'), parties = list(A = 'A'),
                   polls = list(list(date = '2024-01-01', firm = 'INSA/YouGov', parties = list(A = 20)),
                                list(date = '2024-01-02', firm = 'INSA YouGov', parties = list(A = 21)),
                                list(date = '2024-01-03', firm = 'insa yougov ', parties = list(A = 22)),
                                list(date = '2024-01-04', firm = 'INSA YouGov', parties = list(A = 23))))
    polls = readJsonPayload(payload)$polls

    expect_equal(polls$firm, rep('INSA YouGov', 4))
    expect_equal(polls$firmRaw, c('INSA/YouGov', 'INSA YouGov', 'insa yougov ', 'INSA YouGov'))
})

test_that('popRenameFirms renames all spellings and leaves the original alone', {
    x = makePolls(data.table(date = as.Date('2024-01-01') + 0:3,
                             firm = c('Peter Hajek', 'peter-hajek', 'Hajek', 'Market'),
                             A = c(0.1, 0.2, 0.3, 0.4)))
    renamed = popRenameFirms(x, c('Peter Hajek' = 'Hajek'))

    expect_equal(renamed$polls$firm, c('Hajek', 'Hajek', 'Hajek', 'Market'))
    expect_equal(x$polls$firm[1], 'Peter Hajek')
    expect_error(popRenameFirms(x, c('Hajek')), 'named character vector')
})

test_that('renamings can be set for every popRead', {
    withr::local_options(pollofpolls.firms = c('Peter Hajek' = 'Hajek'))
    payload = list(options = list(measure = 'p'), parties = list(A = 'A'),
                   polls = list(list(date = '2024-01-01', firm = 'Peter Hajek', parties = list(A = 20)),
                                list(date = '2024-01-02', firm = 'HAJEK', parties = list(A = 21))))

    expect_equal(readJsonPayload(payload)$polls$firm, c('Hajek', 'Hajek'))
})

test_that('missing sample sizes are taken from the firm, then from all polls', {
    polls = data.table(firm = c('A', 'A', 'A', 'B', 'B', NA), n = c(1000L, 2000L, NA, NA, NA, NA))

    expect_equal(pollofpolls:::fillSampleSizes(polls), c(1000, 2000, 1500, 1500, 1500, 1500))
    expect_equal(pollofpolls:::fillSampleSizes(polls, 400), c(1000, 2000, 400, 400, 400, 400))
    expect_equal(pollofpolls:::fillSampleSizes(polls[, .(firm, n = NA_integer_)]), rep(400, 6))
    expect_error(pollofpolls:::fillSampleSizes(polls, 'median'), 'must be "firm" or a positive number')
})

test_that('kalman gives polls without sample size the weight of their firm', {
    x = makePolls(data.table(date = as.Date('2024-01-01') + c(0, 1), firm = 'Big', n = c(4000L, NA),
                             A = c(0.30, 0.40)))

    byFirm = pollofpolls:::kalman(x)
    fixed = pollofpolls:::kalman(x, missingSampleSize = 400)
    # the second poll counts as much as the first one instead of a tenth of it
    expect_gt(byFirm$value[2], fixed$value[2])
    expect_equal(byFirm$value[2], 0.35, tolerance = 0.01)
})

test_that('a share of zero does not turn a poll into an exact observation', {
    expect_gt(pollofpolls:::getPollVar(0, 1000), 0)
    expect_equal(pollofpolls:::getPollVar(0.3, Inf), 0)
    expect_equal(pollofpolls:::getPollVar(0.3, 1000), 0.3*0.7/1000)
})
