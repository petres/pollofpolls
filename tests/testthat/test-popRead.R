test_that('popRead parses the endpoint payload', {
    de = readTestPolls()

    expect_s3_class(de, 'popPolls')
    expect_equal(nrow(de$polls), 60)
    expect_equal(names(de$polls)[1:4], c('date', 'dateFrom', 'firm', 'n'))
    expect_true(all(c('Union', 'SPD', 'GRUENE', 'AfD') %in% names(de$polls)))
    expect_s3_class(de$polls$date, 'Date')
    expect_false(is.unsorted(de$polls$date))
    expect_true(all(de$polls$Union > 0 & de$polls$Union < 1))
})

test_that('popRead survives the quirks of the live data', {
    de = readTestPolls()

    # date_from is delivered as the string "NA" for some polls
    expect_false(any(is.na(de$polls$dateFrom)))
    expect_equal(de$polls$dateFrom[1], de$polls$date[1])

    # sample sizes are sometimes strings and sometimes missing
    expect_type(de$polls$n, 'integer')
    expect_equal(de$polls$n[2], 1500L)
    expect_true(is.na(de$polls$n[3]))
})

test_that('popRead keeps the published party order and picks up colours', {
    de = readTestPolls()

    expect_equal(de$parties$code, c('Union', 'SPD', 'GRUENE', 'AfD'))
    expect_equal(de$parties[code == 'SPD']$color, '#F0001C')
    expect_equal(de$parties[code == 'AfD']$color, '#2175d9')
})

test_that('popRead uses the scraped chart title as name', {
    de = readTestPolls()

    expect_equal(de$name, 'Germany \u2014 National parliament voting intention')
})

test_that('popRead loads elections and published trends', {
    de = readTestPolls()

    expect_equal(nrow(de$elections), 1)
    expect_named(de$trends, 'kalmanSmooth')
    expect_named(de$trends$kalmanSmooth, c('date', 'party', 'value'))
    expect_true(all(de$trends$kalmanSmooth$value < 1))
})

test_that('popRead can skip the website lookup', {
    de = withFixtures(popRead('DE-parliament', metadata = FALSE))

    expect_equal(de$name, 'DE - DE-parliament')
    expect_equal(nrow(de$parties), 4)
    expect_false(any(is.na(de$parties$color)))
})

test_that('popRead honours the load argument', {
    de = withFixtures(popRead('DE-parliament', load = 'polls', metadata = FALSE))

    expect_equal(nrow(de$elections), 0)
    expect_length(de$trends, 0)
})

test_that('firm names are cleaned of stray white space', {
    payload = list(options = list(measure = 'p'), parties = list(A = 'A'),
                   polls = list(list(date = '2024-01-01', firm = 'Forsa\t', parties = list(A = 20)),
                                list(date = '2024-01-02', firm = ' Forsa  ', parties = list(A = 21)),
                                list(date = '2024-01-03', firm = '', parties = list(A = 22)),
                                list(date = '2024-01-04', firm = 'Research \u00adAffairs',
                                     parties = list(A = 23))))

    expect_equal(readJsonPayload(payload)$polls$firm, c('Forsa', 'Forsa', NA, 'Research Affairs'))
})

test_that('polls without a date are dated by their fieldwork or dropped', {
    payload = list(options = list(measure = 'p'), parties = list(A = 'A'),
                   polls = list(list(date = 'NA', date_from = '2024-01-05', parties = list(A = 20)),
                                list(date = '2024-01-01', parties = list(A = 21)),
                                list(date = '', parties = list(A = 22))))

    expect_warning(polls <- readJsonPayload(payload)$polls, 'Dropped 1 poll')
    expect_equal(polls$date, as.Date(c('2024-01-01', '2024-01-05')))
    expect_equal(polls$A, c(0.21, 0.20))
})

test_that('popRead asks for a single code', {
    expect_error(popRead(c('DE-parliament', 'AT-parliament')), 'single poll code')
    expect_error(popRead(NA_character_), 'single poll code')
})

test_that('popRead explains unknown codes and broken responses', {
    withr::local_options(pollofpolls.cache = FALSE)
    local_mocked_bindings(fetchUrl = function(url) {
        if (grepl('missing', url))
            stop(errorCondition('Failed', class = 'pollofpolls_http_error', status = 404L))
        '<html>maintenance</html>'
    }, .package = 'pollofpolls')

    expect_error(popRead('XX-missing', metadata = FALSE), "No poll data available for code 'XX-missing'")
    expect_error(popRead('XX-broken', metadata = FALSE), "Could not parse the data of 'XX-broken'")
})

test_that('popRead records where and when the data was retrieved', {
    de = readTestPolls()

    expect_equal(de$code, 'DE-parliament')
    expect_s3_class(de$retrieved, 'POSIXct')
})
