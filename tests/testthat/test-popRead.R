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
