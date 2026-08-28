test_that('poll codes are extracted from multi line divs', {
    metadata = pollofpolls:::parsePollPage('germany.html',
                                           html = readFixture('germany.html'))

    expect_equal(metadata$code, c('DE-parliament', 'DE-approval'))
    expect_false(any(grepl('[[:space:]<>"]', metadata$code)))
})

test_that('html entities in chart titles are decoded', {
    metadata = pollofpolls:::parsePollPage('germany.html',
                                           html = readFixture('germany.html'))

    expect_equal(metadata[code == 'DE-parliament']$title,
                 'Germany \u2014 National parliament voting intention')
    expect_equal(metadata[code == 'DE-approval']$title, 'Germany \u2014 Chancellor approval')
})

test_that('party colours are read from the chart attributes', {
    metadata = pollofpolls:::parsePollPage('germany.html',
                                           html = readFixture('germany.html'))
    colors = metadata[code == 'DE-parliament']$colors[[1]]

    expect_equal(sort(names(colors)), c('AfD', 'GRUENE', 'SPD', 'Union'))
    expect_equal(unname(colors['Union']), '#000000')
})

test_that('country pages are collected from the overview page', {
    pages = pollofpolls:::countryPages(readFixture('germany.html'))

    expect_setequal(pages, c('https://www.politico.eu/europe-poll-of-polls/germany/',
                             'https://www.politico.eu/europe-poll-of-polls/austria/'))
})

test_that('a poll code is looked up on its country page only', {
    withr::local_options(pollofpolls.cache = FALSE)
    pollofpolls:::popCacheClear()

    requested = character()
    testthat::local_mocked_bindings(fetchUrl = function(url) {
        requested <<- c(requested, url)
        readFixture('germany.html')
    }, .package = 'pollofpolls')

    metadata = pollofpolls:::getCodeMetadata('DE-parliament')

    expect_equal(nrow(metadata), 1)
    expect_equal(requested, 'https://www.politico.eu/europe-poll-of-polls/germany/')
})

test_that('unknown iso2 codes do not send a request to a guessed page', {
    expect_null(pollofpolls:::hintPage('XX-parliament'))
    expect_equal(pollofpolls:::hintPage('DE-parliament'),
                 'https://www.politico.eu/europe-poll-of-polls/germany/')
})

test_that('retryable http status codes are recognised', {
    expect_true(pollofpolls:::isRetryable(429L))
    expect_true(pollofpolls:::isRetryable(503L))
    expect_true(pollofpolls:::isRetryable(NA_integer_))
    expect_false(pollofpolls:::isRetryable(404L))
})
