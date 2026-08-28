fixture = function(name)
    testthat::test_path('fixtures', name)

readFixture = function(name)
    paste(readLines(fixture(name), warn = FALSE, encoding = 'UTF-8'), collapse = '\n')

# Runs `code` with every network access answered from the fixtures directory and
# with both caches disabled, so that the tests never touch the network.
withFixtures = function(code) {
    withr::local_options(pollofpolls.cache = FALSE, pollofpolls.requestDelay = 0)
    pollofpolls:::popCacheClear()

    mockFetch = function(url) {
        if (grepl('wp-json', url, fixed = TRUE))
            return(readFixture(paste0(basename(url), '.json')))
        if (grepl('germany', url, fixed = TRUE))
            return(readFixture('germany.html'))
        stop(sprintf("Unexpected request to '%s'", url), call. = FALSE)
    }

    testthat::local_mocked_bindings(fetchUrl = mockFetch, .package = 'pollofpolls')
    force(code)
}

readTestPolls = function()
    withFixtures(popRead('DE-parliament'))
