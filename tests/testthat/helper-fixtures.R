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

# Reads a payload given as R list, as if it had been published under `code`.
readJsonPayload = function(payload, code = 'XX-test', ...) {
    withr::local_options(pollofpolls.cache = FALSE)
    json = as.character(jsonlite::toJSON(payload, auto_unbox = TRUE))
    testthat::local_mocked_bindings(fetchUrl = function(url) json, .package = 'pollofpolls')
    popRead(code, metadata = FALSE, ...)
}

# A popPolls object with one column per party, named after the party codes.
makePolls = function(polls, ...) {
    codes = setdiff(names(polls), c('date', 'dateFrom', 'firm', 'n'))
    parties = data.table(code = codes, name = codes,
                         color = grDevices::hcl.colors(length(codes), palette = 'Dark 3'))
    popCreate(polls = polls, parties = parties, ...)
}

# Points the on-disk cache to a temporary directory for the calling test.
localCache = function(env = parent.frame()) {
    withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir(.local_envir = env), .local_envir = env)
    withr::local_options(pollofpolls.cache = TRUE, pollofpolls.requestDelay = 0, .local_envir = env)
    pollofpolls:::popCacheClear()
}
