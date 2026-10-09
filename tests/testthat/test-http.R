response = function(status, content = '', headers = list())
    list(status = status, content = content, headers = headers,
         reason = sprintf('HTTP status %d', status))

test_that('fetchUrl retries as advised by Retry-After and backs off otherwise', {
    withr::local_options(pollofpolls.retryDelay = 1, pollofpolls.attempts = 3)
    responses = list(response(429L, headers = list('retry-after' = '7')),
                     response(503L),
                     response(200L, '{}'))
    calls = 0
    waited = numeric()
    local_mocked_bindings(httpGet = function(url) {
        calls <<- calls + 1
        responses[[calls]]
    }, wait = function(seconds) waited <<- c(waited, seconds), .package = 'pollofpolls')

    expect_equal(pollofpolls:::fetchUrl('https://example.org/'), '{}')
    expect_equal(calls, 3)
    expect_equal(waited, c(7, 2))
})

test_that('fetchUrl gives up on errors that are not worth a retry', {
    calls = 0
    local_mocked_bindings(httpGet = function(url) {
        calls <<- calls + 1
        response(404L)
    }, wait = function(seconds) stop('must not wait'), .package = 'pollofpolls')

    error = expect_error(pollofpolls:::fetchUrl('https://example.org/x'), 'HTTP status 404',
                         class = 'pollofpolls_http_error')
    expect_equal(error$status, 404L)
    expect_equal(calls, 1)
})

test_that('fetchUrl does not hammer a server that asks for a long break', {
    local_mocked_bindings(
        httpGet = function(url) response(429L, headers = list('retry-after' = '3600')),
        wait = function(seconds) stop('must not wait'), .package = 'pollofpolls')

    expect_error(pollofpolls:::fetchUrl('https://example.org/'), 'asks to wait 3600 seconds')
})

test_that('Retry-After is understood as seconds and as http date', {
    expect_equal(pollofpolls:::retryAfter(list('retry-after' = '120')), 120)
    expect_null(pollofpolls:::retryAfter(list()))
    expect_null(pollofpolls:::retryAfter(list('retry-after' = 'soon')))

    withr::local_locale(c(LC_TIME = 'C'))
    httpDate = function(time) format(time, '%a, %d %b %Y %H:%M:%S GMT', tz = 'GMT')
    expect_lt(abs(pollofpolls:::retryAfter(list('retry-after' = httpDate(Sys.time() + 60))) - 60), 3)
    expect_equal(pollofpolls:::retryAfter(list('retry-after' = httpDate(Sys.time() - 60))), 0)
})

test_that('poll codes are encoded in the endpoint url', {
    expect_equal(pollofpolls:::endpointUrl('DE-parliament'),
                 'https://www.politico.eu/wp-json/politico/v1/poll-of-polls/DE-parliament')
    expect_match(pollofpolls:::endpointUrl('a b/c'), 'poll-of-polls/a%20b%2Fc$')
})

test_that('the cache expires and can be cleared', {
    localCache()

    pollofpolls:::cacheWrite('test', list(a = 1))
    expect_equal(pollofpolls:::cacheRead('test'), list(a = 1))
    expect_null(pollofpolls:::cacheRead('test', maxAge = -1))

    expect_true(popCacheClear())
    expect_null(pollofpolls:::cacheRead('test'))
    expect_false(popCacheClear())
})

test_that('the poll index is reused across sessions', {
    localCache()
    requests = 0
    local_mocked_bindings(fetchUrl = function(url) {
        requests <<- requests + 1
        readFixture('germany.html')
    }, .package = 'pollofpolls')

    pollofpolls:::getCodeMetadata('DE-parliament')
    # a new session starts with an empty memory cache
    rm(list = ls(pollofpolls:::.popCache), envir = pollofpolls:::.popCache)

    expect_equal(nrow(pollofpolls:::getCodeMetadata('DE-parliament')), 1)
    expect_equal(requests, 1)
})

test_that('poll data is only cached if asked for', {
    localCache()
    requests = 0
    local_mocked_bindings(fetchUrl = function(url) {
        requests <<- requests + 1
        readFixture('DE-parliament.json')
    }, .package = 'pollofpolls')

    popRead('DE-parliament', metadata = FALSE)
    popRead('DE-parliament', metadata = FALSE)
    expect_equal(requests, 2)

    withr::local_options(pollofpolls.dataMaxAge = 3600)
    first = popRead('DE-parliament', metadata = FALSE)
    second = popRead('DE-parliament', metadata = FALSE)
    expect_equal(requests, 3)
    expect_equal(second$polls, first$polls)
    expect_equal(second$retrieved, first$retrieved)
})
