test_that('popDownload saves the payloads for popRead', {
    expected = readTestPolls()
    withr::local_options(pollofpolls.cache = FALSE, pollofpolls.requestDelay = 0)
    dir = withr::local_tempdir()
    requested = character()
    local_mocked_bindings(fetchUrl = function(url) {
        requested <<- c(requested, url)
        if (grepl('broken', url))
            stop('HTTP status 500', call. = FALSE)
        if (grepl('html', url))
            return('<html></html>')
        readFixture('DE-parliament.json')
    }, .package = 'pollofpolls')

    messages = capture_messages(
        result <- popDownload(dir, codes = c('DE-parliament', 'XX-broken', 'XX-html')))
    expect_match(messages[1], '[1/3] DE-parliament', fixed = TRUE)
    expect_match(messages[4], '2 of 3 downloads failed')
    expect_equal(result$status, c('downloaded', 'failed', 'failed'))
    expect_match(result$error[2], 'HTTP status 500')
    expect_match(result$error[3], 'not valid JSON')
    expect_true(file.exists(file.path(dir, 'DE-parliament.json')))
    expect_false(file.exists(file.path(dir, 'XX-broken.json')))

    # reading from the directory does not send a request
    de = popRead('DE-parliament', dir = dir, metadata = FALSE)
    expect_equal(de$polls, expected$polls)
    expect_equal(de$trends, expected$trends)
    expect_length(requested, 3)

    again = popDownload(dir, codes = 'DE-parliament', overwrite = FALSE, quiet = TRUE)
    expect_equal(again$status, 'skipped')
    expect_length(requested, 3)

    expect_error(popRead('AT-parliament', dir = dir), 'popDownload')
})
