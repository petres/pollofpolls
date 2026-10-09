# HTTP access and on-disk caching --------------------------------------------
#
# Everything in this file is internal. The package talks to a public website,
# so it tries hard to be a good citizen: it identifies itself, accepts
# compressed responses, waits between requests, retries transient failures as
# advised by the server and caches what it has already seen.

.popCache <- new.env(parent = emptyenv())

popUserAgent = function() {
    version = tryCatch(as.character(utils::packageVersion('pollofpolls')),
                       error = function(e) 'dev')
    sprintf('pollofpolls/%s (R package; +https://github.com/petres/pollofpolls)', version)
}

endpointUrl = function(code)
    sprintf(.baseEndpoint, utils::URLencode(code, reserved = TRUE))

# Performs a single GET request. Never throws: transport errors (DNS, timeouts,
# ...) are reported with status NA, so that the caller can decide whether a
# failure is worth retrying. curl asks for gzip compressed responses by default.
httpGet = function(url) {
    handle = curl::new_handle(useragent = popUserAgent(),
                              timeout = getOption('pollofpolls.timeout', 60))

    response = tryCatch(curl::curl_fetch_memory(url, handle = handle),
                        error = function(e) e)
    if (inherits(response, 'error'))
        return(list(status = NA_integer_, content = NULL, headers = list(),
                    reason = conditionMessage(response)))

    content = rawToChar(response$content)
    Encoding(content) = 'UTF-8'
    list(status = response$status_code,
         content = content,
         headers = curl::parse_headers_list(response$headers),
         reason = sprintf('HTTP status %d', response$status_code))
}

isSuccess = function(status)
    !is.na(status) && status >= 200L && status < 300L

# Only rate limiting and server side errors are worth a second attempt.
isRetryable = function(status)
    is.na(status) || status == 429L || status >= 500L

# Seconds to wait as requested by a Retry-After header (either a number of
# seconds or an HTTP date), NULL if there is no usable header.
retryAfter = function(headers) {
    value = headers[['retry-after']]
    if (is.null(value) || !nzchar(value))
        return(NULL)

    seconds = suppressWarnings(as.numeric(value))
    if (is.na(seconds)) {
        date = tryCatch(curl::parse_date(value), error = function(e) NA)
        if (length(date) != 1 || is.na(date))
            return(NULL)
        seconds = as.numeric(difftime(date, Sys.time(), units = 'secs'))
    }

    max(0, seconds)
}

# Wrapper around Sys.sleep(), so that tests can replace it.
wait = function(seconds) {
    if (seconds > 0)
        Sys.sleep(seconds)
    invisible(NULL)
}

#' @param url URL to download.
#' @param attempts Number of attempts before giving up.
#' @noRd
fetchUrl = function(url, attempts = getOption('pollofpolls.attempts', 3L)) {
    delay = getOption('pollofpolls.retryDelay', 1)
    maxDelay = getOption('pollofpolls.maxRetryDelay', 60)

    for (i in seq_len(max(1L, attempts))) {
        result = httpGet(url)
        if (isSuccess(result$status))
            return(result$content)
        if (!isRetryable(result$status) || i == attempts)
            break

        requested = retryAfter(result$headers)
        if (!is.null(requested) && requested > maxDelay)
            stop(sprintf("Failed to fetch '%s': %s, the server asks to wait %d seconds before trying again",
                         url, result$reason, ceiling(requested)), call. = FALSE)

        wait(if (is.null(requested)) min(maxDelay, delay * 2^(i - 1)) else requested)
    }

    # POLITICO's CDN rejects the address ranges of many hosting providers
    hint = if (identical(result$status, 403L))
        paste0('. POLITICO refuses requests from many cloud servers (such as CI runners): run ',
               'the code on a local machine, or download the data there with popDownload() ',
               'and read it with popRead(dir = ...)') else ''

    stop(errorCondition(sprintf("Failed to fetch '%s': %s%s", url, result$reason, hint),
                        class = 'pollofpolls_http_error', status = result$status, call = NULL))
}

# Waits between consecutive requests to the same host.
throttle = function() {
    wait(getOption('pollofpolls.requestDelay', 0.5))
}

# Payload of the data endpoint for `code` and the time it was retrieved. Cached
# on disk only if options(pollofpolls.dataMaxAge) is set to a positive number of
# seconds.
fetchData = function(code) {
    maxAge = getOption('pollofpolls.dataMaxAge', 0)
    key = cacheKey('data', code)

    if (maxAge > 0) {
        cached = cacheRead(key, maxAge = maxAge)
        if (is.list(cached) && is.character(cached$content))
            return(cached)
    }

    result = list(content = fetchUrl(endpointUrl(code)), retrieved = Sys.time())
    if (maxAge > 0)
        cacheWrite(key, result)

    result
}


# On-disk cache ---------------------------------------------------------------

cacheDir = function()
    tools::R_user_dir('pollofpolls', 'cache')

cacheEnabled = function()
    isTRUE(getOption('pollofpolls.cache', TRUE))

cacheKey = function(prefix, code)
    paste0(prefix, '-', gsub('[^A-Za-z0-9_.-]', '_', code))

cacheFile = function(key)
    file.path(cacheDir(), paste0(key, '.rds'))

cacheRead = function(key, maxAge = getOption('pollofpolls.cacheMaxAge', 86400)) {
    if (!cacheEnabled())
        return(NULL)

    file = cacheFile(key)
    if (!file.exists(file))
        return(NULL)
    if (as.numeric(difftime(Sys.time(), file.mtime(file), units = 'secs')) > maxAge)
        return(NULL)

    tryCatch(readRDS(file), error = function(e) NULL)
}

cacheWrite = function(key, value) {
    if (!cacheEnabled())
        return(invisible(FALSE))

    dir = cacheDir()
    if (!dir.exists(dir) && !dir.create(dir, recursive = TRUE, showWarnings = FALSE))
        return(invisible(FALSE))

    invisible(tryCatch({
        saveRDS(value, cacheFile(key))
        TRUE
    }, error = function(e) FALSE))
}

#' Clear the Cache
#'
#' `popGetInfo()` and `popRead()` remember which polls exist and which colours
#' belong to which party. If `options(pollofpolls.dataMaxAge)` is set, the
#' downloaded poll data is kept as well. Everything is kept for the running
#' session and, unless `options(pollofpolls.cache = FALSE)` is set, in
#' `tools::R_user_dir("pollofpolls", "cache")`. Use this function to drop it.
#'
#' @return Invisibly `TRUE` if cache files were removed, `FALSE` otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' popCacheClear()
#' }
popCacheClear = function() {
    rm(list = ls(.popCache), envir = .popCache)

    files = list.files(cacheDir(), pattern = '\\.rds$', full.names = TRUE)
    if (length(files) == 0)
        return(invisible(FALSE))

    invisible(all(file.remove(files)))
}
