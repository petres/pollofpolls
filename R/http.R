# HTTP access and on-disk caching --------------------------------------------
#
# Everything in this file is internal. The package talks to a public website,
# so it tries hard to be a good citizen: it identifies itself, waits between
# requests, retries transient failures and caches what it has already seen.

.popCache <- new.env(parent = emptyenv())

popUserAgent = function() {
    version = tryCatch(as.character(utils::packageVersion('pollofpolls')),
                       error = function(e) 'dev')
    sprintf('pollofpolls/%s (R package; +https://github.com/petres/pollofpolls)', version)
}

# Reads an URL into a single string, identifying the package as the caller.
readUrl = function(url) {
    timeout = getOption('pollofpolls.timeout', 60)
    old = options(timeout = max(getOption('timeout', 60), timeout))
    on.exit(options(old), add = TRUE)

    con = base::url(url, open = 'rb', headers = c('User-Agent' = popUserAgent()))
    on.exit(close(con), add = TRUE)

    paste(readLines(con, warn = FALSE, encoding = 'UTF-8'), collapse = '\n')
}

# Wraps readUrl() and reports the HTTP status instead of throwing, so that the
# caller can decide whether a failure is worth retrying.
tryUrl = function(url) {
    status = NA_integer_
    reason = NULL

    content = withCallingHandlers(
        tryCatch(readUrl(url), error = function(e) {
            # the warning below is raised first and carries the better message
            if (is.null(reason)) reason <<- conditionMessage(e)
            NULL
        }),
        warning = function(w) {
            reason <<- conditionMessage(w)
            statusText = sub(".*HTTP status was '([0-9]{3}).*", '\\1', reason)
            if (grepl('^[0-9]{3}$', statusText))
                status <<- as.integer(statusText)
            invokeRestart('muffleWarning')
        })

    list(content = content, status = status, reason = reason)
}

# Only rate limiting and server side errors are worth a second attempt.
isRetryable = function(status)
    is.na(status) || status == 429L || status >= 500L

#' @param url URL to download.
#' @param attempts Number of attempts before giving up.
#' @noRd
fetchUrl = function(url, attempts = getOption('pollofpolls.attempts', 3L)) {
    delay = getOption('pollofpolls.retryDelay', 1)
    result = list(content = NULL, status = NA_integer_, reason = NULL)

    for (i in seq_len(attempts)) {
        result = tryUrl(url)
        if (!is.null(result$content))
            return(result$content)
        if (!isRetryable(result$status))
            break
        if (i < attempts)
            Sys.sleep(delay * 2^(i - 1))
    }

    stop(sprintf("Failed to fetch '%s'%s", url,
                 if (is.null(result$reason)) '' else paste0(': ', result$reason)),
         call. = FALSE)
}

# Waits between consecutive requests to the same host.
throttle = function() {
    delay = getOption('pollofpolls.requestDelay', 0.5)
    if (delay > 0)
        Sys.sleep(delay)
    invisible(NULL)
}


# On-disk cache ---------------------------------------------------------------

cacheDir = function()
    tools::R_user_dir('pollofpolls', 'cache')

cacheEnabled = function()
    isTRUE(getOption('pollofpolls.cache', TRUE))

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

#' Clear the Cached Poll Index
#'
#' `popGetInfo()` and `popRead()` remember which polls exist and which colours
#' belong to which party. The index is kept for the running session and, unless
#' `options(pollofpolls.cache = FALSE)` is set, in
#' `tools::R_user_dir("pollofpolls", "cache")`. Use this function to drop it.
#'
#' @return Invisibly `TRUE` if a cache file was removed, `FALSE` otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' popCacheClear()
#' }
popCacheClear = function() {
    rm(list = ls(.popCache), envir = .popCache)

    file = cacheFile('metadata')
    if (file.exists(file))
        return(invisible(file.remove(file)))

    invisible(FALSE)
}
