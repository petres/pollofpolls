#' Download Poll Data
#'
#' Saves the data of several polls as JSON files, exactly as published by
#' POLITICO, e.g. to keep a snapshot or to work offline. The files can be read
#' with `popRead(code, dir = dir)`.
#'
#' Requests are sent one after the other with a delay in between (see the
#' `pollofpolls.requestDelay` option). A failed download does not stop the
#' others, see the `status` column of the result.
#'
#' @param dir Directory the files are written to, created if necessary.
#' @param codes Codes of the polls to download, by default all polls listed by
#'   [popGetInfo()].
#' @param overwrite Whether existing files should be replaced.
#' @param quiet Whether progress messages should be suppressed.
#'
#' @return Invisibly a `data.table` with the columns `code`, `file`, `status`
#'   (`"downloaded"`, `"skipped"` or `"failed"`) and `error`.
#' @export
#'
#' @examples
#' \dontrun{
#' popDownload('polls', codes = c('AT-parliament', 'DE-parliament'))
#' at = popRead('AT-parliament', dir = 'polls')
#'
#' # everything
#' popDownload('polls')
#' }
popDownload = function(dir, codes = NULL, overwrite = TRUE, quiet = FALSE) {
    if (is.null(codes))
        codes = popGetInfo()$code
    if (!is.character(codes) || anyNA(codes) || length(codes) == 0)
        stop('`codes` must be a character vector of poll codes', call. = FALSE)
    if (!dir.exists(dir) && !dir.create(dir, recursive = TRUE, showWarnings = FALSE))
        stop(sprintf("Could not create the directory '%s'", dir), call. = FALSE)

    codes = unique(codes)
    files = file.path(dir, paste0(codes, '.json'))
    status = rep('skipped', length(codes))
    errors = rep(NA_character_, length(codes))

    requested = FALSE
    for (i in seq_along(codes)) {
        if (!overwrite && file.exists(files[i]))
            next
        if (requested)
            throttle()
        requested = TRUE

        if (!quiet)
            message(sprintf('[%d/%d] %s', i, length(codes), codes[i]))

        errors[i] = tryCatch({
            content = fetchUrl(endpointUrl(codes[i]))
            if (!jsonlite::validate(content))
                stop('the response is not valid JSON', call. = FALSE)
            writeBin(charToRaw(enc2utf8(content)), files[i])
            NA_character_
        }, error = function(e) conditionMessage(e))
        status[i] = if (is.na(errors[i])) 'downloaded' else 'failed'
    }

    result = data.table(code = codes, file = files, status = status, error = errors)
    if (!quiet && any(status == 'failed'))
        message(sprintf('%d of %d downloads failed, see the error column of the result',
                        sum(status == 'failed'), length(codes)))

    invisible(result)
}
