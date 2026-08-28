# Poll index -----------------------------------------------------------------
#
# POLITICO does not publish a machine readable list of the available polls, so
# the index (poll code, title and party colours) has to be scraped from the
# country pages. Because that is expensive, the result is cached and single
# pages are fetched on demand whenever possible.

.basePage <- 'https://www.politico.eu/europe-poll-of-polls/'
.baseEndpoint <- 'https://www.politico.eu/wp-json/politico/v1/poll-of-polls/%s'

# Country pages a poll code is most likely found on. Used only as a hint: an
# unknown or outdated entry just means that the full index is built instead.
.iso2Slugs <- c(
    AT = 'austria',        BE = 'belgium',        BG = 'bulgaria',
    CH = 'switzerland',    CY = 'cyprus',         CZ = 'czech-republic',
    DE = 'germany',        DK = 'denmark',        EE = 'estonia',
    ES = 'spain',          EU = 'european-parliament-election',
    FI = 'finland',        FR = 'france',         GB = 'united-kingdom',
    GR = 'greece',         HR = 'croatia',        HU = 'hungary',
    IE = 'ireland',        IT = 'italy',          LT = 'lithuania',
    LU = 'luxembourg',     LV = 'latvia',         MT = 'malta',
    NL = 'netherlands',    NO = 'norway',         PL = 'poland',
    PT = 'portugal',       RO = 'romania',        SE = 'sweden',
    SI = 'slovenia',       SK = 'slovakia'
)

.plotDivPattern <- '<div[^>]+class="plot-trend pop-d3"[^>]*>'
.colorPattern <- 'data-([-A-Za-z0-9_]+)-color="#([0-9A-Fa-f]{3,8})"'


# HTML helpers ----------------------------------------------------------------

decodeEntities = function(x) {
    if (length(x) == 0)
        return(x)

    named = c('&nbsp;' = ' ', '&amp;' = '&', '&lt;' = '<', '&gt;' = '>',
              '&quot;' = '"', '&apos;' = "'", '&mdash;' = '\u2014',
              '&ndash;' = '\u2013', '&hellip;' = '\u2026')
    for (entity in names(named))
        x = gsub(entity, named[[entity]], x, fixed = TRUE)

    vapply(x, function(s) {
        if (is.na(s))
            return(NA_character_)
        positions = gregexpr('&#x?[0-9A-Fa-f]+;', s, perl = TRUE)[[1]]
        if (positions[1] == -1)
            return(s)
        entities = regmatches(s, list(positions))[[1]]
        decoded = vapply(entities, function(e) {
            hex = grepl('^&#x', e, ignore.case = TRUE)
            number = sub(';$', '', sub('^&#x?', '', e, ignore.case = TRUE))
            intToUtf8(strtoi(number, base = if (hex) 16L else 10L))
        }, character(1), USE.NAMES = FALSE)
        regmatches(s, list(positions)) = list(decoded)
        s
    }, character(1), USE.NAMES = FALSE)
}

stripTags = function(x)
    trimws(decodeEntities(gsub('<[^>]+>', '', x)))

# Extracts a single attribute out of a tag. Note that the tags are spread over
# several lines, so '.' (which does not match newlines) must not be used here.
tagAttribute = function(tag, name) {
    pattern = sprintf('%s="[^"]*"', name)
    match = regmatches(tag, regexpr(pattern, tag, perl = TRUE))
    if (length(match) == 0)
        return(NA_character_)
    sub('"$', '', sub(sprintf('^%s="', name), '', match, perl = TRUE))
}

tagColors = function(tag) {
    matches = regmatches(tag, gregexpr(.colorPattern, tag, perl = TRUE))[[1]]
    if (length(matches) == 0)
        return(stats::setNames(character(), character()))

    codes = sub(.colorPattern, '\\1', matches, perl = TRUE)
    values = paste0('#', sub(.colorPattern, '\\2', matches, perl = TRUE))
    stats::setNames(values, codes)
}

# Title of the chart, i.e. the text of the last heading in front of it.
headingBefore = function(html, position) {
    prefix = substr(html, 1, position)
    headings = gregexpr('<h[1-6][^>]*>', prefix, perl = TRUE)[[1]]
    if (headings[1] == -1)
        return(NA_character_)

    remainder = substr(prefix, headings[length(headings)], nchar(prefix))
    closing = regexpr('</h[1-6]>', remainder, perl = TRUE)
    if (closing[1] == -1)
        return(NA_character_)

    title = stripTags(substr(remainder, 1, closing[1] - 1))
    if (!nzchar(title)) NA_character_ else title
}


# Index building --------------------------------------------------------------

emptyMetadata = function()
    data.table(code = character(), title = character(), url = character(),
               colors = list())

parsePollPage = function(url, html = NULL) {
    if (is.null(html))
        html = fetchUrl(url)

    positions = gregexpr(.plotDivPattern, html, perl = TRUE)[[1]]
    if (positions[1] == -1)
        return(emptyMetadata())

    divs = regmatches(html, list(positions))[[1]]
    rows = lapply(seq_along(divs), function(i) {
        code = tagAttribute(divs[[i]], 'data-code')
        if (is.na(code) || !nzchar(code))
            return(NULL)

        data.table(code = code,
                   title = headingBefore(html, positions[i]),
                   url = url,
                   colors = list(tagColors(divs[[i]])))
    })

    rows = rows[!vapply(rows, is.null, logical(1))]
    if (length(rows) == 0)
        return(emptyMetadata())

    unique(rbindlist(rows), by = 'code')
}

countryPages = function(html) {
    pattern = 'href="(https://www\\.politico\\.eu/europe-poll-of-polls/[^"#?]+/)"'
    matches = regmatches(html, gregexpr(pattern, html, perl = TRUE))[[1]]
    if (length(matches) == 0)
        return(character())

    unique(sub(pattern, '\\1', matches, perl = TRUE))
}

buildPollMetadata = function() {
    html = fetchUrl(.basePage)
    pages = unique(c(.basePage, countryPages(html)))

    if (length(pages) > 1)
        message(sprintf('Indexing %d Poll of Polls pages, this is done once and then cached ...',
                        length(pages)))

    parts = list(parsePollPage(.basePage, html = html))
    for (page in setdiff(pages, .basePage)) {
        throttle()
        parts[[length(parts) + 1]] = tryCatch(parsePollPage(page),
                                              error = function(e) emptyMetadata())
    }

    storeMetadata(rbindlist(parts), complete = TRUE)
}

storeMetadata = function(metadata, complete = FALSE) {
    if (nrow(metadata) > 0) {
        metadata = unique(metadata, by = 'code')
        setorder(metadata, 'code')
    }

    .popCache$metadata = metadata
    .popCache$complete = complete || isTRUE(.popCache$complete)

    cacheWrite('metadata', list(metadata = metadata, complete = .popCache$complete))
    metadata
}

loadMetadata = function() {
    if (!is.null(.popCache$metadata))
        return(.popCache$metadata)

    cached = cacheRead('metadata')
    if (!is.list(cached) || !data.table::is.data.table(cached$metadata) ||
        !all(c('code', 'title', 'url', 'colors') %in% names(cached$metadata)))
        cached = list(metadata = emptyMetadata(), complete = FALSE)

    .popCache$metadata = cached$metadata
    .popCache$complete = isTRUE(cached$complete)
    .popCache$metadata
}

getPollMetadata = function(refresh = FALSE) {
    if (!refresh) {
        metadata = loadMetadata()
        if (isTRUE(.popCache$complete))
            return(metadata)
    }

    buildPollMetadata()
}

hintPage = function(code) {
    iso2 = toupper(sub('-.*', '', code))
    if (!iso2 %in% names(.iso2Slugs))
        return(NULL)

    paste0(.basePage, .iso2Slugs[[iso2]], '/')
}

# Metadata for a single poll code, fetching as few pages as possible.
getCodeMetadata = function(code) {
    # `code` is also a column name, so the lookup value needs its own symbol
    pollCode = code
    lookup = function(metadata) {
        if (nrow(metadata) == 0)
            return(emptyMetadata())
        metadata[code == pollCode]
    }

    result = lookup(loadMetadata())
    if (nrow(result) > 0)
        return(result)

    page = hintPage(code)
    if (!is.null(page)) {
        pageMetadata = tryCatch(parsePollPage(page), error = function(e) emptyMetadata())
        if (nrow(pageMetadata) > 0) {
            result = lookup(storeMetadata(rbind(loadMetadata(), pageMetadata, fill = TRUE)))
            if (nrow(result) > 0)
                return(result)
        }
    }

    if (!isTRUE(.popCache$complete))
        result = lookup(getPollMetadata(refresh = TRUE))

    result
}
