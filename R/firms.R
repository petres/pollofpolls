#' @import data.table

# Firm names -------------------------------------------------------------------
#
# The same polling firm is published under several spellings, e.g.
# "INSA/YouGov" and "INSA YouGov", "IPSOS" and "Ipsos" or "Tecnè" and "Tecne".
# Names that only differ in case, accents, punctuation and white space are
# therefore treated as one firm. Anything beyond that needs knowledge about the
# firms and is left to the user, see popRenameFirms().

# Latin letters with diacritics and their base letters. Folded with chartr()
# because the transliteration of iconv() depends on the platform and the locale.
.accents <- c(
    old = '\u00c0\u00c1\u00c2\u00c3\u00c4\u00c5\u00c7\u00c8\u00c9\u00ca\u00cb\u00cc\u00cd\u00ce\u00cf\u00d0\u00d1\u00d2\u00d3\u00d4\u00d5\u00d6\u00d8\u00d9\u00da\u00db\u00dc\u00dd\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5\u00e7\u00e8\u00e9\u00ea\u00eb\u00ec\u00ed\u00ee\u00ef\u00f0\u00f1\u00f2\u00f3\u00f4\u00f5\u00f6\u00f8\u00f9\u00fa\u00fb\u00fc\u00fd\u00ff\u0100\u0101\u0102\u0103\u0104\u0105\u0106\u0107\u0108\u0109\u010a\u010b\u010c\u010d\u010e\u010f\u0110\u0111\u0112\u0113\u0114\u0115\u0116\u0117\u0118\u0119\u011a\u011b\u011c\u011d\u011e\u011f\u0120\u0121\u0122\u0123\u0124\u0125\u0126\u0127\u0128\u0129\u012a\u012b\u012c\u012d\u012e\u012f\u0130\u0131\u0132\u0133\u0134\u0135\u0136\u0137\u0139\u013a\u013b\u013c\u013d\u013e\u013f\u0140\u0141\u0142\u0143\u0144\u0145\u0146\u0147\u0148\u014c\u014d\u014e\u014f\u0150\u0151\u0154\u0155\u0156\u0157\u0158\u0159\u015a\u015b\u015c\u015d\u015e\u015f\u0160\u0161\u0162\u0163\u0164\u0165\u0166\u0167\u0168\u0169\u016a\u016b\u016c\u016d\u016e\u016f\u0170\u0171\u0172\u0173\u0174\u0175\u0176\u0177\u0178\u0179\u017a\u017b\u017c\u017d\u017e\u017f',
    new = 'AAAAAACEEEEIIIIDNOOOOOOUUUUYaaaaaaceeeeiiiidnoooooouuuuyyAaAaAaCcCcCcCcDdDdEeEeEeEeEeGgGgGgGgHhHhIiIiIiIiIiIiJjKkLlLlLlLlLlNnNnNnOoOoOoRrRrRrSsSsSsSsTtTtTtUuUuUuUuUuUuWwYyYZzZzZzs'
)

# Key under which spellings of the same firm are merged: lower case letters and
# digits only.
firmKey = function(x) {
    key = chartr(.accents[['old']], .accents[['new']], x)
    key = tolower(gsub('[^\\p{L}\\p{N}]', '', key, perl = TRUE))

    # names without any letter or digit are only merged with identical ones
    blank = !is.na(key) & !nzchar(key)
    key[blank] = x[blank]
    key
}

# Gives every firm the most frequent of its spellings.
unifyFirms = function(firms) {
    if (length(firms) == 0)
        return(firms)

    # `key` is an argument of data.table(), hence `id`
    spellings = data.table(firm = firms, id = firmKey(firms))[!is.na(firm), .N, by = .(id, firm)]
    setorder(spellings, id, -N, firm)
    canonical = spellings[, .(firm = firm[1]), by = id]

    canonical$firm[match(firmKey(firms), canonical$id)]
}

checkFirmNames = function(firms) {
    if (!is.character(firms) || is.null(names(firms)) || anyNA(firms) ||
        anyNA(names(firms)) || !all(nzchar(names(firms))))
        stop('`firms` must be a named character vector such as c("Peter Hajek" = "Hajek")',
             call. = FALSE)
}

# Renames firms as given by the named vector `firms` (old = new), matching the
# old names ignoring case, accents, punctuation and white space.
renameFirms = function(x, firms) {
    if (length(firms) == 0 || length(x) == 0)
        return(x)
    checkFirmNames(firms)

    # the new names are aliases of themselves, so that their other spellings
    # end up with the requested spelling as well
    lookup = c(firms, stats::setNames(unname(firms), unname(firms)))
    index = match(firmKey(x), firmKey(names(lookup)))
    x[!is.na(index)] = unname(lookup)[index[!is.na(index)]]
    x
}

#' Rename Polling Firms
#'
#' Merges polling firms that are published under different names. Spellings
#' that only differ in case, accents, punctuation and white space (such as
#' `"INSA/YouGov"` and `"INSA YouGov"`) are merged by [popRead()] already; this
#' function is for everything that needs knowledge about the firms, e.g. a firm
#' that has been renamed or polls published under the name of a partner.
#'
#' Renamings that should apply to every [popRead()] can be set once with
#' `options(pollofpolls.firms = c(...))`. Renamings are not chained: with
#' `c(A = "B", B = "C")` firm `A` becomes `B`, not `C`.
#'
#' @param x A `popPolls` object.
#' @param firms Named character vector: the names are the firm names to
#'   replace, the values the names to use instead, e.g.
#'   `c("Peter Hajek" = "Hajek")`. The names are matched ignoring case, accents,
#'   punctuation and white space.
#'
#' @return `x` with the `firm` column of `$polls` renamed. The names as
#'   published are kept in the `firmRaw` column.
#' @export
#'
#' @examples
#' \dontrun{
#' at = popRead('AT-parliament')
#' popFirms(at)
#' at = popRenameFirms(at, c('Peter Hajek' = 'Hajek', 'Market' = 'Market/Lazarsfeld'))
#'
#' # for all polls read from now on
#' options(pollofpolls.firms = c('Peter Hajek' = 'Hajek'))
#' }
popRenameFirms = function(x, firms) {
    checkPopPolls(x, 'x')
    checkFirmNames(firms)
    if (nrow(x$polls) == 0 || !'firm' %in% names(x$polls))
        return(x)

    renamed = renameFirms(x$polls$firm, firms)
    x$polls = copy(x$polls)
    x$polls[, firm := renamed]
    x
}


# Sample sizes ------------------------------------------------------------------

# Sample size assumed if no poll reports one.
.defaultSampleSize <- 400

# Sample sizes of the polls. With `missing = "firm"` a missing sample size is
# replaced by the median sample size of the same firm, by the median of all
# polls if the firm never reports one and by .defaultSampleSize if no poll
# does. A number for `missing` is used for every poll without sample size.
fillSampleSizes = function(polls, missing = 'firm') {
    if (!(identical(missing, 'firm') || (is.numeric(missing) && length(missing) == 1 &&
                                          !is.na(missing) && missing > 0)))
        stop('`missingSampleSize` must be "firm" or a positive number', call. = FALSE)

    sizes = data.table(
        n = if ('n' %in% names(polls)) as.numeric(polls$n) else rep(NA_real_, nrow(polls)),
        firm = if ('firm' %in% names(polls)) polls$firm else rep(NA_character_, nrow(polls))
    )
    if (is.numeric(missing))
        return(sizes[is.na(n), n := missing]$n)

    sizes[, firmMedian := stats::median(n, na.rm = TRUE), by = firm]
    sizes[is.na(n) & !is.na(firm), n := firmMedian]

    overall = stats::median(sizes$n, na.rm = TRUE)
    sizes[is.na(n), n := if (is.na(overall)) .defaultSampleSize else overall]
    sizes$n
}
