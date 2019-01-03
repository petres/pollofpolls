#' @import data.table
ident = function(data) {
    party = NULL; date = NULL; value = NULL; . = NULL # WARNINGS
    return (toLong(data)[!is.na(value), .(value = mean(value)), by=.(date, party)])
}
