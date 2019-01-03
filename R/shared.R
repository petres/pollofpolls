#' @import data.table

as.date = function(x, origin='1970-01-01')
    as.Date(x, origin=origin)

toLong = function(data, what='polls') {
    value = NULL # WARNINGS
    melt(data[[what]], variable.name = "party", measure.vars=intersect(data$parties$code, colnames(data[[what]])))[!is.na(value)]
}

getPollVar = function(p, n)
    p*(1-p)/n

