# Columns of data.tables are referred to by name, which R CMD check cannot tell
# apart from undefined global variables.
utils::globalVariables(c(
    '.', 'code', 'color', 'date', 'firm', 'from', 'i.target', 'id', 'n',
    'party', 'target', 'title', 'to', 'url', 'value', 'var', 'variance',
    'weight', 'x.date'
))
