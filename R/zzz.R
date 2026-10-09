# Columns of data.tables are referred to by name, which R CMD check cannot tell
# apart from undefined global variables.
utils::globalVariables(c(
    '.', 'code', 'color', 'date', 'dateFrom', 'election', 'expected', 'firm',
    'from', 'i.target', 'id', 'lower', 'n', 'name', 'party', 'previous',
    'seats', 'share', 'target', 'title', 'to', 'trend', 'upper', 'url',
    'value', 'var', 'variance', 'weight', 'x.date'
))
