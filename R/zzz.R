# Columns of data.tables are referred to by name, which R CMD check cannot tell
# apart from undefined global variables.
utils::globalVariables(c(
    '.', 'N', 'V1', 'before', 'code', 'color', 'date', 'dateFrom', 'effect',
    'election', 'error', 'estimate', 'expected', 'firm', 'firmMedian', 'firmRaw',
    'from', 'i.target', 'id', 'kind', 'lower', 'n', 'name', 'party', 'polls',
    'pMajority', 'previous', 'remaining', 'result', 'seats', 'se', 'share', 'source',
    'target', 'title', 'to', 'trend', 'upper', 'url', 'value', 'var',
    'variance', 'weight', 'x.date'
))
