
if (annual.cost.table) {
cost.table = tryCatch( costs(yr.data.generator), error = function(cond) { return('ERROR: costs function not returning correct results.') })
} else { print('Section not run according to input file.') }

