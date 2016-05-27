
if(capacity.factor.table) {

cf = tryCatch( capacity_factor(yr.data.generator), error = function(cond) { return('ERROR: capacity_factor function not returning correct results.') })

} else { print('Section not run according to input file.') }
