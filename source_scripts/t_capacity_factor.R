# Check if this section was selected to run in the input file
if(capacity.factor.table) {

# Call the query function to get capacity factor.
cf = tryCatch( capacity_factor(yr.data.generator), error = function(cond) { return('ERROR: capacity_factor function not returning correct results.') })

} else { print('Section not run according to input file.') }
