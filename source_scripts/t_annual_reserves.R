# Check if this section was selected to run in the input file
if(annual.reserves.table) {

# Call the query function to get annual reserve amounts.
annual.reserves = tryCatch( annual_reserves(total.reserve.provision, total.reserve.shortage), error = function(cond) { return('ERROR: annual_reserves function not returning correct results.') })

} else { print('Section not run according to input file.') }
