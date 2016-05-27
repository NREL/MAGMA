
if(annual.reserves.table) {

annual.reserves = tryCatch( annual_reserves(yr.data.reserve), error = function(cond) { return('ERROR: annual_reserves function not returning correct results.') })

} else { print('Section not run according to input file.') }
