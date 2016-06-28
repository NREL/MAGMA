# Check if this section was selected to run in the input file
if (annual.cost.table) {
# Run the cost query function
cost.table = tryCatch( costs(total.emissions.cost, total.fuel.cost, total.ss.cost, total.vom.cost), error = function(cond) { return('ERROR: costs function not returning correct results.') })
} else { print('Section not run according to input file.') }

