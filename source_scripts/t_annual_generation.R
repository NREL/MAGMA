# Check if this section was selected to run in the input file
if (annual.generation.table){

  # If the data doesn't already exist, run the query function.
  if ( !exists('yr.gen') ) {
    # Query annual generation by type.
    yr.gen = tryCatch( gen_by_type(total.generation, total.avail.cap), error = function(cond) { return('ERROR: gen_by_type function not returning correct results') } ) 
  }
    
  # Check to see the data query didn't have any errors.
  if ( typeof(yr.gen)=='character' ) { 
    knitr::kable(yr.gen)
    
  } else {
     
    yr.gen$Type = factor(yr.gen$Type, levels = c(gen.order)) # Set the order level that generatino type will be displayed.
    yr.gen = yr.gen[order(yr.gen$Type),]
    
    # Separate out curtailment from generation
    curt = yr.gen[yr.gen$Type=='Curtailment',]
    yr.gen = yr.gen[yr.gen$Type!='Curtailment',]
    
    # Calculate the percent generation of each type, and percent curtailment
    gen.tot = data.frame('Type'='Total', 'GWh'=sum(yr.gen$GWh) )
    yr.gen = rbind(yr.gen, gen.tot)
    percent = data.frame('Percent'= yr.gen$GWh / yr.gen$GWh[yr.gen$Type=='Total'] * 100)
    curt$Percent = curt$GWh / sum( yr.gen[yr.gen$Type %in% re.types,'GWh'] + curt$GWh ) * 100
    
    # combine back into one table. 
    yr.gen = rbind( cbind(yr.gen, percent), curt)
    row.names(yr.gen) = NULL
  
  }

} else { print('Section not run according to input file.') }
