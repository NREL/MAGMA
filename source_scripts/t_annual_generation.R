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
     
    yr.gen[, Type := factor(Type, levels = c(gen.order))] # Set the order level that generatino type will be displayed.
    setorder(yr.gen,Type)
    
    # Separate out curtailment from generation
    curt = yr.gen[Type=='Curtailment', ]
    yr.gen = yr.gen[Type!='Curtailment', ]
    
    # Calculate the percent generation of each type, and percent curtailment
    yr.gen[, Total:=sum(GWh)]
    yr.gen[, Percent:=GWh/Total*100]
    yr.gen[, Total:=NULL]
    curt[, Total:=GWh+yr.gen[Type %in% re.types, sum(GWh)] ]
    curt[, Percent:=GWh/Total*100]
    curt[, Total:=NULL]
    
    # combine back into one table. 
    yr.gen = rbindlist(list(yr.gen, curt))
    row.names(yr.gen) = NULL
  
  }

} else { print('Section not run according to input file.') }
