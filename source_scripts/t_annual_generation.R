
if (annual.generation.table){

  if ( !exists('yr.gen') ) {
    # Query annual generation by type.
    yr.gen = tryCatch( gen_by_type(yr.data.generator), error = function(cond) { return('ERROR: gen_by_type function not returning correct results') } ) 
  }
    
  if ( typeof(yr.gen)=='character' ) { 
    knitr::kable(yr.gen)
    
  } else {
     
    yr.gen$Type = factor(yr.gen$Type, levels = c(gen.order))
    yr.gen = yr.gen[order(yr.gen$Type),]
    row.names(yr.gen) = NULL
    
    curt = yr.gen[yr.gen$Type=='Curtailment',]
    yr.gen = yr.gen[yr.gen$Type!='Curtailment',]
    
    gen.tot = data.frame('Type'='Total', 'GWh'=sum(yr.gen$GWh) )
    yr.gen = rbind(yr.gen, gen.tot)
    percent = data.frame('Percent'= yr.gen$GWh / yr.gen$GWh[yr.gen$Type=='Total'] * 100)
    curt$Percent = curt$GWh / sum( yr.gen[yr.gen$Type %in% re.types,'GWh'] + curt$GWh ) * 100
    
    yr.gen = rbind( cbind(yr.gen, percent), curt)
  
  }

} else { print('Section not run according to input file.') }
