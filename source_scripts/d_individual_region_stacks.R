
# Check to see if this file already exists, otherwise query the data.
if ( !exists('r.z.gen') ) { 
  # Query region and zonal generation using the annual generation data.
  r.z.gen = tryCatch( select(region_zone_gen(yr.data.generator), Region, Zone, Type, GWh = value), error = function(cond) { return('ERROR') } )
} 

# Check if zonal.gen query worked and create plot of regional generation, else return an error.
if ( typeof(r.z.gen)=='character' ) {
  print('ERROR: region_zone_gen function not returning correct results.')
} else {
  
  # Reorder the levels of Type to plot them in order
  r.z.gen$Type = factor(r.z.gen$Type, levels = gen.order)
    
  r.z.gen.plot = r.z.gen %>%
    group_by(Type, Region, Zone) %>% # Group by certain elements for next line of code to work
    dplyr::summarise(TWh = sum(GWh)/1000) %>% # Convert GWh to TWh
    filter(!Zone %in% ignore.zones) %>% # Remove any zones that are ignored according to input file
    filter(!Region %in% ignore.regions) # Remove any regions that are ignored according to input file.
}
  