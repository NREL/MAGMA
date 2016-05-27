if ( !exists('r.z.gen') ) {
  # Query region and zonal generation
  r.z.gen = tryCatch( select(region_zone_gen(yr.data.generator), Region, Zone, Type, GWh = value), error = function(cond) { return('ERROR') } )
} 

# Check if zonal.gen query worked and create plot of regional gen, else return an error.
if ( typeof(r.z.gen)=='character' ) {
  print('ERROR: region_zone_gen function not returning correct results.')
} else {
  
  # reorder the levels of Type to plot them in order
  r.z.gen$Type = factor(r.z.gen$Type, levels = gen.order)
    
  # Convert GWh to TWh
  r.z.gen.plot = r.z.gen %>%
    group_by(Type, Region, Zone) %>%
    dplyr::summarise(TWh = sum(GWh)/1000) %>%
    filter(!Zone %in% ignore.zones) %>%
    filter(!Region %in% ignore.regions)
}