
if ( !exists('zone.gen') & !exists('region.gen') ) {
  # Query region and zonal generation
  region.gen = tryCatch( select(region_zone_gen(), Region, Zone, Type, GWh = value), error = function(cond) { return('ERROR') } )
} else if ( exists('zone.gen') ) {
  region.gen = zone.gen
}

# Check if zonal.gen query worked and create plot of regional gen, else return an error.
if ( typeof(region.gen)=='character' ) {
  print('ERROR: region_zone_gen function not returning correct results.')
} else {
  
  # reorder the levels of Type to plot them in order
  region.gen$Type = factor(region.gen$Type, levels = gen.order)
    
  # Convert GWh to TWh
  region.gen.plot = region.gen %>%
    group_by(Type, Region, Zone) %>%
    dplyr::summarise(TWh = sum(GWh)/1000) %>%
    filter(Zone != 'BHUTAN')
}
