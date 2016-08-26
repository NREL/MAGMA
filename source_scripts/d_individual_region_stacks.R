
# Check to see if this file already exists, otherwise query the data.
if ( !exists('r.z.gen') ) { 
  # Query region and zonal generation using the annual generation data.
  r.z.gen = tryCatch( region_zone_gen(total.generation, total.avail.cap)[, .(GWh=value), by=.(Region, Zone, Type)], error = function(cond) { return('ERROR') } )
} 

# Check if zonal.gen query worked and create plot of regional generation, else return an error.
if ( typeof(r.z.gen)=='character' ) {
  print('ERROR: region_zone_gen function not returning correct results.')
} else {
  
  # Reorder the levels of Type to plot them in order
  r.z.gen[, Type := factor(Type, levels = gen.order)]
    
  r.z.gen.plot = r.z.gen[, .(TWh = sum(GWh)/1000) , by=.(Type, Region, Zone)]
  # Remove any regions or zones that are ignored according to input file.
  r.z.gen.plot = r.z.gen.plot[(!Zone %in% ignore.zones & !Region %in% ignore.regions), ] 
}
  