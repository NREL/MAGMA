# Check if this section was selected to run in the input file
if(region.zone.gen.table) {

# If it doesn't already exist then call the query function.
if ( !exists('r.z.gen') ) {
  # Query region and zonal generation
  r.z.gen = tryCatch( select(region_zone_gen(total.generation, total.avail.cap), Region, Zone, Type, GWh = value), error = function(cond) { return('ERROR: region_zone_gen function not returning correct results.') } )
}
  
if ( typeof(r.z.gen)=='character' ) { 
  r.gen.table = 'ERROR: region_zone_gen function not returning correct results.'
  z.gen.table = 'ERROR: region_zone_gen function not returning correct results.'
} else {

  r.z.gen[, Type := factor(Type, levels = gen.order)] # Reassign the order which the generation type will be displayed.
  
  # Sum regional generation by type
  r.gen.table = r.z.gen[, .(GWh = sum(GWh)), by=.(Region, Type)] %>%
    dcast(Region~Type, value.var = 'GWh')
  
  # Sum zonal generation by type
  z.gen.table = r.z.gen[, .(GWh = sum(GWh)), by=.(Zone, Type)] %>%
    dcast(Zone~Type, value.var = 'GWh')
  
  # If the data looks good, add load to the regional generation.
  if (typeof(r.load)=='character' ) {
    r.gen.table = 'ERROR: region_load function not returning correct results.'  
  } else {
    region.load = r.load[, .(Region = name, Load = value)]
    setkey(region.load,Region)
    setkey(r.gen.table,Region)
    r.gen.table = r.gen.table[region.load]
  }
  
  # If the data looks good, add load to the zonal generation.
  if (typeof(z.load)=='character' ) {
    z.gen.table = 'ERROR: zone_load function not returning correct results.'
  } else {
    zone.load = z.load[, .(Zone = name, Load = value)]
    setkey(region.load,Zone)
    setkey(r.gen.table,Zone)
    z.gen.table = z.gen.table[zone.load]
  }
  
}

} else { print('Section not run according to input file.') }
