
if(region.zone.gen.table) {

if ( !exists('r.z.gen') ) {
  # Query region and zonal generation
  r.z.gen = tryCatch( select(region_zone_gen(yr.data.generator), Region, Zone, Type, GWh = value), error = function(cond) { return('ERROR: region_zone_gen function not returning correct results.') } )
}
  
if ( typeof(r.z.gen)=='character' ) { 
  r.gen.table = 'ERROR: region_zone_gen function not returning correct results.'
  z.gen.table = 'ERROR: region_zone_gen function not returning correct results.'
} else {

  r.z.gen$Type = factor(r.z.gen$Type, levels = gen.order)
  
  r.gen.table = r.z.gen %>%
    dcast(Region~Type, value.var = 'GWh', fun.aggregate = sum)
  
  z.gen.table = r.z.gen %>%
    dcast(Zone~Type, value.var = 'GWh', fun.aggregate = sum)
  
  if (typeof(r.load)=='character' ) {
    r.gen.table = 'ERROR: region_load function not returning correct results.'  
  } else {
    region.load = select(r.load, Region = name, Load = value)
    r.gen.table = join(r.gen.table, region.load, by = 'Region')
  }
  
  if (typeof(z.load)=='character' ) {
    z.gen.table = 'ERROR: zone_load function not returning correct results.'
  } else {
    zone.load = select(z.load, Zone = name, Load = value)
    z.gen.table = join(z.gen.table, zone.load, by = 'Zone')
  }
  
}

} else { print('Section not run according to input file.') }
