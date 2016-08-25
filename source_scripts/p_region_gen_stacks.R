# Check if this section was selected run in the input file
if (region.gen.stacks) {

if( !exists('r.z.gen') ) {
  # Query region and zonal generation
  r.z.gen = tryCatch( region_zone_gen(total.generation, total.avail.cap), error = function(cond) { return('ERROR') } )
}

# Check if zonal.gen query worked and create plot of regional gen, else return an error.
if ( typeof(r.z.gen)=='character' ) {
  print('ERROR: region_zone_gen function not returning correct results.')
} else if ( typeof(r.load) == 'character' ) {
  print('ERROR: region_load function not returning correct results.')
} else {
  # Make sure all loads and zones are present
  setkey(r.load,Region)
  setkey(rz.unique,Region)
  region.load = rz.unique[r.load]
  region.load = region.load[complete.cases(region.load),]
  # Create and plot data
  p1 <- gen_stack_plot(r.z.gen[(!Zone %in% ignore.zones && !Region %in% ignore.regions),],
                     region.load[(!Zone %in% ignore.zones && !Region %in% ignore.regions),],
                     filters = c('Region','Zone'))
  p1 <- p1 + facet_wrap(~Zone, scales = 'free', ncol=2)
}

} else { print('Section not run according to input file.') }
