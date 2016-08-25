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
          
    if (length(unique(r.z.gen.plot$Region))>length(unique(r.z.gen.plot$Zone))) {
      region.load = r.load[!Region %in% ignore.regions, ] # Remove load data from regions being ignored
      region.load[, value := value/1000]
      
      setkey(region.load,Region)
      setkey(rz.unique,Region)
      region.load = rz.unique[region.load][!Zone %in% ignore.zones, ] # ignored regions removed above
      region.load = region.load[complete.cases(region.load),]
      plot.load = region.load

      x.col = 'Region'
      facet = facet_wrap(~Zone, scales = 'free', ncol=2)
    } else{
      zone.load = z.load[!Zone %in% ignore.zones, ] # Remove load data from regions being ignored
      zone.load[, value := value/1000]

      setkey(zone.load,Zone)
      setkey(rz.unique,Zone)
      zone.load = rz.unique[zone.load][!Region %in% ignore.regions, ] # ignored zones removed above
      zone.load = zone.load[complete.cases(zone.load),]
      plot.load = zone.load

      x.col = 'Zone'
      facet = facet_wrap(~Region, scales = 'free', ncol=2)
    }
    
    # Create and plot data
    p1 <- gen_stack_plot(r.z.gen[(!Zone %in% ignore.zones && !Region %in% ignore.regions),],
                       plot.load[(!Zone %in% ignore.zones && !Region %in% ignore.regions),],
                       filters = c('Region','Zone'), x_col = x.col)
    p1 <- p1 + facet
  }
} else { print('Section not run according to input file.') }
