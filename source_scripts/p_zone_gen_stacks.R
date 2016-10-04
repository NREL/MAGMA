if (zone.gen.stacks) {
  if ( typeof(total.generation)=='character' ) {
    print('INPUT ERROR: total.generation has errors. Cannot run this section.')
  } else if ( typeof(total.avail.cap) == 'character' ) { 
    print('INPUT ERROR: total.avail.cap has errors. Cannot run this section.')
  } else{ 
    # Query region and zonal generation
    r.z.gen = tryCatch( region_zone_gen(total.generation, total.avail.cap), 
                        error = function(cond) { return('ERROR') } )  
    # Create plot
    plot.data = gen_stack_plot(r.z.gen[!Zone %in% ignore.zones, ],
                               z.load[!Zone %in% ignore.zones, ], 
                               filters = 'Zone')
    print(plot.data[[1]] + facet_wrap(~Zone, scales = 'free', ncol=3) +
          theme(aspect.ratio = 2.5))
  }
} else { print('Section not run according to input file.') }