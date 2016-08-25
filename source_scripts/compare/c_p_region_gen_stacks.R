
if (region.gen.stacks & length(db.loc)>1) {

if( !exists('r.gen.scen') ) {
  # Query region and zonal generation
  r.gen.scen = tryCatch( region_gen_diff(total.generation, total.avail.cap), error = function(cond) { return('ERROR') } )
}

# Check if zonal.gen query worked and create plot of regional gen, else return an error.
if ( typeof(r.gen.scen)=='character' ) {
  print('ERROR: region_gen_diff function not returning correct results.')
} else if ( typeof(r.load) == 'character' ) {
  print('ERROR: region_load function not returning correct results.')
} else {

  # Call function to create plot and print
  [p1, seq.py] = gen_diff_stack_plot(r.gen.scen[!Region %in% ignore.regions, ], 
                                     r.load[!Region %in% ignore.regions, ],
                                     filters = 'Region')
  print(p1 + facet_wrap(~Region, scales = 'free', ncol=3))
}

} else { print('Section not run according to input file.') }
