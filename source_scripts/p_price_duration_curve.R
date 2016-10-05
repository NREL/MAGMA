# Check if this section was selected to run in the input file
if(price.duration.curve) {

  if ( typeof(interval.region.price)=='character' ) { 
      print('ERROR: interval.region.price not correct. Cannot run this section')
    } else {

      # Create plot
      p1 = price_duration_curve(interval.region.price[!name %in% ignore.regions & property == 'Price', ],
                                filters = c('property','name'), color='area')
      # Create plot with slightly different y-axis limit.
      p2 = p1 + coord_cartesian(ylim=c(0,200))
           
      print(p1)
      print(p2)
  }

} else { print('Section not run according to input file.') }
