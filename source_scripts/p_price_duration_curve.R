# Check if this section was selected to run in the input file
if(price.duration.curve) {

# If price duration curve is selected in the input file, int.data.region should be created.
region.data = interval.region.price[!name %in% ignore.regions, ]

# If there is a problem with the query return an error, else create the plots.
if ( typeof(region.data)=='character' ) { 
  print('ERROR: interval_region_price function not returning correct results.')
} else {
  
    # Create plot
p1 = price_duration_curve(interval.region.price[!name %in% ignore.regions & property == 'Price', ])
  
  # Create plot with slightly different y-axis limit.
p2 = p1 + ylim(c(0,200))
         
print(p1)
print(p2)
}

} else { print('Section not run according to input file.') }
