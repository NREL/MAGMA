# Check if this section was selected to run in the input file
if (daily.curtailment) {
  
  # If the data doesn't exist, run the query function. 
  if ( !exists('interval.curt') ) {
    # Query curtailment data
    interval.curt = tryCatch( total_curtailment(interval.generation, interval.avail.cap), error = function(cond) { return('ERROR')})
  }
  # If there is a problem with the query return an error.
  if ( typeof(interval.curt)=='character' ) { 
    print('ERROR: daily_curtailment function not returning correct results.')
  } else {
    
    # Calculate average curtailment for each day 
    daily.curt = interval.curt[,.(Curtailment = mean(Curtailment)),by=.(scenario,day,year)] 
    daily.curt[, timeformat := sprintf("%d %d", day+1, year)]
    daily.curt[, time := as.POSIXct(strptime(timeformat,'%j %Y'))] # Add time column
    
    p1 = line_plot(daily.curt, filters=c('scenario','time'), x.col='time', 
                   y.col='Curtailment', y.lab='Curtailment (MWh)', color='scenario')
    p1 = p1 + scale_x_datetime(breaks = date_breaks(width = "1 month"), 
                               labels = date_format("%b %d"), expand = c(0, 0))
    print(p1)
    
    # Calculate diffs
    daily.curt[, scenario:=as.character(scenario)]
    diff.daily.curt = daily.curt[, .(scenario, Curtailment = Curtailment - Curtailment[scenario==ref.scenario]), by=.(time)]
    
    p2 = line_plot(diff.daily.curt, filters=c('scenario','time'), x.col='time',
                   y.col='Curtailment', y.lab='Curtailment (MWh)', color='scenario')
    print(p2)
  }
  
} else { print('Section not run according to input file.') }
