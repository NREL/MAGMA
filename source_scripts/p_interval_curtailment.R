# Check if this section was selected to run in the input file
if (interval.curtailment){
  
  # If the data doesn't exist, run the query function. 
  if ( !exists('interval.curt') ) {
    # Query curtailment data
    interval.curt = tryCatch( total_curtailment(interval.generation, interval.avail.cap), error = function(cond) { return('ERROR')})
  }
  
  # Check for errors in the querying function.
  if ( typeof(interval.curt)=='character' ) { 
    print('ERROR: daily_curtailment function not returning correct results.')
  } else {
    # Sum up the curtailment each interval to get average interval curtailment. Assign an interval to each row.
    avg.curt = interval.curt[,.(Curtailment=mean(Curtailment)/1000),by=.(interval)]
    avg.curt[, hour := floor((interval-1)*(3600*24/intervals.per.day)/3600)]
    avg.curt[, minute := floor(((interval-1)*(3600*24/intervals.per.day)/3600-hour)/60)]
    avg.curt[, second := floor((((interval-1)*(3600*24/intervals.per.day)/3600-hour)/60-minute)/60)]
    avg.curt[, time := as.POSIXct(strptime(paste(hour,minute,second, sep=":"), "%H:%M:%S"),'UTC')]
    }
    p1 = line_plot(avg.curt, filters='time', x.col='time', y.col='Curtailment', y.lab='Curtailment (GWh)')
    p1 = p1 + scale_x_datetime(breaks = date_breaks(width = "2 hour"), 
                               labels = date_format("%H:%M"), expand = c(0, 0))
    print(p1)
} else { print('Section not run according to input file.') }
