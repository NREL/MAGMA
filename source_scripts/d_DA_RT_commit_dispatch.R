
# Call query functions to get committed capacity (day ahead) and available capacity in the real time.
committed.cap = tryCatch( cap_committed(interval.da.committment), error = function(cond) { return('ERROR')})
avail.cap.rt = tryCatch( cap_committed(interval.avail.cap), error = function(cond) { return('ERROR')})

# Check to see if interval generation data already exists. If it doesn't call that query function.
if( !exists('int.gen') ) {
  int.gen = tryCatch( interval_generation(interval.region.load, interval.zone.load, interval.generation, interval.avail.cap), error = function(cond) { return('ERROR') } )
}

# If there is a problem with the query return an error, else create the plots.
if ( typeof(committed.cap)=='character' | typeof(int.gen)=='character' | typeof(avail.cap.rt)=='character' ) { 
  print('ERROR: daily_curtailment or cap_committed function not returning correct results.')
} else {
  
  # Remove unneccessary data columns and rearrange the data for plotting. This is interval generation data in the real time.
  da.rt.data = int.gen %>%
    select(-Curtailment, -Load) %>%
    melt(id.vars = .(time, Region, Zone), variable.name = 'Type', value.name='RT.gen')
  
  names(avail.cap.rt)[names(avail.cap.rt)=='committed.cap']='RT.cap'
  names(committed.cap)[names(committed.cap)=='committed.cap']='DA.cap'
  
  # Add the day ahead capacity and real time capacity to the real time generation.
  da.rt.data = da.rt.data %>%
    merge(avail.cap.rt, by=c('time', 'Region', 'Zone', 'Type') ) %>%
    merge(committed.cap, by=c('time', 'Region', 'Zone', 'Type') ) 
  
  # Only pull out data for the time spans that were requested in the input file. 
  for ( i in 1:n.periods ) {
    key.period.time = seq(start.end.times[i,'start'], start.end.times[i,'end'], 
                          by = min(da.rt.data$time[da.rt.data$time>min(da.rt.data$time)]) - min(da.rt.data$time))
    plot.data = filter(da.rt.data, time %in% key.period.time)
    plot.data$Period = period.names[i]
    
    if ( i == 1 ) {
      plot.data.all = plot.data
    } else {
      plot.data.all = rbind(plot.data.all, plot.data)
    }
  }  

}
