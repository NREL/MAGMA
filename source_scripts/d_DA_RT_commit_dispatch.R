
# Query curtailment data
committed.cap = tryCatch( cap_committed(int.data.commit), error = function(cond) { return('ERROR')})
avail.cap.rt = tryCatch( cap_committed(int.data.avail.cap), error = function(cond) { return('ERROR')})

if( !exists('int.gen') ) {
  int.gen = tryCatch( interval_gen(int.data.region, int.data.zone, int.data.gen, int.data.avail.cap), error = function(cond) { return('ERROR') } )
}

# If there is a problem with the query return an error, else create the plots.
if ( typeof(committed.cap)=='character' | typeof(int.gen)=='character' | typeof(avail.cap.rt)=='character' ) { 
  print('ERROR: daily_curtailment or cap_committed function not returning correct results.')
} else {
  
  da.rt.data = int.gen %>%
    select(-Price, -Curtailment, -Load) %>%
    melt(id.vars = .(time, Region, Zone), variable.name = 'Type', value.name='RT.gen')
  
  names(avail.cap.rt)[names(avail.cap.rt)=='committed.cap']='RT.cap'
  names(committed.cap)[names(committed.cap)=='committed.cap']='DA.cap'
  
  da.rt.data = da.rt.data %>%
    merge(avail.cap.rt, by=c('time', 'Region', 'Zone', 'Type') ) %>%
    merge(committed.cap, by=c('time', 'Region', 'Zone', 'Type') ) 
  
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
