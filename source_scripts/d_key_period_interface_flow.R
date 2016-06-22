
if (!exists('interface.flows')) {
  # Call the query function to get interface flows for the interfaces selected in the query function.
  interface.flows = tryCatch( interval_interface_flows(int.data.interface), error=function(cond) {return('ERROR: interface_flows query not returning correct results.')})
}

# Check for errors in the query function. If theres an error don't continue.
if ( typeof(interface.flows)=='character' ) { 
  key.period.interface.flow = 'ERROR: interface_flows function not returning correct results.'
  plot.flows = 'ERROR: interface_flows function not returning correct results.'
  print('ERROR: interface_flows function not returning correct results.')
} else {

  # From the full set of data, pull out only the data corresponding to the key period specified in the input file. 
  for ( i in 1:n.periods ) {
    key.period.time = seq(start.end.times[i,'start'], start.end.times[i,'end'], 
                          by = min(interface.flows$time[interface.flows$time>min(interface.flows$time)]) - min(interface.flows$time))
    key.period.flow = filter(interface.flows, time %in% key.period.time)
    key.period.flow$Period = period.names[i]
    
    if ( i == 1 ) {
      interface.flows.key.periods = key.period.flow
    } else {
      interface.flows.key.periods = rbind(interface.flows.key.periods, key.period.flow)
    }
  }
  plot.flows = interface.flows.key.periods
}