# Check to make sure relevant data exists
if ( typeof(interval.region.load)=='character' ) {
  print('INPUT ERROR: interval.region.load has errors. Cannot run this section.')
  #  } else if ( typeof(interval.zone.load) == 'character' ) { 
  #    print('INPUT ERROR: interval.zone.load has errors. Cannot run this section.')
} else if ( typeof(interval.generation) == 'character' ) { 
  print('INPUT ERROR: interval.generation has errors. Cannot run this section.')
} else if ( typeof(interval.avail.cap) == 'character' ) { 
  print('INPUT ERROR: interval.avail.cap has errors. Cannot run this section.')
} else{
  # Query interval generation by type from interval data
  int.gen.region = tryCatch( interval_region_generation(interval.region.load, interval.region.ue, interval.generation, interval.avail.cap), 
                             error = function(cond) { return('ERROR') } )
  
  int.gen.zone = tryCatch( interval_zone_generation(interval.zone.load, interval.zone.ue, interval.generation, interval.avail.cap), 
                           error = function(cond) { return('ERROR') } )
  
  # ###############################################################################
  # Total database and region data.
  # ###############################################################################
  # If the query doesn't work, return an error. 
  if ( typeof(int.gen.region)=='character' ) { 
    key.period.gen.region = 'ERROR: interval_region_generation function not returning correct results.'
    print('ERROR: interval_region_generation function not returning correct results.')
  } else {
    
    # From the full year of data, pull out only the data corresponding to the key periods specified in the input file. 
    timediff = int.gen.region[,.(timediff=diff(time)),by=.(scenario,Region,Type)][,.(min(timediff))][,V1]
    for ( i in 1:n.periods ) {
      key.period.time = seq(start.end.times[i,start], start.end.times[i,end], 
                            by = timediff)
      key.period.gen.region = int.gen.region[int.gen.region$time %in% key.period.time]
      key.period.gen.region[, Period := period.names[i]]
      
      if ( i == 1 ) {
        int.gen.key.periods = key.period.gen.region
      } else {
        int.gen.key.periods = rbindlist(list(int.gen.key.periods, key.period.gen.region))
      }
    }
    
    # Rearrange data for plotting
    key.period.gen.region = int.gen.key.periods 
    
    # Rearrange factor levels for plotting.
    key.period.gen.region[, Type := factor(Type, levels = c(gen.order, 'Load','Served Load'))]
    
    # Pull out just generation data
    gen.type = key.period.gen.region[! Type %in% c('Load','Served Load'), ]
    gen.type[value<0, value:=0]
    gen.type[, Period := ordered(Period, levels = period.names)]
    
    # Pull out just load data
    gen.load = key.period.gen.region[Type %in% c('Load','Served Load'), ]
    
    # ###############################################################################
    # Region Data
    # ###############################################################################  
    
    gen.type.region = gen.type[,.(value=sum(value,na.rm=TRUE)),by=.(time,scenario,Region,Type,Period)]
    gen.load.region = gen.load[,.(value=sum(value,na.rm=TRUE)),by=.(time,scenario,Region,Type,Period)] 
    setorder(gen.type.region, Type)
    setorder(gen.load.region, Type)
    
    # ###############################################################################
    # Total database Data
    # ###############################################################################   
    
    gen.type.total = gen.type[,.(value=sum(value,na.rm=TRUE)),by=.(time,scenario,Type,Period)]
    gen.load.total = gen.load[,.(value=sum(value,na.rm=TRUE)),by=.(time,scenario,Type,Period)]
    setorder(gen.type.total, Type)
    setorder(gen.load.total, Type)
    
  }
  
  # ###############################################################################
  # Zone data
  # ###############################################################################
  # If the query doesn't work, return an error. 
  if ( typeof(int.gen.zone)=='character' ) { 
    key.period.gen.zone = 'ERROR: interval_zone_generation function not returning correct results.'
    print('ERROR: interval_zone_generation function not returning correct results.')
  } else {
    
    # From the full year of data, pull out only the data corresponding to the key periods specified in the input file. 
    timediff = int.gen.zone[,.(timediff=diff(time)),by=.(scenario,Type,Zone)][,.(min(timediff))][,V1]
    for ( i in 1:n.periods ) {
      key.period.time = seq(start.end.times[i,start], start.end.times[i,end], 
                            by = timediff)
      key.period.gen.zone = int.gen.zone[int.gen.zone$time %in% key.period.time]
      key.period.gen.zone[, Period := period.names[i]]
      
      if ( i == 1 ) {
        int.gen.key.periods = key.period.gen.zone
      } else {
        int.gen.key.periods = rbindlist(list(int.gen.key.periods, key.period.gen.zone))
      }
    }
    
    # Rearrange data for plotting
    key.period.gen.zone = int.gen.key.periods 
    
    # Rearrange factor levels for plotting.
    key.period.gen.zone[, Type := factor(Type, levels = c(gen.order, 'Load','Served Load'))]
    
    # Pull out just generation data
    gen.type = key.period.gen.zone[! Type %in% c('Load','Served Load'), ]
    gen.type[value<0, value:=0]
    gen.type[, Period := ordered(Period, levels = period.names)]
    
    # Pull out just load data
    gen.load = key.period.gen.zone[Type %in% c('Load','Served Load'), ]
    
    # ###############################################################################
    # Zone Data
    # ###############################################################################   
    
    gen.type.zone = gen.type[,.(value=sum(value,na.rm=TRUE)),by=.(time,scenario,Zone,Type,Period)]
    gen.load.zone = gen.load[,.(value=sum(value,na.rm=TRUE)),by=.(time,scenario,Zone,Type,Period)]  
    setorder(gen.type.zone, Type)
    setorder(gen.load.zone, Type)
    
  }
}
