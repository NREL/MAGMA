# Query interval generation by type data
int.gen = tryCatch( interval_gen(), error = function(cond) { return('ERROR') } )

# If the query doesn't work, return an error. Else create plot.
if ( typeof(int.gen)=='character' ) { 
  print('ERROR: interval_gen function not returning correct results.')
} else {
  
  # Previous version of script used this up here. No longer needed.
  # # Rearrange factor levels for plotting.
  # int.gen$Type = factor(int.gen$Type, levels = c(gen.order, 'Load')) 
  
  for ( i in 1:n.periods ) {
    key.period.time = seq(start.end.times[i,'start'], start.end.times[i,'end'], 
                          by = filter(int.gen, Region == int.gen$Region[1])[2,'time']-filter(int.gen, Region == int.gen$Region[1])[1,'time'])
    key.period.gen = filter(int.gen, time %in% key.period.time)
    key.period.gen$Period = period.names[i]
    
    if ( i == 1 ) {
      int.gen.key.periods = key.period.gen
    } else {
      int.gen.key.periods = rbind(int.gen.key.periods, key.period.gen)
    }
  }

  # Sum generation by type, and rearrange data for plotting.
  key.period.gen = int.gen.key.periods %>%
    # Previous version had already melted data frame which had to be casted before melting. No longer needed.
    # dcast(time+Period+Region+Zone~Type, value.var = 'value', fun.aggregate = sum) %>% 
    melt(id.vars = c('time', 'Period', 'Region', 'Zone'), variable.name = 'Type', value.name = 'value')
  
  # Rearrange factor levels for plotting.
  key.period.gen$Type = factor(key.period.gen$Type, levels = c(gen.order, 'Load')) 
  
  gen.type = subset(key.period.gen, Type != 'Load')
  gen.type$value[gen.type$value<0]=0
  gen.type$Period = ordered(gen.type$Period, levels = period.names)
  
  gen.load = subset(key.period.gen, Type %in% 'Load')

  # ###############################################################################
  # Region Data
  # ###############################################################################  
    
  gen.type.region = gen.type %>%
    group_by(time, Region, Type, Period) %>%
    summarise(value = sum(value))
  
  gen.load.region = gen.load %>%
    group_by(time, Region, Type, Period) %>%
    summarise(value = sum(value))    
  
  # ###############################################################################
  # Zone Data
  # ###############################################################################   
  
  gen.type.zone = gen.type %>%
    group_by(time, Zone, Type, Period) %>%
    summarise(value = sum(value))
  
  gen.load.zone = gen.load %>%
    group_by(time, Zone, Type, Period) %>%
    summarise(value = sum(value))    
  
  # ###############################################################################
  # Total database Data
  # ###############################################################################   
  
  gen.type.total = gen.type %>%
    group_by(time, Type, Period) %>%
    summarise(value = sum(value))    
  
  gen.load.total = gen.load %>%
    group_by(time, Type, Period) %>%
    summarise(value = sum(value))    

}