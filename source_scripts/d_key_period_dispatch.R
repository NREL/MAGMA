# Query interval generation by type data
int.gen = tryCatch( interval_gen(), error = function(cond) { return('ERROR') } )

# If the query doesn't work, return an error. Else create plot.
if ( typeof(int.gen)=='character' ) { 
  print('ERROR: interval_gen function not returning correct results.')
} else {
  
  # Rearrange factor levels for plotting.
  int.gen$Type = factor(int.gen$Type, levels = c(gen.order, 'Load')) 
  
  for ( i in 1:n.periods ) {
    key.period.time = seq(start.end.times[i,'start'], start.end.times[i,'end'], by = (int.gen[2,'time']-int.gen[1,'time']))
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
    dcast(time+Period~Type, value.var = 'value', fun.aggregate = sum) %>%
    melt(id.vars = c('time', 'Period'), variable.name = 'Type', value.name = 'value')
  
  gen.type = subset(key.period.gen, Type != 'Load')
  gen.type$value[gen.type$value<0]=0
  gen.type$Period = ordered(gen.type$Period, levels = period.names)
  
  gen.load = subset(key.period.gen, Type %in% 'Load')
  
  # this is just for scaling the y-axis (either by load or generation, whichever is bigger)
  stack = gen.type %>% 
    group_by(time) %>%
    summarise(value = sum(value))
  stack$Type = "ALL"
  
  if (max(gen.load$value)>max(stack$value)){
    stack = gen.load %>%
      group_by(time) %>%
      summarise(value=sum(value))
  }
  
  # This automatically creates the y-axis scaling
  py  =pretty(stack$value/1000, n = 4)
  seq.py = seq(0, py[length(py)], 2*(py[2]-py[1])) # get whole breaks sequence
  
}