# Check if this section was selected to run in the input file
if(reserves.plots) {

# Reserves interval data query
r = tryCatch( interval_reserves(int.data.reserve), error = function(cond) { return('ERROR') })

if ( typeof(r)=='character' ) { 
  print('ERROR: interval_reserves function not returning correct results.')
} else {

  # Summing reserves types, and adding indexing for interval number and day.
  r = data.frame( time = r['time'], provision = rowSums(r[2:ncol(r)]))
  r['interval'] = rep(1:intervals.per.day, times = length(r[,1])/intervals.per.day)
  r['day'] = rep(1:(length(r[,1])/intervals.per.day), each = intervals.per.day)
  
  # Average reserve provision at each interval
  int.avg = ddply(r, 'interval', summarise, Provision = mean(provision))
  
  # this is just for scaling the y-axis (either by load or generation, whichever is bigger)
  stack = int.avg %>% 
    group_by(interval) %>%
    summarise(value = sum(Provision))
  stack$Type = "ALL"
              
  # This automatically creates the y-axis scaling
  py  =pretty(stack$value/1000)
  seq.py = seq(0, py[length(py)], 2*(py[2]-py[1])) # get whole breaks sequence
  
  # Creating interval reserves provisions plot
  p.1 = ggplot(int.avg)+
           geom_line(aes(x=interval, y=Provision/1000), size = 2, color = 'black')+    
           labs(y="Reserve Provisions (GWh)", x='Interval')+
  #                     scale_x_datetime(breaks = date_breaks(width = "1 month"), labels = date_format("%b"), expand = c(0, 0))+
           scale_y_continuous(breaks=seq.py, limits=c(0, max(py)), expand=c(0,0))+
           theme( legend.key =       element_rect(color = "grey80", size = 0.4),
                  legend.key.size =  grid::unit(0.9, "lines"), 
                  legend.text =      element_text(size=text.plot/1.1),
                  strip.text =       element_text(size=rel(0.7)),
                  axis.text =        element_text(size=text.plot/1.2), 
                  axis.title =       element_text(size=text.plot, face=2), 
                  axis.title.x =     element_text(vjust=-0.3),
                  panel.grid.major = element_line(colour = "grey85"),
                  panel.grid.minor = element_line(colour = "grey93"),
  #                 aspect.ratio =     0.5,
                  panel.margin =     unit(1.0, "lines") )

  # Calculating the daily hourly average
  dy.avg = ddply(r, 'day', summarise, Provision = mean(provision))

  # this is just for scaling the y-axis (either by load or generation, whichever is bigger)
  stack = dy.avg %>% 
    group_by(day) %>%
    summarise(value = sum(Provision))
  stack$Type = "ALL"
                  
  # This automatically creates the y-axis scaling
  py  =pretty(stack$value/1000)
  seq.py = seq(0, py[length(py)], 2*(py[2]-py[1])) # get whole breaks sequence

  # Creating average daily reserves provisions plot
  p.2 = ggplot(dy.avg)+
           geom_line(aes(x=day, y=Provision/1000), color='black', size = 2)+    
           labs(y="Reserve Provisions (GWh)", x='Day')+
  #                     scale_x_datetime(breaks = date_breaks(width = "1 month"), labels = date_format("%b"), expand = c(0, 0))+
           scale_y_continuous(breaks=seq.py, limits=c(0, max(py)), expand=c(0,0))+
           theme( legend.key =       element_rect(color = "grey80", size = 0.4),
                  legend.key.size =  grid::unit(0.9, "lines"), 
                  legend.text =      element_text(size=text.plot/1.1),
                  strip.text =       element_text(size=rel(0.7)),
                  axis.text =        element_text(size=text.plot/1.2), 
                  axis.title =       element_text(size=text.plot, face=2), 
                  axis.title.x =     element_text(vjust=-0.3),
                  panel.grid.major = element_line(colour = "grey85"),
                  panel.grid.minor = element_line(colour = "grey93"),
  #                 aspect.ratio =     0.5,
                  panel.margin =     unit(1.0, "lines") )
  print(p.1)
  print(p.2)
}

} else { print('Section not run according to input file.') }
