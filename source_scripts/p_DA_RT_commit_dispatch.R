if (commit.dispatch) {

# Query curtailment data
committed.cap = tryCatch( cap_committed(int.data.commit), error = function(cond) { return('ERROR')})

if( !exists('int.gen') ) {
  int.gen = tryCatch( interval_gen(int.data.region, int.data.zone, int.data.gen, int.data.avail.cap), error = function(cond) { return('ERROR') } )
}

# If there is a problem with the query return an error, else create the plots.
if ( typeof(committed.cap)=='character' | typeof(int.gen)=='character' ) { 
  print('ERROR: daily_curtailment or cap_committed function not returning correct results.')
} else {

  
  
  
  
  
  
  
  
  dy.curt = data.frame(colMeans(daily.curt) ) # Calculate mean of each hour of the year
  dy.curt['time'] = seq(first.day, last.day, by = 'day') # Add time column
  colnames(dy.curt) = c('Curtailment', 'time')

  # this is just for scaling the y-axis (either by load or generation, whichever is bigger)
  stack = dy.curt %>% 
    group_by(time) %>%
    summarise(value = sum(Curtailment))
  stack$Type = "ALL"
                  
  # This automatically creates the y-axis scaling
  py  =pretty(stack$value)
  seq.py = seq(0, py[length(py)], 2*(py[2]-py[1])) # get whole breaks sequence

  # Create plot
  p1 = ggplot(dy.curt)+
         geom_bar(aes(x=time, y=Curtailment, color="black"), stat='identity', color=NA)+    
         labs(y="Curtailment (MWh)", x='Date')+
         scale_x_datetime(breaks   = date_breaks(width = "1 month"), labels = date_format("%b"), expand = c(0, 0))+
         scale_y_continuous(breaks = seq.py, limits=c(0, max(py)), expand=c(0,0))+
         theme( legend.key =       element_rect(color = "grey80", size = 0.4),
                legend.key.size =  grid::unit(0.9, "lines"), 
                legend.text =      element_text(size=text.plot/1.1),
                strip.text =       element_text(size=rel(0.7)),
                axis.text =        element_text(size=text.plot/1.2), 
                axis.title =       element_text(size=text.plot, face=2), 
                axis.title.x =     element_text(vjust=-0.3),
                panel.grid.major = element_line(colour = "grey85"),
                panel.grid.minor = element_line(colour = "grey93"),
#               aspect.ratio =     0.5,
                panel.margin =     unit(1.0, "lines") )
  print(p1)
}

} else { print('Section not run according to input file.') }
