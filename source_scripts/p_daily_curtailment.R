# Check if this section was selected to run in the input file
if (daily.curtailment) {

# Query curtailment data
interval.curt = tryCatch( total_curtailment(interval.generation, interval.avail.cap), error = function(cond) { return('ERROR')})

# If there is a problem with the query return an error.
if ( typeof(interval.curt)=='character' ) { 
  print('ERROR: daily_curtailment function not returning correct results.')
} else {

  # Calculate average curtailment for each day 
  daily.curt = interval.curt[,.(Curtailment = mean(Curtailment)),by=.(day)] 
  daily.curt[, time := seq(first.day, last.day, by = 'day')[1:nrow(daily.curt)]] # Add time column

  # this is just for scaling the y-axis (either by load or generation, whichever is bigger)
  stack = daily.curt[, .(value=sum(Curtailment)),by=.(time)] 
  stack[, Type := "ALL"]
                  
  # This automatically creates the y-axis scaling
  seq.py = pretty(c(0,stack$value), n=5, min.n=3 )

  # Create plot
  p1 = ggplot(daily.curt)+
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
