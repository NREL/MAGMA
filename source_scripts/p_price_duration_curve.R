# Check if this section was selected to run in the input file
if(price.duration.curve) {

# If price duration curve is selected in the input file, int.data.region should be created.
region.data = int.data.region

# If there is a problem with the query return an error, else create the plots.
if ( typeof(region.data)=='character' ) { 
  print('ERROR: int_region_query function not returning correct results.')
} else {

  # Pull out price from the regional data query
  region.data = filter(region.data, property == 'Price')
  
  # Separate price for each region and create a duration curve for each region. 
  for ( i in 1:length(unique(region.data$name)) ) {
    r.name = unique(region.data$name)[i]
    d.curve.data = filter(region.data, name==r.name)
    d.curve.data = d.curve.data[order(d.curve.data$value),]
    d.curve.data$interval = ( seq(1:nrow(d.curve.data)) - nrow(d.curve.data) )*-1
    d.curve.data$area = r.name

    # Combine the duration curves for each region as the loop runs.
    if ( i == 1 ) {
      plot.data = d.curve.data
    } else {
      plot.data = rbind(plot.data, d.curve.data)
    }
  }
  
    # Create plot
p.1 = ggplot(plot.data)+
         geom_line(aes(x=interval, y=value, color=area), size=0.8)+  
         labs(y="Price ($/MWh)", x='Hours of Year')+
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
  
  # Create plot with slightly different y-axis limit.
p.2 = ggplot(plot.data)+
         geom_line(aes(x=interval, y=value, color=area), size=0.8)+  
         ylim(c(0,200))+
         labs(y="Price ($/MWh)", x='Hours of Year')+
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
print(p.1)
print(p.2)
}

} else { print('Section not run according to input file.') }
