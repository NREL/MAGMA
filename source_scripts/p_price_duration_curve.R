
if(price.duration.curve) {

region.data = int.data.region

# If there is a problem with the query return an error, else create the plots.
if ( typeof(region.data)=='character' ) { 
  print('ERROR: int_region_query function not returning correct results.')
} else {

  region.data = filter(region.data, property == 'Price')
  
  for ( i in 1:length(unique(region.data$name)) ) {
    r.name = unique(region.data$name)[i]
    d.curve.data = filter(region.data, name==r.name)
    d.curve.data = d.curve.data[order(d.curve.data$value),]
    d.curve.data$interval = ( seq(1:nrow(d.curve.data)) - nrow(d.curve.data) )*-1
    d.curve.data$area = r.name

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
  
  # Create plot
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
print(p.1, p.2)
}

} else { print('Section not run according to input file.') }
