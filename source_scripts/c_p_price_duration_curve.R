# Check if this section was selected to run in the input file
if(price.duration.curve) {

# If price duration curve is selected in the input file, int.data.region should be created.
region.data = interval.region.price[!name %in% ignore.regions, ]

# If there is a problem with the query return an error, else create the plots.
if ( typeof(region.data)=='character' ) { 
  print('ERROR: interval_region_price function not returning correct results.')
} else {

  # Pull out price from the regional data query
  region.data = region.data[property == 'Price', .(scenario,name,time,value) ]
  
  # Separate price for each region and create a duration curve for each region. 
  region.data[, interval := rank(-value,ties.method="random"), by=.(scenario,name)]
  setnames(region.data,'name','area')
  
    # Create plot
p.1 = ggplot(region.data)+
         geom_line(aes(x=interval, y=value, color=scenario), size=0.8)+  
         labs(y="Price ($/MWh)", x='Hours of Year')+
         scale_color_brewer(palette="Set1")+
         facet_wrap(~area, ncol=2)+
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
p.2 = ggplot(region.data)+
         geom_line(aes(x=interval, y=value, color=scenario), size=0.8)+  
         ylim(c(0,200))+
         labs(y="Price ($/MWh)", x='Hours of Year')+
         scale_color_brewer(palette="Set1")+
         facet_wrap(~area, ncol=2)+
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
