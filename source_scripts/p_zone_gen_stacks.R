
if (zone.gen.stacks) {

# Query region and zonal generation
r.z.gen = tryCatch( select(region_zone_gen(yr.data.generator), Region, Zone, Type, GWh = value), error = function(cond) { return('ERROR') } )

# If the query doesn't work, return an error. Else run code to create plot.
if ( typeof(r.z.gen)=='character' ) {
  print('ERROR: region_zone_gen function not returning correct results.')
} else if ( typeof(z.load) == 'character' ) { 
  print('ERROR: zone_load function not returning correct results.')
} else {

  # reorder the levels of Type to plot them in order
  r.z.gen$Type = factor(r.z.gen$Type, levels = gen.order)
    
  # r.z.gen.sum is just used to set the maximum height on the plot, see pretty() fcn below
  r.z.gen.sum = r.z.gen %>% 
    dplyr::summarise(TWh=sum(GWh)/1000) #change GWh generation to TWh
  
  # Remove regions or zones to ignore and convert GWh to TWh
  r.z.gen.plot = r.z.gen %>%
    group_by(Type, Zone) %>%
    dplyr::summarise(TWh = sum(GWh)/1000) %>%
    filter(!Zone %in% ignore.zones)
    
  zone.load = filter(z.load, !name %in% ignore.zones)
  zone.load$value = zone.load$value/1000

  # ***Not needed for these plots as the y axis scaling changes***
  # This automatically creates the y-axis scaling. 
  # py=pretty(r.z.gen.sum$TWh, n=5, min.n = 5)
  # seq.py=seq(0, py[length(py)], 10*(py[2]-py[1]))
  
  # Create plot
  p1 = ggplot() +
     geom_bar(data = r.z.gen.plot, aes(x = Zone, y = TWh, fill=Type, order=as.numeric(Type)), stat="identity", position="stack" ) +
     geom_errorbar(aes(x=name, y=value, ymin=value, ymax=value, color='load'), data=zone.load, linetype='longdash', size=0.45)+
     scale_color_manual(name='', values=c("load"="grey40"), labels=c("Load"))+
     scale_fill_manual(values = gen.color, guide = guide_legend(reverse = TRUE))+
     labs(y="Generation (TWh)", x=NULL)+
     # scale_y_continuous(breaks=seq.py, expand=c(0,0), label=comma)+
     guides(color = guide_legend(order=1), fill = guide_legend(order=2, reverse=TRUE))+
     theme(    legend.key =      element_rect(color="grey80", size = 0.8), 
               legend.key.size = grid::unit(1.0, "lines"),
               legend.text =     element_text(size=text.plot), 
               legend.title =    element_blank(),
       #                         text = element_text(family="Arial"),
               axis.text =       element_text(size=text.plot/1.2), 
               axis.text.x =     element_text(angle=-45, hjust=0),
               axis.title =      element_text(size=text.plot, face=2), 
               axis.title.y =    element_text(vjust=1.2), 
               panel.margin =    unit(1.5, "lines"))
               # aspect.ratio =    2.5)
    # facet_wrap(~Zone, scales = 'free_y')
  print(p1)
}

} else { print('Section not run according to input file.') }