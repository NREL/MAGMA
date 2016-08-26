# Check if this section was selected run in the input file
if (region.gen.stacks) {

if( !exists('r.z.gen') ) {
  # Query region and zonal generation
  r.z.gen = tryCatch( select(region_zone_gen(total.generation, total.avail.cap), Region, Zone, Type, GWh = value), error = function(cond) { return('ERROR') } )
}

# Check if zonal.gen query worked and create plot of regional gen, else return an error.
if ( typeof(r.z.gen)=='character' ) {
  print('ERROR: region_zone_gen function not returning correct results.')
} else if ( typeof(r.load) == 'character' ) {
  print('ERROR: region_load function not returning correct results.')
} else {
  
  # reorder the levels of Type to plot them in order
  r.z.gen[, Type := factor(Type, levels = gen.order)]
    
  # r.z.gen.sum is just used to set the maximum height on the plot, see pretty() fcn below
  r.z.gen.sum = r.z.gen[, .(TWh = sum(GWh)/1000)]  #change GWh generation to TWh
  
  # Convert GWh to TWh and remove region and zone data that should be ignored
  r.z.gen.plot = r.z.gen[(!Zone %in% ignore.zones & !Region %in% ignore.regions), 
                         .(TWh = sum(GWh)/1000), by=.(Type, Region, Zone)]
    
  if (length(unique(r.z.gen.plot$Region))>length(unique(r.z.gen.plot$Zone))) {
    region.load = r.load[!name %in% ignore.regions, ] # Remove load data from regions being ignored
    setnames(region.load, 'name', 'Region')
    region.load[, value := value/1000]
    
    setkey(region.load,Region)
    setkey(rz.unique,Region)
    region.load = rz.unique[region.load][!Zone %in% ignore.zones, ] # ignored regions removed above
    region.load = region.load[complete.cases(region.load),]
    plot.load = region.load

    x.col = 'Region'
    facet = facet_wrap(~Zone, scales = 'free', ncol=2)
  } else{
    zone.load = z.load[!name %in% ignore.zones, ] # Remove load data from regions being ignored
    setnames(zone.load, 'name', 'Zone')
    zone.load[, value := value/1000]

    setkey(zone.load,Zone)
    setkey(rz.unique,Zone)
    zone.load = rz.unique[zone.load][!Region %in% ignore.regions, ] # ignored zones removed above
    zone.load = zone.load[complete.cases(zone.load),]
    plot.load = zone.load

    x.col = 'Zone'
    facet = facet_wrap(~Region, scales = 'free', ncol=2)
  }

  # *** Not needed for these plots as y-axis is varying. ***
  # # This automatically creates the y-axis scaling
  # py=pretty(r.z.gen.sum$TWh, n=5, min.n = 5)
  # seq.py=seq(0, py[length(py)], 10*(py[2]-py[1]))
  
  setorder(r.z.gen.plot,Type)
  # Create plot
  p1 = ggplot() +
    geom_bar(data = r.z.gen.plot, aes_string(x = x.col, y = 'TWh', fill='Type'), stat='identity', position="stack" ) +
    geom_errorbar(aes_string(x=x.col, y='value', ymin='value', ymax='value'), data=plot.load, linetype='longdash', size=0.45)+
    scale_fill_manual(values = gen.color, guide = guide_legend(reverse = TRUE))+
    scale_color_manual(name='', values=c("load"="grey40"), labels=c("Load"))+
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
  print(p1 + facet)
}

} else { print('Section not run according to input file.') }
