
if (region.gen.stacks) {

if( !exists('r.z.gen') ) {
  # Query region and zonal generation
  r.z.gen = tryCatch( select(region_zone_gen(yr.data.generator), Region, Zone, Type, GWh = value), error = function(cond) { return('ERROR') } )
}

# Check if zonal.gen query worked and create plot of regional gen, else return an error.
if ( typeof(r.z.gen)=='character' ) {
  print('ERROR: region_zone_gen function not returning correct results.')
} else if ( typeof(r.load) == 'character' ) {
  print('ERROR: region_load function not returning correct results.')
} else {
  
  # reorder the levels of Type to plot them in order
  r.z.gen$Type = factor(r.z.gen$Type, levels = gen.order)
    
  # r.z.gen.sum is just used to set the maximum height on the plot, see pretty() fcn below
  r.z.gen.sum = r.z.gen %>% 
    dplyr::summarise(TWh=sum(GWh)/1000) #change GWh generation to TWh
  
  # Convert GWh to TWh
  r.z.gen.plot = r.z.gen %>%
    group_by(Type, Region, Zone) %>%
    dplyr::summarise(TWh = sum(GWh)/1000) %>%
    filter(!Zone %in% ignore.zones) %>%
    filter(!Region %in% ignore.regions)
    
  region.load = filter(r.load, !name %in% ignore.regions)
  colnames(region.load)[which(colnames(region.load)=='name')]='Region'
  region.load$value = region.load$value/1000
  
  region.load = region.load %>%
    join(region.zone.mapping[,c('Zone', 'Region')], by = 'Region', type='left', match='first') %>%
    filter(!Zone %in% ignore.zones) %>%
    filter(!Region %in% ignore.regions)
  region.load = region.load[complete.cases(region.load),]
  
  # *** Not needed for these plots as y-axis is varying. ***
  # # This automatically creates the y-axis scaling
  # py=pretty(r.z.gen.sum$TWh, n=5, min.n = 5)
  # seq.py=seq(0, py[length(py)], 10*(py[2]-py[1]))
  
  # Create plot
  p1 = ggplot() +
    geom_bar(data = r.z.gen.plot, aes(x = Region, y = TWh, fill=Type, order=as.numeric(Type)), stat='identity', position="stack" ) +
    geom_errorbar(aes(x=Region, y=value, ymin=value, ymax=value, color='load'), data=region.load, linetype='longdash', size=0.45)+
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
                   panel.margin =    unit(1.5, "lines"))+
        facet_wrap(~Zone, scales = 'free', ncol=2)
                   # , nrow=length(unique(r.z.gen.plot$Zone)))
  print(p1)
}

} else { print('Section not run according to input file.') }