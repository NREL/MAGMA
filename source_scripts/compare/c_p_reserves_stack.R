# Check if this section was selected run in the input file
if (reserve.stack){

# Query annual reserves provision by type.
  yr.res.scen = tryCatch( annual_reserves_provision(total.gen.res), error = function(cond) { return('ERROR: annual_reserves function not returning correct results.') })

# If the query doesn't work, print an error. Else continue.
if ( typeof(yr.res.scen)=='character' ) {
  print('ERROR: annual_reserves_provision function not returning correct results.')
} else {

  # res.sum is just used to set the maximum height on the plot, see pretty() fcn below
  res.sum = yr.res.scen[, .(TWh = sum(GWh)/1000), by=.(scenario, Reserve)]  #change GWh generation to TWh

  # reorder the levels of Type to plot them in order
  yr.res.scen[, Type := factor(Type, levels = c(gen.order))]
  if(any(is.na(yr.res.scen$Type))) print("ERROR:gen.order doesn't contain all of the res types: FIX YOUR INPUT DATA CSV")

  # Group by type and convert GWh to TWh
  res.plot = yr.res.scen[, .(TWh = sum(GWh)/1000), by=.(scenario, Type, Reserve)]
  

  setorder(res.plot,Type)
  # Create plot
  p1 = ggplot() +
         geom_bar(data = res.plot, aes(x = scenario, y = TWh, fill=Type, order=as.numeric(Type)), stat="identity", position="stack" ) +
         scale_fill_manual('', values = gen.color, limits=rev(gen.order))+     
         labs(y="Provision (TWh)", x=NULL)+
         guides(color = guide_legend(order=1), fill = guide_legend(order=2))+
         facet_wrap(~Reserve, scales="free_y", ncol=2) +
         theme(    legend.key =      element_rect(color="grey80", size = 0.8),
                   legend.key.size = grid::unit(1.0, "lines"),
                   legend.text =     element_text(size=text.plot),
                   legend.title =    element_blank(),
           #                         text = element_text(family="Arial"),
                   axis.text =       element_text(size=text.plot/1.2),
                   # axis.text.x =   element_text(face=2),
                   axis.title =      element_text(size=text.plot, face=2),
                   axis.title.y =    element_text(vjust=1.2),
                   panel.margin =    unit(1.5, "lines"),
                   aspect.ratio =    2.5 )
  print(p1)
}

} else { print('Section not run according to input file.')}
