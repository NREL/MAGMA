# Check if this section was selected run in the input file
if (total.gen.stack){

# Query annual generation by type.
yr.gen = tryCatch( gen_by_type(total.generation, total.avail.cap), error = function(cond) { return('ERROR') } )

# If the query doesn't work, print an error. Else continue.
if ( typeof(yr.gen)=='character' ) {
  print('ERROR: gen_by_type function not returning correct results.')
} else {

  # gen.sum is just used to set the maximum height on the plot, see pretty() fcn below
  gen.sum = yr.gen[, .(TWh = sum(GWh)/1000)]  #change GWh generation to TWh

  # reorder the levels of Type to plot them in order
  yr.gen[, Type := factor(Type, levels = c(gen.order))]
  if(any(is.na(yr.gen$Type))) print("ERROR:gen.order doesn't contain all of the gen types: FIX YOUR INPUT DATA CSV")

  # Group by type and convert GWh to TWh
  gen.plot = yr.gen[, .(TWh = sum(GWh)/1000), by=.(Type)]

  tot.load = sum(r.load$value)/1000
  
  # This automatically creates the y-axis scaling
  py = pretty(gen.sum$TWh, n=5, min.n = 5)
  seq.py = seq(0, py[length(py)], 10*(py[2]-py[1]))

  setorder(gen.plot,Type)
  # Create plot
  p1 = ggplot() +
         geom_bar(data = gen.plot, aes(x = 'Dispatch', y = TWh, fill=Type, order=as.numeric(Type)), stat="identity", position="stack" ) +
         geom_hline(aes(yintercept=tot.load, color='load'), size=0.45, linetype='longdash')+
         scale_color_manual(name='', values=c("load"="grey40"), labels=c("Load"))+
         scale_fill_manual('', values = gen.color, limits=rev(gen.order))+     
         labs(y="Generation (TWh)", x=NULL)+
         scale_y_continuous(breaks=seq.py, limits=c(0, max(py)), expand=c(0,0), label=comma)+
         guides(color = guide_legend(order=1), fill = guide_legend(order=2))+
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
