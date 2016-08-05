# Check if this section was selected run in the input file
if (total.gen.stack){

# Query annual generation by type.
yr.gen.scen = tryCatch( gen_diff_by_type(total.generation, total.avail.cap), error = function(cond) { return('ERROR') } )

# If the query doesn't work, print an error. Else continue.
if ( typeof(yr.gen)=='character' ) {
  print('ERROR: gen_by_type function not returning correct results.')
} else {

  # Separate out the positive and negative halves for easier plotting
  # also convert to TWh
  dat.pos = yr.gen.scen[GWh>=0 & scenario!=ref.scenario, 
                        .(TWh = sum(GWh)/1000), by=.(scenario,Type)]
  dat.neg = yr.gen.scen[GWh<0 & scenario!=ref.scenario, 
                        .(TWh = sum(GWh)/1000), by=.(scenario,Type)]

  # gen.sum is just used to set the maximum height on the plot, see pretty() fcn below
  gen.sum = rbindlist(list(dat.pos[, .(TWh = sum(TWh))], dat.neg[, .(TWh = sum(TWh))]))  

  # reorder the levels of Type to plot them in order
  dat.pos[, Type := factor(Type, levels = c(gen.order))]
  dat.neg[, Type := factor(Type, levels = c(gen.order))]
  if(any(is.na(yr.gen.scen$Type))) print("ERROR:gen.order doesn't contain all of the gen types: FIX YOUR INPUT DATA CSV")

  # Calculate difference in load
  tot.load.scen = r.load[,.(value = sum(value)/1000),by=.(scenario)]
  diff.load = tot.load.scen[,.(scenario, TWh = value-value[scenario==ref.scenario])]
  diff.load = diff.load[scenario!=ref.scenario, ]
  
  # This automatically creates the y-axis scaling
  py = pretty(gen.sum$TWh, n=5, min.n = 5)
  seq.py = seq(py[1], py[length(py)], 0.5*(py[2]-py[1]))

  setorder(dat.pos,Type)
  setorder(dat.neg,Type)
  # Create plot
  p1 = ggplot() +
         geom_bar(data = dat.pos, aes(x = scenario, y = TWh, fill=Type), stat="identity", position="stack" ) +
         geom_bar(data = dat.neg, aes(x = scenario, y = TWh, fill=Type), stat="identity", position="stack" ) +
         geom_errorbar(data = diff.load, aes(x = scenario, ymin=TWh, ymax=TWh, color='load'), size=0.45, linetype='longdash')+
         scale_color_manual(name='', values=c("load"="grey40"), labels=c("Load"))+
         scale_fill_manual('', values = gen.color, limits=rev(gen.order))+     
         labs(y="Generation (TWh)", x=NULL)+
         scale_y_continuous(breaks=seq.py, limits=c(min(py), max(py)), expand=c(0,0), label=comma)+
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
                   aspect.ratio =    2.5/length(unique(dat.pos$scenario)) )
  print(p1)
}

} else { print('Section not run according to input file.')}
