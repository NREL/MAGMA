pretty_axes <- function(data1, data2=NULL, filters=NULL){
  # Function to create well spaced axes for plots. 
  # Takes maximum of both data sets (if two are provided)
  # and nicely spaces axes
  # Inputs:
  #   data1 - data used for plot
  #   filters - columns to aggregate over
  
  # Aggregate data
  if(is.null(filters)){
    sum1 = data1[, .(value = sum(value)) ]  
    if (!is.null(data2)){
      sum2 = data2[, .(value = sum(value)) ]
      sum1 = rbindlist(list(sum1,sum2))
    }
  }else{
    sum1 = data1[, .(value = sum(value)), by=filters]  
    if (!is.null(data2)){
      sum2 = data2[, .(value = sum(value)), by=filters]
      sum1 = rbindlist(list(sum1,sum2))
    }
  }
  
  # This automatically creates the y-axis scaling
  py = pretty(c(sum1$value,0), n=5, min.n = 5)
  seq.py = seq(py[1], py[length(py)], 10*(py[2]-py[1]))
  return(py)
}


gen_stack_plot <- function(gen.data, load.data, filters=NULL){
  # Creates total generation stack plot
  # Assumes data has been processed according to XXXX
  # filters are other things you might want to plot over
  # Returns plot handle for ggplot, can add things like facet to that
  
  # reorder the levels of Type to plot them in order
  gen.data[, Type := factor(Type, levels = c(gen.order))]
  if(any(is.na(gen.data$Type))) print("ERROR:gen.order doesn't contain all of the gen types: FIX YOUR INPUT DATA CSV")
  
  if(is.null(filters)){
    aggfilters = c("scenario","Type")
  }else{
    aggfilters = c(c("scenario","Type"),filters)
  }
  loadfilters = aggfilters[aggfilters!='Type']
  
  # Group by type and convert GWh to TWh
  gen.plot = gen.data[, .(TWh = sum(GWh)/1000), by=aggfilters]
  setorder(gen.plot,Type)
  
  tot.load = load.data[, .(TWh = sum(value)/1000), by=loadfilters]
  
  seq.py = pretty_axes(gen.plot[, value:=TWh ], tot.load[, value:=TWh ], filters=loadfilters)

  # Create plot
  p1 = ggplot() +
    geom_bar(data = gen.plot, aes(x = scenario, y = TWh, fill=Type, order=as.numeric(Type)), stat="identity", position="stack" ) +
    geom_errorbar(data = tot.load, aes(x = scenario, y=TWh, ymin=TWh, ymax=TWh, color='load'), size=0.45, linetype='longdash')+
    scale_color_manual(name='', values=c("load"="grey40"), labels=c("Load"))+
    scale_fill_manual('', values = gen.color, limits=rev(gen.order))+     
    labs(y="Generation (TWh)", x=NULL)+
    scale_y_continuous(breaks=seq.py, limits=c(min(seq.py), max(seq.py)), expand=c(0,0), label=comma)+
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
  
  return(p1)
}