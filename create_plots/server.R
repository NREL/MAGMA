library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$plot1 <- renderPlot({
    plot.data = plot.data.all[Type %in% input$type & Region==input$region & Period==input$period, ]
    
    # Assign the order which the generation types will appear in the plot.
    plot.data[, Type := factor(Type, levels=rev(gen.order))]
    
    # this is just for scaling the y-axis (either by load or generation, whichever is bigger)
    stack.r = plot.data[, .(value = sum(DA.cap)), by=.(scenario,time)]
    
    # Check that there is actually generation data to plot, otherwise don't make a plot.
    
    blank_data = plot.data[, .(ylim=sum(RT.cap)/1000*1.06), by=.(scenario,time,Type)]
    plot.data[, lt:="DA Committed Capacity or \nForecasted Generation"]
    
    # Create plot
   ggplot()+geom_area(data=plot.data, aes(time, RT.gen/1000, fill=Type), alpha=0.4)+
      geom_line(data=plot.data[Type != 'Hydro', ], aes(time, RT.cap/1000, color=Type), size=1)+
      geom_line(data=plot.data[Type != 'Hydro', ], aes(time, DA.cap/1000, linetype=lt), color="grey50", size=1, alpha=0.5)+
      geom_blank(data=blank_data, aes(x=time, y=ylim))+
      expand_limits(y=0)+
      scale_linetype_manual("",values=c(1,1))+
      scale_fill_manual("RT Generation", values = gen.color)+
      scale_color_manual("RT Committed Capacity", values = gen.color)+
      ylab("Generation or Online Capacity (GW)")+xlab(NULL)+
      #     guides(color = guide_legend(order=1), fill = guide_legend(order=2, reverse=TRUE))+
      theme(legend.key = element_rect(color = "grey70", size = 0.8),
            legend.key.size = grid::unit(1.5, "lines"), 
            strip.text=element_text(face=1, 
                                    size=rel(0.8)), 
            panel.grid.major = element_line(colour = "grey85"),
            panel.grid.minor = element_line(colour = "grey93"),
            panel.margin = unit(0.45, "lines"))+facet_grid(scenario~Type, scales="free_y")
  })
})