# Check if this section was selected to run in the input file
if (revenue.plots) {
  
  # Check inputs
  if ( typeof(interval.generation) == 'character' ) { 
    print('INPUT ERROR: interval.generation has errors. Cannot run this section.')
  } else if ( typeof(interval.region.price) == 'character' ) { 
    print('INPUT ERROR: interval.region.price has errors. Cannot run this section.')
  } else if ( typeof(interval.reserve.price) == 'character' ) { 
    print('INPUT ERROR: interval.reserve.price has errors. Cannot run this section.')
  } else if ( typeof(interval.gen.reserve.provision) == 'character' ) { 
    print('INPUT ERROR: interval.gen.reserve.provision has errors. Cannot run this section.')
  } else{
    # Query curtailment data
    r.z.revenue = tryCatch( revenue_calculator(interval.generation, interval.pump.load, interval.region.price,
                                               interval.gen.reserve.provision,interval.reserve.price), 
                            error = function(cond) { return('ERROR')})
    
    # If there is a problem with the query return an error.
    if ( typeof(r.z.revenue)=='character' ) { 
      print('ERROR: revenue_calculator function not returning correct results.')
    } else {
      total.revenue = r.z.revenue[, .(revenue=sum(revenue)), by=.(scenario,Type,Revenue_Type)]
      total.revenue[, Revenue_Type:=factor(Revenue_Type,levels=c('Generation','Reserves','Charging'))]
      net.revenue = total.revenue[, .(revenue=sum(revenue), Revenue_Type='Net Revenue'), by=.(scenario,Type)]
      
      p = ggplot(total.revenue)+
        geom_bar(aes(scenario,revenue/10^6,fill=Revenue_Type), stat='identity')+
        geom_errorbar(data = net.revenue, aes(x = scenario, ymin=revenue/10^6, ymax=revenue/10^6,linetype=Revenue_Type), 
                        size=0.75) +
        ylab("Total Revenue, Million $") + xlab("Scenario")+
        scale_fill_manual(values=c('dodgerblue2','goldenrod1','firebrick1'),name='Revenue Type')+
        scale_linetype_manual(values='longdash',name='')+
        facet_wrap(~Type,ncol=3,scales='free') +
        theme(    aspect.ratio    = 1.5,
                  legend.key      = element_rect(color="grey80", size = 0.8),
                  legend.key.size = grid::unit(1.0, "lines"),
                  legend.text     = element_text(size=text.plot),
                  legend.title    = element_blank(),
                  axis.text       = element_text(size=text.plot/1.2),
                  axis.text.x     = element_text(angle = -30, hjust = 0),
                  axis.title      = element_text(size=text.plot, face=2),
                  axis.title.y    = element_text(vjust=1.2),
                  strip.text      = element_text(size = text.plot),
                  panel.spacing   = unit(1.5, "lines"))
      print(p)
    }
  }
} else { print('Section not run according to input file.') }