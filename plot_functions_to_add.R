# process DOE gen data to get min gen table
source('process_DOE_gen_data.R')

scenario.order <- c('BaseCase','BR30','lowTX30','BR50','lowTx50')
re.gen.types <- c('Solar-Solar','Wind-Wind')

# installed capacity table to be used in installed capacity and net load duration plots
installed.MW <- merge(total.installed.cap[,.(name, category, value, scenario)], 
                      region.zone.mapping, 
                      by = "name", 
                      all.x = TRUE)

installed.MW <- installed.MW[,.(value.GW = sum(value)/1000), 
                             by = .(Type, scenario, Region, Zone)]

# set region order
zone.order <- unique(region.zone.mapping[,.(Region, Zone)])
setorder(zone.order, Region, Zone)

zone.order.v <- zone.order[,Zone]

# set orders to make plots nicer (order region by interconnection, then zone, fuel with RE on top)
installed.MW[,Type := factor(Type, levels = gen.order)]
installed.MW[,Zone := factor(Zone, levels = zone.order.v)]
installed.MW <- installed.MW[,scenario := factor(scenario, levels = scenario.order)]

setorder(installed.MW, Zone, Type, scenario)

#------------------------------------------------------------------------------|
# installed capacity - total ----
#------------------------------------------------------------------------------|
# do this when we have installed capacity in the solutions

# set aggregation to 'all' or 'interconnection' or 'zone' or 'percent'
plot_installed_MW <- function(aggregation = "all") {
  
  # argument check
  if (!(aggregation %in% c("all", "interconnection", "zone", "percent"))) {
    stop("in plot_installed_MW, please set aggregation to all, interconnetion, zone, or percent")
  }
  
  
  if (aggregation == "zone") {
    
    # plot
    ggplot() +
      geom_bar(data = installed.MW, aes_string(x = 'scenario', y = 'value.GW', fill='Type'), stat="identity", position="stack" ) +
      facet_wrap(~Zone, scales = 'free_x') +
      scale_fill_manual('', values = gen.color, limits=rev(gen.order))+     
      labs(y="Installed Capacity (GW)", x=NULL)+
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
                panel.spacing =    unit(1.5, "lines"),
                aspect.ratio = 2.5/length(db.loc),
                axis.text.x = element_text(angle = -30, hjust = 0)) 
    
    
  } else if (aggregation == "interconnection") {
    
    # sum by interconnection
    installed.MW <- installed.MW[,.(value.GW = sum(value.GW)), 
                                 by = .(Type, scenario, Region)]
    
    setorder(installed.MW, Type)
    
    # plot
    ggplot() +
      geom_bar(data = installed.MW, aes_string(x = 'scenario', y = 'value.GW', fill='Type'), stat="identity", position="stack" ) +
      facet_wrap(~Region, scales = 'free_x') +
      scale_fill_manual('', values = gen.color, limits=rev(gen.order))+     
      labs(y="Installed Capacity (GW)", x=NULL)+
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
                panel.spacing =    unit(1.5, "lines"),
                aspect.ratio = 2.5/length(db.loc),
                axis.text.x = element_text(angle = -30, hjust = 0)) 
    
  } else if (aggregation == "percent") {
    installed.MW <- installed.MW[,.(value.GW = sum(value.GW)), 
                                 by = .(Type, scenario)]
    
    setorder(installed.MW, Type)
    
    #percent of total
    installed.MW[, Total:=sum(value.GW), by=.(scenario)]
    installed.MW[, Percent:=value.GW/Total*100]
    installed.MW[, Total:=NULL]
    
    #plot
    ggplot() +
      geom_bar(data = installed.MW, aes_string(x = 'scenario', y = 'Percent', fill='Type'), stat="identity", position="stack" ) +
      scale_fill_manual('', values = gen.color, limits=rev(gen.order))+     
      labs(y="Percent of Installed Capacity (%)", x=NULL)+
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
                panel.spacing =    unit(1.5, "lines"),
                aspect.ratio = 2.5/length(db.loc),
                axis.text.x = element_text(angle = -30, hjust = 0))
    
    # row.names(installed.MW) = NULL
    # # kable(installed.MW, format.args = list(big.mark = ','), digits=2) 
    # write.csv(x = installed.MW, file = 'OutputFiles/Analysis/Installed Capacity/total_installed_capacity.csv',row.names = FALSE)
    
    
  } else {
    # sum 
    installed.MW <- installed.MW[,.(value.GW = sum(value.GW)), 
                                 by = .(Type, scenario)]
    
    setorder(installed.MW, Type)
    
    # plot
    ggplot() +
      geom_bar(data = installed.MW, aes_string(x = 'scenario', y = 'value.GW', fill='Type'), stat="identity", position="stack" ) +
      scale_fill_manual('', values = gen.color, limits=rev(gen.order))+     
      labs(y="Installed Capacity (GW)", x=NULL)+
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
                panel.spacing =    unit(1.5, "lines"),
                aspect.ratio = 2.5/length(db.loc),
                axis.text.x = element_text(angle = -30, hjust = 0))
  
  }
  
  
}

#------------------------------------------------------------------------------|
# emissions - total ----
#------------------------------------------------------------------------------|

# set aggregation to 'all' or 'interconnection' or 'zone' or 'percent'
plot_emissions <- function(GHG = 'CO2e') {
  
  # argument check
  if (!(GHG %in% c("CO2", "CO2e"))) {
    stop("in plot_emissions, please set GHG to CO2 or CO2e")
  }
  
  # pull data
  emission.factors <- data.table(read.csv('C:/Users/jkatz/PH-PLEXOS/InputFiles/emission_factors.csv'))
  
  
  fuel.offtake <- data.table(query_year(db,'Fuel','Offtake'))
  fuel.offtake <- fuel.offtake[name %in% emission.factors[,Type]]
  
  annual.emissions <- merge(fuel.offtake,emission.factors,
                            by.x='name',
                            by.y='Type',
                            all.x=TRUE)
  
  annual.emissions[, ton.CO2 := lb.CO2.per.MMBTU*value/2000]
  annual.emissions[, ton.CO2e := lb.CO2e.per.MMBTU*value/2000]
  
  if (GHG == "CO2") {
    
    # plot
    ggplot() +
      geom_bar(data = annual.emissions[,.(ton.CO2 = sum(ton.CO2)), by = .(scenario)], 
               aes_string(x = 'scenario', y = 'ton.CO2'), stat="identity", position="stack" ) +
      labs(y="Annual CO2 Emissions (tons)", x=NULL)+
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
                panel.spacing =    unit(1.5, "lines"),
                aspect.ratio = 2.5/length(db.loc),
                axis.text.x = element_text(angle = -30, hjust = 0)) 
    
    
  }
  
  else {
    
    # plot
    ggplot() +
      geom_bar(data = annual.emissions[,.(ton.CO2e = sum(ton.CO2e)), by = .(scenario)], 
               aes_string(x = 'scenario', y = 'ton.CO2e'), stat="identity", position="stack" ) +
      labs(y="Annual CO2e Emissions (tons)", x=NULL)+
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
                panel.spacing =    unit(1.5, "lines"),
                aspect.ratio = 2.5/length(db.loc),
                axis.text.x = element_text(angle = -30, hjust = 0)) 
    
  }



}

#------------------------------------------------------------------------------|
# Net load 
#------------------------------------------------------------------------------|

# set graph type to 'duration curve' or 'histogram'
plot_net_load <- function(aggregation = 'all', type = "duration curve") {
  
  # argument check
  if (!(aggregation %in% c("all", "interconnection"))) {
    stop("in plot_net_load, please set aggregation to all or interconnection.
         Interconnection functionality only works for the net load graph, not net load ramps")
  }
  
  if (!(type %in% c("duration curve", "histogram"))) {
    stop("in plot_net_load, please set graph type to duration curve or histogram")
  }
  
  # pull data
  interval.generation.region <- merge(interval.generation,region.zone.mapping[,.(name,Region,Zone)],
                                      by.x = 'name',
                                      by.y = 'name', all.x = T)
  
  interval.re.generation <- interval.generation.region[category %in% re.gen.types,
                                                       .(re.generation = sum(value)),
                                                       by = .(time,scenario,Region)]
  
  interval.net.load <- merge(interval.region.load, interval.re.generation,
                             by.x = c("time", "scenario", "name"), 
                             by.y = c("time", "scenario", "Region"), all.x = T)
  
  interval.net.load[, net.load := value - re.generation]
  
  
  if (aggregation == "interconnection" && type == 'duration curve') {
    
    net.load.duration <- interval.net.load[, .(net.load = sort(net.load, decreasing = T),
                                               duration = 1:(.N)),
                                           by = .(scenario,name)]
    
    #plot
    net.load.duration.plot <-ggplot(net.load.duration)+
      geom_line(aes(x=duration, y=net.load, color = scenario), size=0.8)+
      labs(y="Net Load (MW)", x="Hours of the Year")+
      scale_color_discrete(name="Scenario")+
      facet_wrap(~name)+
      theme( legend.key =       element_rect(color = "grey80", size = 0.4),
           legend.key.size =  grid::unit(0.9, "lines"), 
           legend.text =      element_text(size=text.plot/1.1),
           strip.text =       element_text(size=rel(0.7)),
           axis.text =        element_text(size=text.plot/1.2), 
           axis.title =       element_text(size=text.plot, face=2), 
           axis.title.x =     element_text(vjust=-0.3),
           panel.grid.major = element_line(colour = "grey85"),
           panel.grid.minor = element_line(colour = "grey93"),
           panel.spacing =     unit(1.0, "lines"),
           aspect.ratio =     .65)
    
  }
  
  else if (aggregation == "all" && type == 'duration curve') {
    
    net.load.duration <- interval.net.load[,.(net.load = sum(net.load)), by = .(time, scenario)]
    
    net.load.duration <- net.load.duration[, .(net.load = sort(net.load, decreasing = T),
                                               duration = 1:(.N)),
                                           by = .(scenario)]
    
    #plot
    net.load.duration.plot <-ggplot(net.load.duration)+
      geom_line(aes(x=duration, y=net.load, color = scenario), size=0.8)+
      labs(y="Net Load (MW)", x="Hours of the Year")+
      scale_color_discrete(name="Scenario")+
      theme( legend.key =       element_rect(color = "grey80", size = 0.4),
             legend.key.size =  grid::unit(0.9, "lines"), 
             legend.text =      element_text(size=text.plot/1.1),
             strip.text =       element_text(size=rel(0.7)),
             axis.text =        element_text(size=text.plot/1.2), 
             axis.title =       element_text(size=text.plot, face=2), 
             axis.title.x =     element_text(vjust=-0.3),
             panel.grid.major = element_line(colour = "grey85"),
             panel.grid.minor = element_line(colour = "grey93"),
             panel.spacing =     unit(1.0, "lines"),
             aspect.ratio =     .65)
    
  }
  
  else if (aggregation == "interconnection" && type == 'histogram') {
    stop("Interconnection facet is currently only available for the duration curve graph type, not for histograms")
    
  }
  
  else {
    ## Net load histogram
    net.load.duration.plot <- ggplot(data = interval.net.load[,.(net.load = sum(net.load)), by = .(time,scenario)], aes(net.load))+  
      geom_histogram(binwidth=500,color='White',fill='Black') +  
      facet_wrap("scenario", scales = 'fixed') +  
      xlab("Net Load (MW)") +  
      ylab("Number of Hours") +
      theme( legend.key =       element_rect(color = "grey80", size = 0.4),
           legend.key.size =  grid::unit(0.9, "lines"), 
           legend.text =      element_text(size=text.plot/1.1),
           strip.text =       element_text(size=rel(0.7)),
           axis.text =        element_text(size=text.plot/1.2), 
           axis.title =       element_text(size=text.plot, face=2), 
           axis.title.x =     element_text(vjust=-0.3),
           panel.grid.major = element_line(colour = "grey85"),
           panel.grid.minor = element_line(colour = "grey93"),
           panel.spacing =     unit(1.0, "lines"),
           aspect.ratio =     0.65,
           axis.text.x = element_text(angle = -30, hjust = 0))
    
  }
  
  return(net.load.duration.plot)
  
  
}

#------------------------------------------------------------------------------|
# Net load ramp
#------------------------------------------------------------------------------|

# set graph type to 'duration curve' or 'histogram'
plot_net_load_ramps <- function(fueltype = 'total', type = "duration curve", normalized = FALSE) {
  
  #argument check
  if (!(fueltype %in% c('total', 'each'))) {
    stop("in plot_net_load_ramp, please set fuel type total or each")
  }
  
  if (!(type %in% c('duration curve','histogram'))) {
    stop("in plot_net_load_ramp, please set graph type to duration curve or histogram")
  }
  
  
  
  
  interval.re.gen.total <- interval.generation[category %in% re.gen.types,
                                               .(re.generation=sum(value)),
                                               by = .(time,scenario)]

  interval.load.total <- interval.region.load[,.('Load'=sum(value)),
                                              by = .(time,scenario)]

  interval.net.load.total <- merge(interval.load.total, interval.re.gen.total,
                                   by.x = c("time", "scenario"),
                                   by.y = c("time", "scenario"), all.x = T)

  interval.net.load.total[, net.load := Load - re.generation]
  
  interval.net.load.total[, net.load.ramp := net.load - shift(net.load, n=1L, type = "lag"), 
                          by = .(scenario)]
  
  if (fueltype == "total" && type == 'duration curve') {
    
    #ramp duration
    net.load.ramp <- interval.net.load.total[, .(ramp.rate = sort(net.load.ramp, decreasing = T),
                                                 duration = 1:(.N-1)),
                                             by = .(scenario)]
    #plot 
    
    net.load.ramp.plot <- ggplot(net.load.ramp)+
      geom_line(aes(x=duration, y=ramp.rate, color = scenario), size=0.8)+
      scale_color_discrete(name="Scenario")+
      labs(y="Net Load Ramp Rate (MW/hour)", x="")+
      theme( legend.key =       element_rect(color = "grey80", size = 0.4),
             legend.key.size =  grid::unit(0.9, "lines"), 
             legend.text =      element_text(size=text.plot/1.1),
             strip.text =       element_text(size=rel(0.7)),
             axis.text =        element_text(size=text.plot/1.2), 
             axis.title =       element_text(size=text.plot, face=2), 
             axis.title.x =     element_text(vjust=-0.3),
             panel.grid.major = element_line(colour = "grey85"),
             panel.grid.minor = element_line(colour = "grey93"),
             panel.spacing =     unit(1.0, "lines"),
             aspect.ratio =     .65)
    
  }
  
  else if (fueltype == "total" && type == 'histogram') {
    # #Net load ramp histogram
    net.load.ramp.plot <- ggplot(data = interval.net.load.total, aes(net.load.ramp))+
      geom_histogram(binwidth=250,color='White',fill='Black') +
      facet_wrap("scenario") +
      xlab("Net Load Ramp Rate (MW/hour)") +
      ylab("Number of Hours") +
      theme( legend.key =       element_rect(color = "grey80", size = 0.4),
             legend.key.size =  grid::unit(0.9, "lines"), 
             legend.text =      element_text(size=text.plot/1.1),
             strip.text =       element_text(size=rel(0.7)),
             axis.text =        element_text(size=text.plot/1.2), 
             axis.title =       element_text(size=text.plot, face=2), 
             axis.title.x =     element_text(vjust=-0.3),
             panel.grid.major = element_line(colour = "grey85"),
             panel.grid.minor = element_line(colour = "grey93"),
             panel.spacing =     unit(1.0, "lines"),
             aspect.ratio =     0.65,
             axis.text.x = element_text(angle = -30, hjust = 0))
    
  }
  
  else if (fueltype == "each" && type == 'histogram') {
    stop("Fuel type facet is currently only available for the duration curve graph type, not for histograms")
    
  }
  
  else if (fueltype == 'each' && type == 'duration curve' && normalized == FALSE) {
    #ramps duration curve by fuel 
    fuel.ramps <- merge(interval.generation,region.zone.mapping,
                        by= c('name','Type'),
                        all.x = T)
    
    fuel.ramps <- fuel.ramps[, .(Generation = sum(value)),
                             by = .(Type, scenario, time)]
    
    fuel.ramps[, ramp := Generation - shift(Generation, 1L, type = "lag"),
               by = .(scenario, Type)]
    
    # convert to per mins
    fuel.ramps[,ramp := ramp/(60)]
    
    fuel.ramp.duration <- fuel.ramps[, .(ramp = sort(ramp, decreasing = T),
                                         duration = 1:(.N-1)),
                                     by = .(scenario, Type)]
    
    # plot
    net.load.ramp.plot <- ggplot(fuel.ramp.duration)+
      geom_line(aes(x=duration, y=ramp, color = scenario), size = 0.8)+
      scale_color_discrete(name="Scenario")+
      labs(y="Ramp Rate (MW/min)", x="") +
      facet_wrap(~Type) +
      theme( legend.key =       element_rect(color = "grey80", size = 0.4),
             legend.key.size =  grid::unit(0.9, "lines"), 
             legend.text =      element_text(size=text.plot/1.1),
             strip.text =       element_text(size=rel(0.7)),
             axis.text =        element_text(size=text.plot/1.2), 
             axis.title =       element_text(size=text.plot, face=2), 
             axis.title.x =     element_text(vjust=-0.3),
             panel.grid.major = element_line(colour = "grey85"),
             panel.grid.minor = element_line(colour = "grey93"),
             panel.spacing =     unit(1.0, "lines"),
             aspect.ratio =     .65)
    
  }
  
  else if (fueltype == 'each' && type == 'duration curve' && normalized == TRUE) {
    #normalized ramp duration by fuel

    ramps.and.installed.MW <- merge(fuel.ramps,installed.MW[,.(value.GW = sum(value.GW)), 
                                                            by = .(Type,scenario)],
                                    by = c('scenario', 'Type'),
                                    all.x =TRUE)
    
    normalized.ramps <- ramps.and.installed.MW[, normalized.ramp := ramp / (value.GW)]
    
    normalized.ramp.duration <-normalized.ramps[, .(normalized.ramp = sort(normalized.ramp, decreasing = T),
                                                    duration = 1:(.N-1)),
                                                by = .(scenario, Type)]
    
    # plot
    net.load.ramp.plot <- ggplot(normalized.ramp.duration)+
      geom_line(aes(x=duration, y=normalized.ramp, color = scenario), size = 0.8)+
      scale_color_discrete(name="Scenario")+
      labs(y="Ramp Rate (MW/min) per Installed GW", x="") +
      facet_wrap(~Type) +
      theme( legend.key =       element_rect(color = "grey80", size = 0.4),
             legend.key.size =  grid::unit(0.9, "lines"), 
             legend.text =      element_text(size=text.plot/1.1),
             strip.text =       element_text(size=rel(0.7)),
             axis.text =        element_text(size=text.plot/1.2), 
             axis.title =       element_text(size=text.plot, face=2), 
             axis.title.x =     element_text(vjust=-0.3),
             panel.grid.major = element_line(colour = "grey85"),
             panel.grid.minor = element_line(colour = "grey93"),
             panel.spacing =     unit(1.0, "lines"),
             aspect.ratio =     .65)
    
  }

  else {
    stop("Normalized ramp rates only calculated for fueltype = each and type = duration curve")
  }
  
  return(net.load.ramp.plot)
  
}

#------------------------------------------------------------------------------|
# Number of starts
#------------------------------------------------------------------------------|

plot_starts <- function() {
  
  units.started.month <- data.table(query_month(db,'Generator','Units Started',columns = c('category','name')))
  
  units.started <- merge(units.started.month,region.zone.mapping,
                         by.x = 'name',
                         by.y = 'name',
                         all.x = TRUE)
  
  units.started <- units.started[!category %in% re.gen.types,
                                 .(starts = sum(value)),
                                 by = .(time,scenario,Type)]
  
  ggplot(units.started)+
    geom_point(aes(x=time, y=starts, color = scenario))+
    scale_color_discrete(name = 'Scenario')+
    labs(y='Number of Starts', x = 'Month')+
    facet_wrap(~Type, scales = 'free_y')+
    theme(    legend.key =      element_rect(color="grey80", size = 0.8),
              legend.key.size = grid::unit(1.0, "lines"),
              legend.text =     element_text(size=text.plot),
              legend.title =    element_blank(),
              #                         text = element_text(family="Arial"),
              axis.text =       element_text(size=text.plot/1.2),
              # axis.text.x =   element_text(face=2),
              axis.title =      element_text(size=text.plot, face=2),
              axis.title.y =    element_text(vjust=1.2),
              panel.spacing =    unit(1.5, "lines"),
              aspect.ratio = 2.5/length(db.loc),
              axis.text.x = element_text(angle = -30, hjust = 0)) 
  
}

#------------------------------------------------------------------------------|
# Minimum generation by fuel type
#------------------------------------------------------------------------------|

# Define fuel type, and also whether to calculate minimum generation as a percent of hours on, or percent of hours of the year
plot_min_gen <- function(fueltype = 'Coal', percent.of = 'year') {
  
  # argument check
  if (!(fueltype %in% installed.MW[,Type])) {
    stop("in plot_min_gen, please choose a fuel type")
  }
  
  if (!(percent.of %in% c('year','hours on'))) {
    stop("in plot_min_gen, please specify whether to calculate min gen as a percent of hours ON or a percent of YEAR")
  }
  
  min.gen.table <- merge(interval.generation, map.min.gen.by.gen.2030,                    
                         by.x = 'name', by.y = 'Generator.Name', all.x = TRUE)
  min.gen.table[value == `Min Stable Level`, atmin := 1]
  min.gen.table[value > 0,  on := 1]
  min.gen.table[`Min Stable Level` > 0, hasmingen := 0]
  min.gen.table[atmin - hasmingen == 1, include := 1]
  
  #summarize number of hours at min gen and hours on by generator
  hours.at.min.gen <- min.gen.table[,.(Type = unique(Type),                                     
                                       HrsAtMinGen = sum(include, na.rm = TRUE),                                     
                                       HrsOn = sum(on, na.rm = TRUE)),                                  
                                    by =.(scenario,name)]
  hours.at.min.gen <- hours.at.min.gen[, minGenPercOfOn := HrsAtMinGen/HrsOn*100]
  hours.at.min.gen <- hours.at.min.gen[, minGenPercOfYr := HrsAtMinGen/8736*100]
  
  fuel.min.gen = hours.at.min.gen[Type==fueltype]
  
  if (percent.of == 'hours on') {

    # histogram: number of generators by hours at min stable level as a percent of hours on
    mingen_hist = ggplot(data = fuel.min.gen, aes(minGenPercOfOn))+  
      geom_histogram(binwidth=5,color='White',fill='Black') +  
      facet_wrap("scenario") +  
      xlab("Hours at Minimum Stable Level (as a percent of hours on)") +  
      ylab("Number of Generators") + 
      ggtitle(fueltype)+
      theme( legend.key =       element_rect(color = "grey80", size = 0.4),
             legend.key.size =  grid::unit(0.9, "lines"), 
             legend.text =      element_text(size=text.plot/1.1),
             strip.text =       element_text(size=rel(0.7)),
             axis.text =        element_text(size=text.plot/1.2), 
             axis.title =       element_text(size=text.plot, face=2), 
             axis.title.x =     element_text(vjust=-0.3),
             panel.grid.major = element_line(colour = "grey85"),
             panel.grid.minor = element_line(colour = "grey93"),
             panel.spacing =     unit(1.0, "lines"),
             aspect.ratio =     0.65,
             axis.text.x = element_text(angle = -30, hjust = 0))
    
  }
      
   else {
     # histogram: number of generators by hours at min stable level as a percent of 8736
     mingen_hist <- ggplot(data = fuel.min.gen, aes(minGenPercOfYr))+  
       geom_histogram(binwidth=5,color='White',fill='Black') +  
       facet_wrap("scenario") +  
       xlab("Hours at Minimum Stable Level (as a percent of year)") +  
       ylab("Number of Generators")+
       ggtitle(fueltype) +
       theme( legend.key =       element_rect(color = "grey80", size = 0.4),
              legend.key.size =  grid::unit(0.9, "lines"), 
              legend.text =      element_text(size=text.plot/1.1),
              strip.text =       element_text(size=rel(0.7)),
              axis.text =        element_text(size=text.plot/1.2), 
              axis.title =       element_text(size=text.plot, face=2), 
              axis.title.x =     element_text(vjust=-0.3),
              panel.grid.major = element_line(colour = "grey85"),
              panel.grid.minor = element_line(colour = "grey93"),
              panel.spacing =     unit(1.0, "lines"),
              aspect.ratio =     0.65,
              axis.text.x = element_text(angle = -30, hjust = 0))
     
   } 
  
  return(mingen_hist)
  
}

#------------------------------------------------------------------------------|
# Unserved energy
#------------------------------------------------------------------------------|

table_USE <- function() {
  
  lost.load.hrs <- data.table(query_year(db,'Region','Hours of Unserved Energy'))
  setnames(lost.load.hrs,'value','Unserved Energy (Hrs)')
  
  lost.load <- data.table(query_year(db,'Region','Unserved Energy'))
  setnames(lost.load,'value','Unserved Energy (GWh)')
  
  lost.load.table <- merge(lost.load.hrs[,.(scenario,name,`Unserved Energy (Hrs)`)],
                           lost.load[,.(scenario,name,`Unserved Energy (GWh)`)],
                           by = c('scenario','name'),
                           all.x = TRUE)
  
  setorder(lost.load.table, name)
  

  
  return(lost.load.table)
  
}

#------------------------------------------------------------------------------|
# Dump energy
#------------------------------------------------------------------------------|

table_dump_energy <- function() {
  
  #hours of dump energy
  dump.energy.hours <- data.table(query_year(db,'Region','Hours of Dump Energy'))
  setnames(dump.energy.hours,'value','Dump Energy (Hrs)')
  
  dump.energy <- data.table(query_year(db,'Region','Dump Energy'))
  setnames(dump.energy,'value','Dump Energy (GWh)')
  
  dump.energy.table <- merge(dump.energy.hours[,.(scenario,name,`Dump Energy (Hrs)`)],
                             dump.energy[,.(scenario,name,`Dump Energy (GWh)`)],
                             by=c('scenario','name'),
                             all.x = TRUE)
  
  setorder(dump.energy.table, name)
  
  return(dump.energy.table)
  
}  