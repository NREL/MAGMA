# Check if this section was selected to run in the input file
if (interface.flow.plots) {
  if (length(interfaces)>0) {
    if ( typeof(interval.interface.flow) == 'character' ) {
      p.int.interface = 'INPUT ERROR: interval.interface.flow not correct. Cannot run this section'
      p.int.interface.diff = 'INPUT ERROR: interval.interface.flow not correct. Cannot run this section'
      p.daily.interface = 'INPUT ERROR: interval.interface.flow not correct. Cannot run this section'
      p.daily.interface.diff = 'INPUT ERROR: interval.interface.flow not correct. Cannot run this section'
    } else { 
      # Call the query function to get interface flows for the interfaces selected in the query function.
      interface.flows = tryCatch( interval_interface_flows(interval.interface.flow), 
                                  error=function(cond) {return('ERROR: interface_flows query not returning correct results.')})
        
      # Check for errors in the query function. If theres an error don't continue.
      if ( typeof(interface.flows)=='character' ) { 
        p.int.interface = 'ERROR: interface_flows function not returning correct results.'
        p.int.interface.diff = 'ERROR: interface_flows function not returning correct results.'
        p.daily.interface = 'ERROR: interface_flows function not returning correct results.'
        p.daily.interface.diff = 'ERROR: interface_flows function not returning correct results.'
      } else {
        # change name to interface in order to not hit recursive error
        interface.flows[, interface:=factor(name, levels = interfaces)]
        # Add day and interval
        interface.flows[, day := as.POSIXlt(time)[[8]] ]
        interface.flows[, interval := 1:intervals.per.day, by=.(day,scenario)]
        # Get interval plots
        avg.interface = interface.flows[, .(value=mean(value)),by=.(scenario,interface,interval)]
        avg.interface[, hour := floor((interval-1)*(3600*24/intervals.per.day)/3600)]
        avg.interface[, minute := floor((interval-1)*(3600*24/intervals.per.day)/60-hour*60)]
        avg.interface[, second := floor((interval-1)*(3600*24/intervals.per.day)-hour*3600-minute*60)]
        avg.interface[, time := as.POSIXct(paste(hour,minute,second, sep=":"),format="%H:%M:%S",tz='UTC')]
      
        p.int.interface = interface_plot(avg.interface, x_col = 'time', color='scenario')
        p.int.interface = p.int.interface + facet_wrap(~interface,ncol=1,scales='free') +
                            scale_x_datetime(breaks = date_breaks(width = "2 hour"), labels = date_format("%H:%M"), 
                                             expand = c(0, 0), timezone='UTC')
        # Make diff plots
        diff.avg.interface = avg.interface[, .(scenario, value = value-value[as.character(scenario)==ref.scenario]),
                                           by=.(time,interface)]
        p.int.interface.diff = interface_plot(diff.avg.interface, x_col = 'time', color='scenario')
        p.int.interface.diff = p.int.interface.diff + facet_wrap(~interface,ncol=1,scales='free') +
                            scale_x_datetime(breaks = date_breaks(width = "2 hour"), labels = date_format("%H:%M"), 
                                             expand = c(0, 0), timezone='UTC') + ylab("Difference in Flow (GW)")

        # Aggregate interval flow data into daily flow data
        daily.flows = interface.flows[, .(value=mean(value)), by=.(day,name,scenario)]
        daily.flows[,time:=as.POSIXct(as.character(day+1),format="%j")]
        p.daily.interface = interface_plot(daily.flows, x_col = 'time', color='scenario')
        if (nrow(daily.flows) > 30*length(interfaces)){
          p.daily.interface = p.daily.interface + scale_x_datetime(breaks=date_breaks(width="1 month"), 
                     labels = date_format("%b"), expand = c(0, 0), timezone='UTC')
        }
        p.daily.interface = p.daily.interface + facet_wrap(~name,ncol=1,scales='free')
        # Make diff plots
        diff.daily.flows = daily.flows[, .(scenario, value = value - value[as.character(scenario)==ref.scenario]), 
                                     by=.(time,name)]
        p.daily.interface.diff = interface_plot(diff.daily.flows, x_col = 'time', color='scenario')
        if (nrow(daily.flows) > 30*length(interfaces)){
          p.daily.interface.diff = p.daily.interface.diff + scale_x_datetime(breaks=date_breaks(width="1 month"), 
                     labels = date_format("%b"), expand = c(0, 0), timezone='UTC')
        }
        p.daily.interface.diff = p.daily.interface.diff + facet_wrap(~name,ncol=1,scales='free') + ylab("Difference in Flow (GW)")
      }
    }
  } else { p.int.interface = 'No interfaces specified. No interface data will be shown.' 
           p.int.interface.diff = 'No interfaces specified. No interface data will be shown.' 
           p.daily.interface = 'No interfaces specified. No interface data will be shown.' 
           p.daily.interface.diff = 'No interfaces specified. No interface data will be shown.' 
         }

} else { p.int.interface = 'Section not run according to input file.' 
         p.int.interface.diff = 'Section not run according to input file.' 
         p.daily.interface = 'Section not run according to input file.' 
         p.daily.interface.diff = 'Section not run according to input file.' 
       }