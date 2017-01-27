# Check if this section was selected to run in the input file
if (line.flow.plots) {
  if (length(lines)>0) {
    if ( typeof(interval.line.flow) == 'character' ) {
      p.int.line = 'INPUT ERROR: interval.line.flow not correct. Cannot run this section'
      p.int.line.diff = 'INPUT ERROR: interval.line.flow not correct. Cannot run this section'
      p.daily.line = 'INPUT ERROR: interval.line.flow not correct. Cannot run this section'
      p.daily.line.diff = 'INPUT ERROR: interval.line.flow not correct. Cannot run this section'
    } else { 
      # Call the query function to get line flows for the lines selected in the query function.
      line.flows = tryCatch( interval_line_flows(interval.line.flow), 
                                  error=function(cond) {return('ERROR: line_flows query not returning correct results.')})
        
      # Check for errors in the query function. If theres an error don't continue.
      if ( typeof(line.flows)=='character' ) { 
        p.int.line = 'ERROR: line_flows function not returning correct results.'
        p.int.line.diff = 'ERROR: line_flows function not returning correct results.'
        p.daily.line = 'ERROR: line_flows function not returning correct results.'
        p.daily.line.diff = 'ERROR: line_flows function not returning correct results.'
      } else {
        # change name to line in order to not hit recursive error
        line.flows[, line:=factor(name, levels = lines)]
        # Add day and interval
        line.flows[, day := as.POSIXlt(time)[[8]] ]
        line.flows[, interval := 1:intervals.per.day, by=.(day,scenario)]
        # Get interval plots
        avg.line = line.flows[, .(value=mean(value)),by=.(scenario,line,interval)]
        avg.line[, hour := floor((interval-1)*(3600*24/intervals.per.day)/3600)]
        avg.line[, minute := floor((interval-1)*(3600*24/intervals.per.day)/60-hour*60)]
        avg.line[, second := floor((interval-1)*(3600*24/intervals.per.day)-hour*3600-minute*60)]
        avg.line[, time := as.POSIXct(paste(hour,minute,second, sep=":"),format="%H:%M:%S",tz='UTC')]
      
        p.int.line = interface_plot(avg.line, x_col = 'time', color='scenario')
        p.int.line = p.int.line + facet_wrap(~line,ncol=1,scales='free') +
                            scale_x_datetime(breaks = date_breaks(width = "2 hour"), labels = date_format("%H:%M"), 
                                             expand = c(0, 0), timezone='UTC')
        # Make diff plots
        diff.avg.line = avg.line[, .(scenario, value = value-value[as.character(scenario)==ref.scenario]),
                                           by=.(time,line)]
        p.int.line.diff = interface_plot(diff.avg.line, x_col = 'time', color='scenario')
        p.int.line.diff = p.int.line.diff + facet_wrap(~line,ncol=1,scales='free') +
                            scale_x_datetime(breaks = date_breaks(width = "2 hour"), labels = date_format("%H:%M"), 
                                             expand = c(0, 0), timezone='UTC') + ylab("Difference in Flow (GW)")

        # Aggregate interval flow data into daily flow data
        daily.flows = line.flows[, .(value=mean(value)), by=.(day,name,scenario)]
        daily.flows[,time:=as.POSIXct(as.character(day+1),format="%j")]
        p.daily.line = interface_plot(daily.flows, x_col = 'time', color='scenario')
        if (nrow(daily.flows)/length(db.loc) > 30*length(lines)){
          p.daily.line = p.daily.line + scale_x_datetime(breaks=date_breaks(width="1 month"), 
                     labels = date_format("%b"), expand = c(0, 0), timezone='UTC')
        }
        p.daily.line = p.daily.line + facet_wrap(~name,ncol=1,scales='free')
        # Make diff plots
        diff.daily.flows = daily.flows[, .(scenario, value = value - value[as.character(scenario)==ref.scenario]), 
                                     by=.(time,name)]
        p.daily.line.diff = interface_plot(diff.daily.flows, x_col = 'time', color='scenario')
        if (nrow(daily.flows)/length(db.loc) > 30*length(lines)){
          p.daily.line.diff = p.daily.line.diff + scale_x_datetime(breaks=date_breaks(width="1 month"), 
                     labels = date_format("%b"), expand = c(0, 0), timezone='UTC')
        }
        p.daily.line.diff = p.daily.line.diff + facet_wrap(~name,ncol=1,scales='free') + ylab("Difference in Flow (GW)")
      }
    }
  } else { p.int.line = 'No lines specified. No line data will be shown.' 
           p.int.line.diff = 'No lines specified. No line data will be shown.' 
           p.daily.line = 'No lines specified. No line data will be shown.' 
           p.daily.line.diff = 'No lines specified. No line data will be shown.' 
         }

} else { p.int.line = 'Section not run according to input file.' 
         p.int.line.diff = 'Section not run according to input file.' 
         p.daily.line = 'Section not run according to input file.' 
         p.daily.line.diff = 'Section not run according to input file.' 
       }