# Check if this section was selected to run in the input file
if (daily.curtailment) {
  # Check inputs
  if ( typeof(interval.generation) == 'character' ) { 
    p.daily.type = 'INPUT ERROR: interval.generation has errors. Cannot run this section.'
    p.daily.type.diff = 'INPUT ERROR: interval.generation has errors. Cannot run this section.'
    p.daily = 'INPUT ERROR: interval.generation has errors. Cannot run this section.'
    p.daily.diff = 'INPUT ERROR: interval.generation has errors. Cannot run this section.'
  } else if ( typeof(interval.avail.cap) == 'character' ) { 
    p.daily.type = 'INPUT ERROR: interval.avail.cap has errors. Cannot run this section.'
    p.daily.type.diff = 'INPUT ERROR: interval.avail.cap has errors. Cannot run this section.'
    p.daily = 'INPUT ERROR: interval.avail.cap has errors. Cannot run this section.'
    p.daily.diff = 'INPUT ERROR: interval.avail.cap has errors. Cannot run this section.'
  } else{
    # Query curtailment data
    interval.curt = tryCatch( total_curtailment(interval.generation, interval.avail.cap), error = function(cond) { return('ERROR')})
    # If there is a problem with the query return an error.
    if ( typeof(interval.curt)=='character' ) { 
      p.daily.type = 'ERROR: daily_curtailment function not returning correct results.'
      p.daily.type.diff = 'ERROR: daily_curtailment function not returning correct results.'
      p.daily = 'ERROR: daily_curtailment function not returning correct results.'
      p.daily.diff = 'ERROR: daily_curtailment function not returning correct results.'
    } else {
      # Daily Curtailment plots
      # Calculate average curtailment for each day 
      daily.curt = interval.curt[,.(Curtailment = mean(Curtailment)),by=.(scenario,day,year,Type)] 
      daily.curt[, timeformat := sprintf("%d %d", day+1, year)]
      daily.curt[, time := as.POSIXct(timeformat,format='%j %Y', tz='UTC')] # Add time column
      
      p.daily.type = line_plot(daily.curt, filters=c('scenario','time','Type'), x.col='time', 
                     y.col='Curtailment', y.lab='Curtailment (MWh)', color='scenario', linesize=1.2)
      if (nrow(daily.curt)>30) {
        p.daily.type = p.daily.type + scale_x_datetime(breaks = date_breaks(width = "1 month"), 
                                   labels = date_format("%b"), expand = c(0, 0)) 
      }
      p.daily.type = p.daily.type + facet_wrap(~Type,ncol=1,scales='free') + scale_color_brewer(palette='Set1')
      
      # Calculate diffs
      diff.daily.curt = daily.curt[, .(scenario, Curtailment = Curtailment - Curtailment[as.character(scenario)==ref.scenario]), 
                                   by=.(time,Type)]
      
      p.daily.type.diff = line_plot(diff.daily.curt, filters=c('scenario','time','Type'), x.col='time',
                     y.col='Curtailment', y.lab='Difference in Curtailment (MWh)', color='scenario', linesize=1.2)
      if (nrow(daily.curt)>30) {
        p.daily.type.diff = p.daily.type.diff + scale_x_datetime(breaks = date_breaks(width = "1 month"), 
                                   labels = date_format("%b"), expand = c(0, 0))
      }
      p.daily.type.diff = p.daily.type.diff + facet_wrap(~Type,ncol=1,scales='free') + scale_color_brewer(palette='Set1')

      # Totals
      # Calculate average curtailment for each day 
      daily.curt.tot = daily.curt[,.(Curtailment = mean(Curtailment)),by=.(scenario,time)] 
      
      p.daily = line_plot(daily.curt.tot, filters=c('scenario','time'), x.col='time', 
                     y.col='Curtailment', y.lab='Curtailment (MWh)', color='scenario', linesize=1.2)
      if (nrow(daily.curt.tot)>(30*length(db.loc))) {
        p.daily = p.daily + scale_x_datetime(breaks = date_breaks(width = "1 month"), 
                                   labels = date_format("%b"), expand = c(0, 0))
      }
      p.daily = p.daily + scale_color_brewer(palette='Set1')
      
      # Calculate diffs
      diff.daily.curt = daily.curt[, .(scenario, Curtailment = Curtailment - Curtailment[as.character(scenario)==ref.scenario]), by=.(time)]
      
      p.daily.diff = line_plot(diff.daily.curt, filters=c('scenario','time'), x.col='time', y.col='Curtailment', 
                     y.lab='Difference in Curtailment (MWh)', color='scenario', linesize=1.2)
      if (nrow(daily.curt)>(30*length(db.loc))) {
        p.daily.diff = p.daily.diff + scale_x_datetime(breaks = date_breaks(width = "1 month"), 
                                   labels = date_format("%b"), expand = c(0, 0))
      }
      p.daily.diff = p.daily.diff + scale_color_brewer(palette='Set1')

    }
  }
} else { 
  p.daily.type = 'Section not run according to input file.'
  p.daily.type.diff = 'Section not run according to input file.'
  p.daily = 'Section not run according to input file.'
  p.daily.diff = 'Section not run according to input file.'
}

# Check if this section was selected to run in the input file
if (interval.curtailment) {
  # Check inputs
  if ( typeof(interval.generation) == 'character' ) { 
    p.int.type = 'INPUT ERROR: interval.generation has errors. Cannot run this section.'
    p.int.type.diff = 'INPUT ERROR: interval.generation has errors. Cannot run this section.'
    p.int = 'INPUT ERROR: interval.generation has errors. Cannot run this section.'
    p.int.diff = 'INPUT ERROR: interval.generation has errors. Cannot run this section.'
  } else if ( typeof(interval.avail.cap) == 'character' ) { 
    p.int.type = 'INPUT ERROR: interval.avail.cap has errors. Cannot run this section.'
    p.int.type.diff = 'INPUT ERROR: interval.avail.cap has errors. Cannot run this section.'
    p.int = 'INPUT ERROR: interval.avail.cap has errors. Cannot run this section.'
    p.int.diff = 'INPUT ERROR: interval.avail.cap has errors. Cannot run this section.'
  } else{
    # Query curtailment data
    interval.curt = tryCatch( total_curtailment(interval.generation, interval.avail.cap), error = function(cond) { return('ERROR')})
    # If there is a problem with the query return an error.
    if ( typeof(interval.curt)=='character' ) { 
      p.int.type = 'ERROR: daily_curtailment function not returning correct results.'
      p.int.type.diff = 'ERROR: daily_curtailment function not returning correct results.'
      p.int = 'ERROR: daily_curtailment function not returning correct results.'
      p.int.diff = 'ERROR: daily_curtailment function not returning correct results.'
    } else {
      # Interval Curtailment plots
      # Type plots
      # Sum up the curtailment each interval to get average interval curtailment. Assign an interval to each row.
      avg.curt = interval.curt[,.(Curtailment_GWh=sum(Curtailment)/1000),by=.(scenario,interval,Type)]
      avg.curt[, hour := floor((interval-1)*(3600*24/intervals.per.day)/3600)]
      avg.curt[, minute := floor((interval-1)*(3600*24/intervals.per.day)/60-hour*60)]
      avg.curt[, second := floor((interval-1)*(3600*24/intervals.per.day)-hour*3600-minute*60)]
      avg.curt[, time := as.POSIXct(paste(hour,minute,second, sep=":"),'UTC', format="%H:%M:%S")]
      
      p.int.type = line_plot(avg.curt, filters=c('scenario','time','Type'), x.col='time', y.col='Curtailment_GWh',
                     y.lab='Curtailment (GWh)', color='scenario')
      p.int.type = p.int.type + scale_color_brewer(palette='Set1') +
           scale_x_datetime(breaks = date_breaks(width = "2 hour"), labels = date_format("%H:%M"), 
                            expand = c(0, 0), timezone='UTC')
      p.int.type = p.int.type + facet_wrap(~Type,ncol=1,scales='free')
      
      # Calculate diffs
      diff.curt = avg.curt[, .(scenario, Curtailment_GWh = Curtailment_GWh - Curtailment_GWh[as.character(scenario)==ref.scenario]), by=.(time,Type)]
      
      p.int.type.diff = line_plot(diff.curt, filters=c('scenario','time','Type'), x.col='time', y.col='Curtailment_GWh',
                     y.lab='Difference in Curtailment (GWh)', color='scenario')
      p.int.type.diff = p.int.type.diff + scale_color_brewer(palette='Set1') +
           scale_x_datetime(breaks = date_breaks(width = "2 hour"), labels = date_format("%H:%M"), 
                            expand = c(0, 0), timezone='UTC')
      p.int.type.diff = p.int.type.diff + facet_wrap(~Type,ncol=1,scales='free')

      # Total plots
      # Sum up the curtailment each interval to get average interval curtailment. Assign an interval to each row.
      avg.curt.tot = avg.curt[,.(Curtailment_GWh=sum(Curtailment_GWh)),by=.(scenario,hour,minute,second,time)]
      
      p.int = line_plot(avg.curt.tot, filters=c('scenario','time'), x.col='time', y.col='Curtailment_GWh',
                     y.lab='Curtailment (GWh)', color='scenario')
      p.int = p.int + scale_color_brewer(palette='Set1') +
           scale_x_datetime(breaks = date_breaks(width = "2 hour"), labels = date_format("%H:%M"), 
                            expand = c(0, 0), timezone='UTC')
      p.int = p.int
      
      # Calculate diffs
      diff.curt = avg.curt.tot[, .(scenario, Curtailment_GWh = Curtailment_GWh - Curtailment_GWh[as.character(scenario)==ref.scenario]), by=.(time)]
      
      p.int.diff = line_plot(diff.curt, filters=c('scenario','time'), x.col='time', y.col='Curtailment_GWh',
                     y.lab='Difference in Curtailment (GWh)', color='scenario')
      p.int.diff = p.int.diff + scale_color_brewer(palette='Set1') +
           scale_x_datetime(breaks = date_breaks(width = "2 hour"), labels = date_format("%H:%M"), 
                            expand = c(0, 0), timezone='UTC')
      p.int.diff = p.int.diff
    }
  }
} else { 
  p.int.type = 'Section not run according to input file.'
  p.int.type.diff = 'Section not run according to input file.'
  p.int = 'Section not run according to input file.'
  p.int.diff = 'Section not run according to input file.'
}
