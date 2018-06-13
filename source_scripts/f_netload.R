# Check to make sure relevant data exists
if ( typeof(interval.region.load)=='character' ) {
    print('INPUT ERROR: interval.region.load has errors. Cannot run this section.')
} else if ( typeof(interval.generation) == 'character' ) { 
    print('INPUT ERROR: interval.generation has errors. Cannot run this section.')
} else if ( typeof(interval.avail.cap) == 'character' ) { 
    print('INPUT ERROR: interval.avail.cap has errors. Cannot run this section.')
} else{
    # ###############################################################################
    # Net Load Calculations
    # ###############################################################################

    interval.vg = tryCatch( total_variablegen(interval.generation, interval.avail.cap), error = function(cond) { return('ERROR')})

    if ( interval.zone.load != "ERROR" ) {
        interval.netload = tryCatch( interval_netload(interval.vg, interval.zone.load), error = function(cond) { return('ERROR')})
    }
    else{
        interval.netload = tryCatch( interval_netload(interval.vg, interval.region.load), error = function(cond) { return('ERROR')})
    }
    
    diurnal.netload = tryCatch( diurnal_netload(interval.netload), error = function(cond) { return('ERROR')})
    if( typeof(interval.netload) == 'character'){
        print('INPUT ERROR OF UNKNOWN TYPE')
    }
    else{
        d.nl = diurnal.netload[variable %in% c("Load","VG Potential","VG Output")]
        d.nlv = diurnal.netload[variable %in% c("Net Load","Potential Net Load")]
        d.int = interval.netload[variable %in% c("Load","Net Load")]
        num = nrow(d.int[variable == 'Load' & scenario == unique(d.int[,scenario])[1],])
        setorder(d.int, scenario,variable,-value)
        d.int[,Dummy := c(1:num)]
        d.cur = d.int[variable=='Net Load', c('scenario','time','value','Dummy')]
        d.temp = interval.netload[variable=='Curtailment', c('scenario','time','value')]
        setkey(d.cur,scenario,time)
        setkey(d.temp,scenario,time)
        d.cur = d.cur[d.temp]
        setorder(d.cur,scenario,Dummy)
    
        d.nl[,variable:=factor(variable,levels = c("Load","VG Potential","VG Output"))]
        y.lab = 'MW'
        if(max(d.nl[,value])>10000.0){
            y.lab = 'GW'
            d.nl[,value:=value/1000.0]
            d.nlv[,value:=value/1000.0]
            d.int[,value:=value/1000.0]
            d.cur[,value:=value/1000.0]
        }
    
        p.nl = ggplot(d.nl, aes(x = time, y = value, color = variable)) + geom_line(size = 1.1) + 
            labs(x = NULL, y = y.lab) +
            facet_grid(scenario~Quarter,scales = 'free') +
            scale_x_datetime(breaks = date_breaks(width = '12 hour'), labels = date_format("%H:%M"),
                             expand = c(0,0), timezone = 'UTC') +
            scale_color_brewer("",palette='Dark2') +
            theme(text = element_text(size = 18))
        
        p.nlv = ggplot(d.nlv, aes(x = time, y = value, color = scenario, linetype = variable)) + geom_line(size = 1.1) +
            labs(x = NULL, y = y.lab) +
            facet_grid(~Quarter,scales = 'free') +
            scale_x_datetime(breaks = date_breaks(width = '12 hour'), labels = date_format("%H:%M"),
                             expand = c(0,0), timezone = 'UTC') +
            scale_color_brewer("",palette='Set1') +
            scale_linetype_discrete("") +
            theme(text = element_text(size = 18))
        
        p.dur = ggplot(d.int, aes(x = Dummy, y = value, color = scenario, linetype = variable)) + geom_line(size = 1.1) +
            labs(x = 'Intervals of Year', y = y.lab) +
            facet_wrap(~scenario, ncol = 1,scales = 'free') +
            scale_color_brewer("",palette='Set1') +
            scale_linetype_discrete("") +   
            theme(text = element_text(size = 18))    
    
        p.durbar = ggplot() +
            geom_line(data = d.cur, aes(x = Dummy, y = value, color = scenario, linetype = 'Net Load'), size = 1.1) +
            facet_wrap(~scenario, ncol = 1,scales = 'free') +
            scale_color_brewer("",palette='Set1') +
            scale_linetype_discrete("") +
            geom_bar(data = d.cur, aes(x = Dummy, y = i.value, fill = scenario, size = 'Curtailment'), alpha = 0.5, stat = 'identity') +
            scale_fill_brewer("",palette='Set1') +
            scale_size_discrete("") +
            theme(text = element_text(size = 18)) +
            labs(x = 'Intervals of Year', y = y.lab) 
    }
}