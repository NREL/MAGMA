if ( typeof(interval.vg)=='character' ) {
    print('INPUT ERROR: interval.vg has errors. Cannot run flexibility inventory.')
} else if ( typeof(interval.netload) == 'character' ) { 
    print('INPUT ERROR: interval.netload has errors. Cannot run flexibility inventory.')
} else if (!exists("flex.intervals")) {
    print('INPUT ERROR: flexibilility intervals must be specified.')
} else if ( typeof(interval.generation) == 'character' ) { 
    print('INPUT ERROR: interval.generation has errors. Cannot run flexibility inventory.')
} else if ( typeof(interval.avail.cap) == 'character' ) {
    print('INPUT ERROR: interva.avail.cap has errors. Cannot run flexibility inventory.')
} else{
    ramp = NULL
    ramp_day = NULL
    ramp_day_down = NULL
    rect = NULL    
    flex.demand.timeseries = NULL
    data_interval = difftime(interval.netload$time[2], interval.netload$time[1], units = 'mins')
    ## if a flex.interval is less than the data resolution, just linearly interpolate
    for(i in flex.intervals){
        min_interval = NULL
        if(grepl("h", i)){
            min_interval = as.minute(as.hour(as.numeric(sapply(strsplit(i, " "), "[[", 1))))
        }
        else{
            min_interval = as.minute(as.numeric(sapply(strsplit(i, " "), "[[", 1)))
        }

        if(min_interval < as.double(data_interval)){
            print(sprintf('flexibility inventory is smaller than data interval....linearly interpolating for %s. NOT RECOMMENDED!', i))
            setkey(interval.netload,scenario,variable)
            expand.interval.netload = interval.netload[, expand_timeseries(.SD, min_interval), by = .(scenario,variable)]
        }
        else{
            expand.interval.netload = copy(interval.netload)
        }
        ## how many times does the data go into the flex interval?
        setkey(expand.interval.netload,scenario,variable,time)
        new_data_interval = difftime(expand.interval.netload$time[2], expand.interval.netload$time[1], units = 'mins')

        if(min_interval %% as.double(new_data_interval) != 0 ){
            print('INPUT ERROR: flex intervals must be evenly divisible by data intervals')
            break
        }
        ntimes = min_interval / as.double(new_data_interval)
        ## Next, determine ramps
        expand.interval.netload[, dif:= c(diff(value, lag = ntimes),rep(NA,times = ntimes)), by = .(scenario,variable)]

        xlab = sprintf("MW change in %s Interval",i)
        p <- ggplot(expand.interval.netload[variable %in% c("VG Potential","Load","Potential Net Load")], aes(x = dif, fill = scenario)) + geom_histogram(bins = 30) + facet_grid(variable~scenario,scales = "free_y") +
            scale_fill_brewer("",palette='Set1') +
            labs(x = xlab, y = "# of Intervals in Bin") +
            guides(fill = FALSE) +
            theme(text = element_text(size = 18),
                  strip.text.y = element_text(size = 14))

        i_name = gsub(" ","",i)
        assign(sprintf("p%s",i_name),p)
        
        ## Ignore everything except Net Load for a Second (or should it be Potential Net Load?)
        netload.statistics.up = expand.interval.netload[variable=='Potential Net Load'& dif > 0,
                                                        .(`Average NL Ramp Up` = round(mean(dif,na.rm = TRUE),1),
                                                          `Stand Dev NL Ramp Up` = round(sd(dif,na.rm = TRUE),1),
                                                          `Max Ramp Up` = round(max(dif,na.rm = TRUE),1),
                                                          `Max Ramp Up Date` = as.character(time[which(dif == max(dif,na.rm = TRUE))[1]]),
                                                          `3 Stand Dev NL Up` = round(sd(dif,na.rm = TRUE) * 3.0,1)),
                                                        by = .(scenario)]
        
        netload.statistics.down = expand.interval.netload[variable=='Potential Net Load' & dif < 0,
                                                               .(`Average NL Ramp Down` = round(mean(dif,na.rm = TRUE),1),
                                                                 `Stand Dev NL Ramp Down` = round(sd(dif,na.rm = TRUE),1),
                                                                 `Max Ramp Down` = round(min(dif,na.rm = TRUE),1),
                                                                 `Max Ramp Down Date` = as.character(time[which(dif == min(dif,na.rm = TRUE))[1]]),
                                                                 `3 Stand Dev NL Down` = round(sd(dif,na.rm = TRUE) * 3.0,1)),
                                                               by = .(scenario)]
        
        netload.statistics.up <- dcast(melt(netload.statistics.up, id = "scenario"),variable~scenario,value.var = 'value')
        netload.statistics.down <- dcast(melt(netload.statistics.down, id = "scenario"),variable~scenario,value.var = 'value')
        
        assign(sprintf("up_%s",i_name),netload.statistics.up)
        assign(sprintf("down_%s",i_name),netload.statistics.down)
        
        nl = expand.interval.netload[variable=='Potential Net Load' & !is.na(dif),]
        setorder(nl,scenario,Quarter,-dif)
        n_row = nrow(nl)/length(unique(nl$scenario))
        nl[,Dummy:=c(1:n_row),.(scenario,Quarter)]

        p <- ggplot(nl,aes(x = Dummy, y = dif, color = Quarter)) + geom_point() + facet_grid(scenario~Quarter) +
            scale_color_manual("", values = c("#a50026","#fdae61","#abd9e9","#313695"),labels= c("Spring","Summer","Fall","Winter")) +
            guides(color = FALSE) +
            labs(y = xlab, x = "# of Intervals") +
            theme(text = element_text(size = 18))
        
        assign(sprintf("pp%s",i_name),p)
        
        geos = unique(gen.geo.mapping$name)
        if(focus){
          if( !is.na(focus.region[1])){
            geos = gen.geo.mapping[name %in% gen.geo.mapping[Region %in% focus.region,name],name]
          }
          if( !is.na(focus.zone[1])){
            geos = gen.geo.mapping[name %in% gen.geo.mapping[Zone %in% focus.zone,name],name]
          }
        }

        ## subsetting generation/capacity based on max ramp in flexibility interval
        for(j in c(1:length(db.loc))){
            time_up <- seq(from = as.POSIXct(netload.statistics.up[[j+1]][4], tz = 'UTC'), by=new_data_interval, length.out = ntimes+1)
            time_down <- seq(from = as.POSIXct(netload.statistics.down[[j+1]][4], tz = 'UTC'), by=new_data_interval, length.out = ntimes+1)
            time_up_extended = NULL
            time_down_extended = NULL
            if(grepl("h",i)){
                hh = strsplit(i," ")[[1]][1]
                if(as.numeric(hh) <= 9){
                    time_up_extended  <- seq(from = as.POSIXct(strsplit((netload.statistics.up[[j+1]][4])," ")[[1]][1], tz = 'UTC'),
                                             by = new_data_interval, length.out = (24*60)/as.double(new_data_interval))
                    time_down_extended <- seq(from = as.POSIXct(strsplit((netload.statistics.down[[j+1]][4]), " ")[[1]][1], tz = 'UTC'),
                                              by = new_data_interval, length.out = (24*60)/as.double(new_data_interval))
                }
                else if(as.numeric(hh) > 9 & as.numeric(hh) < 25){
                    time_up_extended <- seq(from = as.POSIXct(strsplit((netload.statistics.up[[j+1]][4])," ")[[1]][1], tz = 'UTC'),
                                            by = new_data_interval, length.out = (48*60)/as.double(new_data_interval))
                    time_down_extended <- seq(from = as.POSIXct(strsplit((netload.statistics.down[[j+1]][4]), " ")[[1]][1], tz = 'UTC'),
                                              by = new_data_interval, length.out = (48*60)/as.double(new_data_interval))
                }
                else{
                    time_up_extended <- seq(from = as.POSIXct(strsplit((netload.statistics.up[[j+1]][4])," ")[[1]][1], tz = 'UTC'),
                                            by = new_data_interval, length.out = (72*60)/as.double(new_data_interval))
                    time_down_extended <- seq(from = as.POSIXct(strsplit((netload.statistics.down[[j+1]][4]), " ")[[1]][1], tz = 'UTC'),
                                              by = new_data_interval, length.out = (72*60)/as.double(new_data_interval))
                }
            }
            else{
                time_up_extended  <- seq(from = as.POSIXct(strsplit((netload.statistics.up[[j+1]][4])," ")[[1]][1], tz = 'UTC'),
                                         by = new_data_interval, length.out = (24*60)/as.double(new_data_interval))
                time_down_extended  <- seq(from = as.POSIXct(strsplit((netload.statistics.down[[j+1]][4])," ")[[1]][1], tz = 'UTC'),
                                         by = new_data_interval, length.out = (24*60)/as.double(new_data_interval))
            }
            generation <- interval.generation[scenario == names(netload.statistics.up)[j+1] & time %in% time_up & name %in% geos,]
            capacity   <- interval.avail.cap[scenario == names(netload.statistics.up)[j+1]& time %in% time_up & name %in% geos,]
            curtail        <- rbind(generation[(Type %in% re.types),],capacity[(Type %in% re.types),])
            
            generation <- generation[!(Type %in% re.types),]
            generation[,Commitment:=ifelse(value>0,1,0)]
            generation[, DiffGen:= c(diff(value, lag = ntimes),rep(NA,times = ntimes)), by = .(name)]
            generation[, DiffCommit:= c(diff(Commitment, lag = ntimes),rep(NA,times = ntimes)), by = .(name)]
            
            commit  = generation[DiffCommit!=0,.(total=sum(DiffGen,na.rm=TRUE),method = "Commit",ramp = "Up", interval = i),by = .(scenario,Type)]
            dispatch= generation[DiffCommit==0,.(total=sum(DiffGen,na.rm=TRUE),method = "Dispatch",ramp = "Up", interval = i),by = .(scenario,Type)]
            total = rbind(commit,dispatch)

            #sometimes, there's no curtailment
            if(nrow(curtail)>0){
              curtail = dcast.data.table(curtail,scenario+time~property,value.var = "value", fun.aggregate = sum)
              curtail[,Curtailment:=`Available Capacity` - Generation]
              curtail[,DiffCurtail:=c((diff(Curtailment, lag = ntimes)*-1.0),rep(NA,times=ntimes)), by = .(scenario)]
              
              #only include curtailment if it's "helping" with ramp, not "hurting"
              curtail=curtail[,.(total=sum(DiffCurtail,na.rm=TRUE),method = "Commit",ramp = "Up", interval=i, Type = "Curtailment"), by = .(scenario)]
              
              if(curtail$total>0){
                total = rbind(total,curtail)
              }
            }
            
            if(!is.null(ramp)){
                ramp = rbind(ramp,total)             
            }
            else{
                ramp = total
            }
            time_down <- seq(from = as.POSIXct(netload.statistics.down[[j+1]][4], tz = 'UTC'), by=new_data_interval, length.out = ntimes+1) 
            generation <- interval.generation[scenario == names(netload.statistics.up)[j+1] & time %in% time_down & name %in% geos,]
            capacity   <- interval.avail.cap[scenario == names(netload.statistics.up)[j+1] & time %in% time_down & name %in% geos,]
            curtail        <- rbind(generation[(Type %in% re.types),],capacity[(Type %in% re.types),])
            
            generation <- generation[!(Type %in% re.types),]
            generation[,Commitment:=ifelse(value>0,1,0)]
            generation[, DiffGen:= c(diff(value, lag = ntimes),rep(NA,times = ntimes)), by = .(name)]
            generation[, DiffCommit:= c((diff(Commitment, lag = ntimes)),rep(NA,times = ntimes)), by = .(name)]
            
            commit  = generation[DiffCommit!=0,.(total=sum(DiffGen,na.rm=TRUE),method = "Commit",ramp = "Down",interval = i),by = .(scenario,Type)]
            dispatch= generation[DiffCommit==0,.(total=sum(DiffGen,na.rm=TRUE),method = "Dispatch",ramp = "Down",interval = i),by = .(scenario,Type)]
            total = rbind(commit,dispatch)
            if(nrow(curtail)>0){
              curtail = dcast.data.table(curtail,scenario+time~property,value.var = "value", fun.aggregate = sum)
              curtail[,Curtailment:=`Available Capacity` - Generation]
              curtail[,DiffCurtail:=c((diff(Curtailment, lag = ntimes)*-1.0),rep(NA,times=ntimes)), by = .(scenario)]
              
              curtail=curtail[,.(total=sum(DiffCurtail,na.rm=TRUE),method = "Commit",ramp = "Down", interval=i, Type = "Curtailment"), by = .(scenario)]
              #only include curtailment if it's "helping" with ramp, not "hurting"
              if(curtail$total<0){
                  total = rbind(total,curtail)
              }   
            }
            ramp = rbind(ramp,total)
                        #capacity <- interval.avail.cap[scenario == unique(interval.generation$scenario)[j] & time %in% time_up,]
            addme = expand.interval.netload[scenario == names(netload.statistics.up)[j+1] & time %in% time_up_extended,]
            addme[,interval := i]
            addme_down = expand.interval.netload[scenario == names(netload.statistics.up)[j+1] & time %in% time_down_extended,]
            addme_down[,interval := i]
            
            if(!is.null(ramp_day)){
                ramp_day = rbind(ramp_day, addme)
                rect = rbind(rect, data.table(start = min(time_up), end = max(time_up), scenario = names(netload.statistics.up)[j+1], interval=i))
            }
            else{
                ramp_day = addme
                rect = data.table(start = min(time_up), end = max(time_up), scenario = names(netload.statistics.up)[j+1], interval = i)
            }
            if(!is.null(ramp_day_down)){
                ramp_day_down = rbind(ramp_day_down, addme_down)
                rect_down = rbind(rect_down, data.table(start = min(time_down), end = max(time_down),scenario = names(netload.statistics.up)[j+1], interval = i))
            }
            else{
                ramp_day_down = addme_down
                rect_down = data.table(start = min(time_down), end = max(time_down), scenario = names(netload.statistics.up)[j+1], interval = i)
            }
        }
        expand.interval.netload[,i:=i]
        expand.interval.netload[,DemandUp:=ifelse(dif>0,dif,0)]
        expand.interval.netload[,DemandDown:=ifelse(dif<0,dif,0)]
        expand.interval.netload[,dif:=NULL]
        temp=expand.interval.netload[variable=='Potential Net Load',]
        temp[,variable:=NULL]
        if(!is.null(flex.demand.timeseries)){
            flex.demand.timeseries = rbind(flex.demand.timeseries,temp)
        }
        if(is.null(flex.demand.timeseries)){
            flex.demand.timeseries = temp
        }
    }
    ## problem is above this note
    ramp[,interval:=factor(interval,levels = flex.intervals)]
    ramp[,Type:=factor(Type,levels = gen.order)]
    ramp_day[,interval:=factor(interval,levels = flex.intervals)]
    rect[,interval:=factor(interval,levels=flex.intervals)]
    ramp_day_down[,interval:=factor(interval,levels = flex.intervals)]
    rect_down[,interval:=factor(interval, levels =flex.intervals)]
    for(j in c(1:length(db.loc))){
        t = ramp[scenario == unique(interval.generation$scenario)[j],]
        p <- ggplot() +
            geom_bar(data = t[Type!='Potential Net Load'& total > 0,], aes(x = interval, y = total, fill = Type, alpha = method, order = -as.numeric(Type)), stat = "identity") + 
            geom_bar(data = t[Type!='Potential Net Load'& total < 0,], aes(x = interval, y = total, fill = Type, alpha = method, order = -as.numeric(Type)), stat = "identity") +
            scale_fill_manual("",values = gen.color) +
            scale_alpha_manual("",values = c(1,0.5),labels = c("Commit","Dispatch") ) +
            labs(x = NULL, y = "Flexibility supply (MW)") +
            theme(text = element_text(size = 18))
        
        assign(sprintf("p_%s",unique(interval.generation$scenario)[j]),p)
        
        r = ramp_day[scenario == unique(interval.generation$scenario)[j],]
        rr= rect[scenario == unique(interval.generation$scenario)[j],]
        p = ggplot() +
            geom_line(data = r[(variable %in% c("Load","Net Load","VG Output","VG Potential")),],aes(x = time, y= value, color = variable),size = 1.1) + 
            geom_rect(data = rr, inherit.aes = FALSE, aes(xmin = start, xmax = end, ymin = pmin(0,min(ramp_day$value)), ymax = max(ramp_day$value)),fill = 'orange', alpha = 0.3) +
            scale_color_brewer("",palette = "Set1") +
            ylim(NA, max(ramp_day$value)) +
            labs(x = NULL, y = "MW") +
            theme(text = element_text(size = 18)) +
            facet_grid(.~interval,scales = "free", space = "free_x") +
            scale_x_datetime(breaks = date_breaks(width = '24 hour'), labels = date_format("%m-%d\n%H:%M"),
                             expand = c(0,0), timezone = 'UTC') 
        assign(sprintf("p_rect_%s",unique(interval.generation$scenario)[j]),p)
        
        r = ramp_day_down[scenario == unique(interval.generation$scenario)[j],]
        rr= rect_down[scenario == unique(interval.generation$scenario)[j],]
        p = ggplot() +
            geom_line(data = r[(variable %in% c("Load","Net Load","VG Output","VG Potential")),],aes(x = time, y= value, color = variable),size = 1.1) + 
            geom_rect(data = rr, inherit.aes = FALSE, aes(xmin = start, xmax = end, ymin = pmin(0,min(ramp_day_down$value)), ymax = max(ramp_day_down$value)),fill = 'orange', alpha = 0.3) +
            scale_color_brewer("",palette = "Set1") +
            ylim(NA, max(ramp_day_down$value)) +
            labs(x = NULL, y = "MW") +
            theme(text = element_text(size = 18)) +
            facet_grid(.~interval,scales = "free", space = "free") +
            scale_x_datetime(breaks = date_breaks(width = '24 hour'), labels = date_format("%m-%d\n%H:%M"),
                             expand = c(0,0), timezone = 'UTC') 
        assign(sprintf("p_rect_down_%s",unique(interval.generation$scenario)[j]),p)
        
    }
}