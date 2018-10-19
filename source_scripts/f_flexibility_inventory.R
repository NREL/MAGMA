if ( typeof(interval.generation)=='character' ) {
    print('INPUT ERROR: interval.generation has errors. Cannot run flexibility inventory.')
} else if ( typeof(interval.avail.cap) == 'character' ) { 
    print('INPUT ERROR: interval.avail.cap has errors. Cannot run flexibility inventory.')
} else if (!exists("gen.property.mapping")) {
    print('INPUT ERROR: generator.property.mapping characteristics must be specified.')
} else{
    ## create a table
    geos = unique(gen.geo.mapping$name)
    if(focus){
      if( !is.na(focus.region[1])){
        geos = gen.geo.mapping[name %in% gen.geo.mapping[Region %in% focus.region,name],name]
      }
      if( !is.na(focus.zone[1])){
        geos = gen.geo.mapping[name %in% gen.geo.mapping[Zone %in% focus.zone,name],name]
        interval.region.gen = interval.zonal.gen
        interval.region.load = interval.zonal.load
      }
      interval.imports = tryCatch( interval_imports(interval.region.gen, interval.region.load), error = function(cond) { return('ERROR')})
      
      if( "Imports" %in% gen.property.mapping[,name]){
        interval.imports = interval.imports[,name:="Imports"]
        interval.imports = interval.imports[,Type:="Imports"]
        interval.imports = interval.imports[,Generation:=Imports]
        interval.imports = interval.imports[,`Available Capacity`:=gen.property.mapping[name=="Imports",Max.Capacity]]
        interval.imports[, c("Load","Imports"):=NULL]
      }
      else{
        print('WARNING: "Imports" not specified as generator in generator CSV. Infinite Flexibility assumed!!')
        interval.imports = interval.imports[,name:="Imports"]
        interval.imports = interval.imports[,Type:="Imports"]
        interval.imports = interval.imports[,Generation:=Imports]
        interval.imports = interval.imports[,`Available Capacity`:=max(Generation)]
        interval.imports[, c("Load","Imports"):=NULL]
        if(sum(interval.imports$Generation)>1000){
          gen.property.mapping = rbind(gen.property.mapping, data.table(name='Imports',Max.Capacity=max(interval.imports$`Available Capacity`),
                                                                        Min.Stable.Level=min(interval.imports$Generation),Max.Ramp.Up=100000,Max.Ramp.Down=100000,
                                                                        Min.Up.Time=0,Min.Down.Time=0,Must.Run=0))
        }
        setkey(gen.property.mapping,name)
      }
      
    }
    ## Must name your storage "Storage","Batteries","Battery","PHES","PHS","PSH","Stor"
    stor_names = c("Storage","Batteries","Battery","PHES","PHS","PSH","Stor","Shadow")
    if(any(stor_names %in% unique(interval.generation[,Type]))){
      storage = interval.generation[Type %in% stor_names,]
      interval.generation = interval.generation[!(Type %in% stor_names),]
      setkey(storage,scenario,time,name)
      setkey(interval.pump.load,scenario,time,name)
      storage = storage[interval.pump.load]
      if(nrow(storage)!=nrow(interval.pump.load)){
        print('possible discrepancy in storage - pump load and generation might not match')
      }
      storage[,value:=value-i.value]
      storage = storage[,.(scenario,property,name,value,time,category,Type)]
      interval.generation = rbind(interval.generation,storage)
    }

    d.interval.generation = dcast.data.table(interval.generation[name %in% geos,],scenario+time+name+Type ~ property,value.var = "value")
    setkey(d.interval.generation, scenario, name, Type, time)
    d.interval.avail.cap  = dcast.data.table(interval.avail.cap[name %in% geos,],scenario+time+name+Type ~ property,value.var = "value")
    setkey(d.interval.avail.cap, scenario, name, Type, time)
    
    master.gen = d.interval.generation[d.interval.avail.cap]

    if(focus){
      if(sum(interval.imports$Generation)>1000){
        master.gen=rbind(master.gen,interval.imports)
        setkey(master.gen, scenario, name, Type, time)
      }
    }
    
    if(exists("exclude.dates")){
      for(i in length(exclude.dates)){
        master.gen = master.gen[!grepl(exclude.dates[i], time), ]  
      }
    }
    
    # if reserves are considered
    if( typeof(interval.gen.reserve.provision) != 'character'){
        # remove eligible reserves
        if(!is.na(flex.reserves)){
            d.interval.reserve.provision = interval.gen.reserve.provision[!(parent %in% flex.reserves) & name %in% geos,]
            if(nrow(d.interval.reserve.provision)>0){
              d.interval.reserve.provision = dcast.data.table(d.interval.reserve.provision, scenario + time + name + Type ~ property,value.var = "value", fun.aggregate = sum)
            }
            else{
              master.gen[,Provision:=0]
            }
        }
        else{
            d.interval.reserve.provision = interval.gen.reserve.provision[name %in% geos,]
            if(nrow(d.interval.reserve.provision)>0){
              d.interval.reserve.provision = dcast.data.table(d.interval.reserve.provision, scenario + time + name + Type ~ property,value.var = "value", fun.aggregate = sum)
            }
            else{
              master.gen[,Provision:=0]
            }
        }
      
        if(exists("exclude.dates")){
          for(i in length(exclude.dates)){
            d.interval.reserve.provision = d.interval.reserve.provision[!grepl(exclude.dates[i], time), ]  
          }
        }
      
        setkey(d.interval.reserve.provision, scenario, name, Type, time)
        master.gen = d.interval.reserve.provision[master.gen]
        master.gen[is.na(Provision),Provision:=0]
    }
    else{
      master.gen[,Provision:=0]
    }

    # if unserved energy
    if( typeof(interval.region.ue) != 'character'){
        interval.ue = interval.region.ue[,.(Type = property, FlexibilityUp = sum(value), FlexibilityDown = 0),by = .(scenario,time)]
        if(!is.na(focus.region[1])){
          interval.ue = interval.region.ue[name %in% focus.region,.(Type='Unserved Energy',FlexibilityUp=sum(value),FlexibilityDown=0, Capacity = 0),by=.(scenario,time)]          
        }
        if(exists("exclude.dates")){
          for(i in length(exclude.dates)){
            interval.ue = interval.ue[!grepl(exclude.dates[i], time), ]  
          }
        }
    }
    else{
        interval.ue = interval.zone.ue[,.(Type = property, FlexibilityUp = sum(value), FlexibilityDown = 0), by = .(scenario,time)]
        if(!is.na(focus.zone[1])){
          interval.ue = interval.zone.ue[name %in% focus.zone,.(Type = 'Unserved Energy', FlexibilityUp = sum(value), FlexibilityDown = 0, Capacity = 0), by = .(scenario,time)]
        }
        if(exists("exclude.dates")){
          for(i in length(exclude.dates)){
            interval.ue = interval.ue[!grepl(exclude.dates[i], time), ]  
          }
        }
    }
    
    setkey(master.gen, name)
    master.gen = gen.property.mapping[master.gen]
    master.gen[,Max.Capacity:=ifelse(Type %in% re.types,0,Max.Capacity)]
    
    flex.inventory.timeseries = NULL
    important.periods.timeseries = NULL
    
    data_interval = difftime(master.gen$time[2], master.gen$time[1], units = 'mins')
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
            setkey(master.gen,scenario,name,Type)
            meltme = names(master.gen)[which(!names(master.gen) %in% c("Generation","Available Capacity","Provision"))]
            master.gen = melt.data.table(master.gen, id = meltme)
            setkeyv(master.gen,meltme)
            meltme = c(meltme,"variable")
            meltme = meltme[!(meltme %in% c("time","value"))]
            master.gen = master.gen[, expand_timeseries(.SD, min_interval), by = eval(meltme)]
            master.gen = dcast.data.table(master.gen, ... ~ variable,value.var ="value")
        }
        
        setkey(master.gen, scenario, name, Type, time)
        new_data_interval = difftime(master.gen$time[2], master.gen$time[1], units = 'mins')
        
        if(min_interval %% as.double(new_data_interval) != 0 ){
            print('INPUT ERROR: flex intervals must be evenly divisible by data intervals')
            break
        }
        ntimes = min_interval / as.double(new_data_interval)
        
        temp = copy(master.gen)
        
        temp[,Commit:=ifelse(Generation>0,1,0)]
        ## converting up time/down time from hours to data_interval
        temp[,Min.Up.Time:=as.double(as.hour(Min.Up.Time)/as.hour(as.minute(as.double(new_data_interval))))]
        temp[,Min.Down.Time:=as.double(as.hour(Min.Down.Time)/as.hour(as.minute(as.double(new_data_interval))))]
        ## setting Min.Up.Time to infinity if unit is Must RUn
        temp[Must.Run==1,Min.Up.Time:=1e8]
        ## converting ramp up/down from MW/min to MW/data_interval
        temp[,Max.Ramp.Up  := min_interval * Max.Ramp.Up ]
        temp[,Max.Ramp.Down:= min_interval * Max.Ramp.Down]
        
        temp[, Intervals.At.Status := seq_len(.N), by = rleid(scenario,name,Commit)]
        temp[, RE.Ramp.Down:= c(`Available Capacity`[-(1:ntimes)], rep(0,times = ntimes)), by = .(scenario,name)]
        
        ## only here for KZ!!
        inflexiblegens = c("Akmola_SUBCRITICAL-COAL_180_15","Akmola_SUBCRITICAL-COAL_22_13","Akmola_SUBCRITICAL-COAL_480_14","Aktobe_CCGT-GAS_37_5","Aktobe_CE-GAS_2.9_7","Aktobe_CE-GAS_38.92_6","Aktobe_CE-GAS_6.2_8",
        "Aktobe_CT-GAS_152_2","Aktobe_CT-GAS_33.8_3","Aktobe_CT-GAS_97.8_4","Aktobe_GAS-BOILER_118_1","Almaty_GAS-BOILER_145_17","Almaty_SUBCRITICAL-COAL_173_19","Almaty_SUBCRITICAL-COAL_24_23","Almaty_SUBCRITICAL-COAL_510_18",
        "CENTRAL_ASIA","East-Kazakhstan_SUBCRITICAL-COAL_18_42","East-Kazakhstan_SUBCRITICAL-COAL_252.5_38","East-Kazakhstan_SUBCRITICAL-COAL_59_40","East-Kazakhstan_SUBCRITICAL-COAL_75_39","Karaganda_CT-GAS_103_70",
        "Karaganda_CT-GAS_87_71","Karaganda_SUBCRITICAL-COAL_115_68","Karaganda_SUBCRITICAL-COAL_132_64","Karaganda_SUBCRITICAL-COAL_152_69","Karaganda_SUBCRITICAL-COAL_18_72","Karaganda_SUBCRITICAL-COAL_32_66","Karaganda_SUBCRITICAL-COAL_435_65",
        "Karaganda_SUBCRITICAL-COAL_560_67","Kizilorda_CT-Gas_46_77","Kizilorda_GAS-BOILER_67_78","Kostanai_GAS-BOILER_12_54","Kostanai_SUBCRITICAL-COAL_267_53","North-Kazakhstan_SUBCRITICAL-COAL_434_16","Pavlodar_SUBCRITICAL-COAL_110_59",
        "Pavlodar_SUBCRITICAL-COAL_12_61","Pavlodar_SUBCRITICAL-COAL_350_58","Pavlodar_SUBCRITICAL-COAL_520_60","RUSSIA_VOLGA","South-Kazakhstan_SUBCRITICAL-COAL_12.5_76","South-Kazakhstan_SUBCRITICAL-COAL_160_74","Zhambil_GAS-BOILER_60_75")
        temp[(name %in% inflexiblegens) & (scenario=="2500MW-RE"),Max.Ramp.Up:= 0]
        temp[(name %in% inflexiblegens) & (scenario=="2500MW-RE"),Max.Ramp.Down:=0]
        temp[(name %in% inflexiblegens) & (scenario=="2500MW-RE"),Min.Up.Time:=10000000]
        temp[(name %in% inflexiblegens) & (scenario=="2500MW-RE"),Min.Down.Time:=10000000]
        ## only here for KZ!!
        
        ## Logic: If it's already on, its upward flexibility is limited by either remaining capacity or ramp up
        ## if it's off, it's limited by min down time. if down time is not binding, it's limited by either available capacity or max ramp up
        ## if down time is binding within whole flexibility interval, flexibilityup = 0
        ## if it can turn on x time into the flexibility interval, then it's limited by available capacity or max ramp up in that more limited frame
        temp[, FlexibilityUp:=ifelse(Type %in% re.types,pmax(0,`Available Capacity`),
                                     ifelse(Commit==1, pmin(`Available Capacity` - Generation - Provision, Max.Ramp.Up), 
                                        ifelse((Intervals.At.Status - 1) >= Min.Down.Time, pmin(Max.Capacity,ifelse(Max.Ramp.Up<Min.Stable.Level,0,Max.Ramp.Up)), 
                                            ifelse((Intervals.At.Status - 1 + ntimes) > Min.Down.Time, pmin(Max.Capacity, ifelse(Max.Ramp.Up * (1 - ((Min.Down.Time - Intervals.At.Status + 1)/ntimes))<Min.Stable.Level,0,Max.Ramp.Up * (1 - ((Min.Down.Time - Intervals.At.Status + 1)/ntimes)))), 
                                                   0))))]
        
        ## Logic: If it's already off, it can't move downward (will have to change for storage)
        ## If it's on, it can either turn off (if min.up.time allows) or turn down.
        temp[, FlexibilityDown:=ifelse(Type %in% re.types, pmax(0,RE.Ramp.Down),
                                       ifelse(Commit==0, 0, 
                                            ifelse((Intervals.At.Status - 1) >= Min.Up.Time, ifelse(Max.Ramp.Down > Generation, Generation, pmin(pmax(Generation - Min.Stable.Level,0), Max.Ramp.Down)),
                                                ifelse((Intervals.At.Status - 1 + ntimes) > Min.Up.Time, ifelse(Max.Ramp.Down * (1 - (Min.Up.Time - Intervals.At.Status + 1)/ntimes) > Generation, Generation, pmin(pmax(Generation - Min.Stable.Level,0), Max.Ramp.Down * (1 - (Min.Up.Time - Intervals.At.Status + 1)/ntimes))), 
                                                   pmin(pmax(Generation - Min.Stable.Level,0), Max.Ramp.Down)))))]
        ## special cases
        ## special case #1: Imports
        if("Imports"%in%unique(gen.property.mapping[,name])){
          temp[name=="Imports",FlexibilityUp:=ifelse((Intervals.At.Status - 1) >= Min.Down.Time, pmin(Max.Ramp.Up,pmax(Max.Capacity-Generation,0)),
                                                     ifelse((Intervals.At.Status - 1 + ntimes) > Min.Down.Time, pmin(pmax(Max.Capacity-Generation,0), ifelse(Max.Ramp.Up * (1 - ((Min.Down.Time - Intervals.At.Status + 1)/ntimes))<Min.Stable.Level,0,Max.Ramp.Up * (1 - ((Min.Down.Time - Intervals.At.Status + 1)/ntimes)))), 
                                                            0))]
          temp[name=="Imports", FlexibilityDown:=ifelse((Intervals.At.Status - 1) >= Min.Up.Time, pmin(Max.Ramp.Down,pmax(Generation-Min.Stable.Level,0)),
                                                       ifelse((Intervals.At.Status - 1 + ntimes) > Min.Up.Time, ifelse(Max.Ramp.Down * (1 - (Min.Up.Time - Intervals.At.Status + 1)/ntimes) > Generation, pmax(Generation-Min.Stable.Level,0), pmin(pmax(Generation - Min.Stable.Level,0), Max.Ramp.Down * (1 - (Min.Up.Time - Intervals.At.Status + 1)/ntimes))), 
                                                              pmin(pmax(Generation - Min.Stable.Level,0), Max.Ramp.Down)))]
        }
        ## special case #2: Storage
        if(any(stor_names%in%unique(temp[,Type]))){
          temp=temp[!(is.na(temp$Generation)),]
          temp[Type %in% stor_names,FlexibilityUp:=ifelse((Intervals.At.Status - 1) >= Min.Down.Time, pmin(Max.Ramp.Up,Max.Capacity-Generation-Provision),
                                                     ifelse((Intervals.At.Status - 1 + ntimes) > Min.Down.Time, pmin(Max.Capacity-Generation-Provision, ifelse(Max.Ramp.Up * (1 - ((Min.Down.Time - Intervals.At.Status + 1)/ntimes))<Min.Stable.Level,0,Max.Ramp.Up * (1 - ((Min.Down.Time - Intervals.At.Status + 1)/ntimes)))), 
                                                            0))]
          temp[Type %in% stor_names, FlexibilityDown:=ifelse((Intervals.At.Status - 1) >= Min.Up.Time, pmin(Max.Ramp.Down,pmax(Generation+Max.Capacity,0)),
                                                        ifelse((Intervals.At.Status - 1 + ntimes) > Min.Up.Time, ifelse(Max.Ramp.Down * (1 - (Min.Up.Time - Intervals.At.Status + 1)/ntimes) > Generation,pmax(Generation+Max.Capacity,0), pmin(pmax(Generation +Max.Capacity,0), Max.Ramp.Down * (1 - (Min.Up.Time - Intervals.At.Status + 1)/ntimes))), 
                                                               pmin(pmax(Generation + Max.Capacity,0), Max.Ramp.Down)))]
          
        }
        ## timeseries of interesting periods
        
        ## Add biggest ramp up, biggest ramp down, interesting periods
        for(q in scenario.names){
            rup = temp[(time %in% unique(ramp_day[scenario ==q & interval == i,time])) & scenario == q,]
            rup[,id:='Biggest Ramp Up']
            rup[,i:=i,]
            rdown = temp[(time %in% unique(ramp_day_down[scenario == q & interval == i,time])) & scenario == q,]
            rdown[,id:='Biggest Ramp Down']
            rdown[,i:=i,]
            if(!is.null(important.periods.timeseries)){
                important.periods.timeseries = rbind(important.periods.timeseries,rup,rdown)
            }
            if(is.null(important.periods.timeseries)){
                important.periods.timeseries = rbind(rup,rdown)
            }
        }
        
        temp=temp[,.(FlexibilityUp=sum(FlexibilityUp),FlexibilityDown=sum(FlexibilityDown),Capacity=sum(`Available Capacity`)), by = .(scenario,time,Type)]
        if(sum(interval.ue$FlexibilityUp) > 1){
          temp=rbind(temp,interval.ue)
          gen.color2 = c(gen.color, 'Unserved Energy' = "gray80")
        }
        else{
          gen.color2 = gen.color
        }
        temp=divide_quarterly(temp)
        temp[,i := i]
        if(!is.null(flex.inventory.timeseries)){
            flex.inventory.timeseries = rbind(flex.inventory.timeseries,temp)
        }
        if(is.null(flex.inventory.timeseries)){
            flex.inventory.timeseries = temp
        }

    }
    flex.total.timeseries = flex.inventory.timeseries[,.(FlexibilityUp=sum(FlexibilityUp),FlexibilityDown=sum(FlexibilityDown)),by = .(scenario,time,i,Interval,Day,Month,Quarter)]
    setkey(flex.total.timeseries,scenario,i,time,Interval,Day,Month,Quarter)
    setkey(flex.demand.timeseries,scenario,i,time,Interval,Day,Month,Quarter)
    master.flex = flex.demand.timeseries[flex.total.timeseries]
    
    ## WHEN IS LOWEST FLEX INVENTORY?
    master.flex[,DemandDown:=DemandDown*-1.0,]
    master.flex[,InventoryUp:= FlexibilityUp-DemandUp]
    master.flex[,InventoryDown:=FlexibilityDown-DemandDown]
    
    master.flex.summary <- master.flex[,.(InventoryUp=mean(InventoryUp,na.rm = TRUE),InventoryDown=mean(InventoryDown, na.rm= TRUE)), by = .(scenario,i,Interval,Quarter)]
    setkey(master.flex.summary,scenario,i,Interval,Quarter)
    
    flex.inventory.final.results = master.flex[,.(InventoryUp = min(InventoryUp, na.rm = TRUE),
                                                  InventoryDown = min(InventoryDown, na.rm = TRUE), 
                                                  TimeUp = time[which(InventoryUp == min(InventoryUp, na.rm = TRUE))], 
                                                  TimeDown = time[which(InventoryDown == min(InventoryDown, na.rm = TRUE))]), by = .(scenario,i)]
    setnames(flex.inventory.final.results,c("Scenario","Flexibility Interval","Inventory Up (MW)","Inventory Down (MW)","Time Up","Time Down"))
    
    for(int in flex.intervals){
        i.name = gsub(" ","",int)
        num = nrow(master.flex[i == unique(master.flex[,i])[1] & scenario == unique(master.flex[,scenario])[1],])
        setorder(master.flex, scenario,i,-InventoryUp)
        master.flex[,Dummy := c(1:num)]
        
        p = ggplot() +
            geom_line(data = master.flex[i == int,], aes(x = Dummy, y = InventoryUp, color = scenario), size = 1.1) +
            labs(x = 'Intervals of Year', y = "Flexibility Up Surplus") +
            scale_color_brewer("",palette='Set1') +
            theme(text = element_text(size = 18))
    
        assign(sprintf("p_invup_%s",i.name),p)
        
        setorder(master.flex, scenario,i,-InventoryDown)
        master.flex[,Dummy := c(1:num)]
        
        p = ggplot() +
            geom_line(data = master.flex[i == int,], aes(x = Dummy, y = InventoryDown, color = scenario), size = 1.1) +
            labs(x = 'Intervals of Year', y = "Flexibility Down Surplus") +
            scale_color_brewer("",palette='Set1') +
            theme(text = element_text(size = 18))
        
        assign(sprintf("p_invdown_%s",i.name),p)
        
        flex.total = flex.total.timeseries
        setorder(flex.total, scenario, i,-FlexibilityUp)
        num = nrow(flex.total[i == unique(flex.total[,i])[1] & scenario == unique(master.flex[,scenario])[1],])
        flex.total[,Dummy:= c(1:num)]
        
        p = ggplot() + 
          geom_line(data = flex.total[i==int,],aes(x=Dummy,y =FlexibilityUp, color = scenario), size = 1.1) +
          labs( x = "Intervals of Year", y = "Flexibility Up Inventory") +
          scale_color_brewer("", palette = 'Set1') +
          theme(text = element_text(size = 18))
        
        assign(sprintf("p_flexup_duration_%s",i.name),p)
        
        setorder(flex.total, scenario, i, -FlexibilityDown)
        flex.total[,Dummy:=c(1:num)]
        
        p = ggplot() + 
          geom_line(data = flex.total[i==int],aes(x=Dummy, y = FlexibilityDown, color = scenario), size=1.1) +
          labs(x="Intervals of Year", y = "Flexibility Down Inventory") +
          scale_color_brewer("", palette = 'Set1') + 
          theme(text = element_text(size = 18))
        
        assign(sprintf("p_flexdown_duration_%s",i.name),p)
    
        ## diurnal curves of inventory up and down
        
        p = ggplot() + 
            geom_line(data = master.flex.summary[i == int,], aes(x = Interval, y = InventoryUp, color = scenario), size = 1.1) + 
            facet_grid(.~Quarter, scales = 'free') +
            labs(x = NULL, y = "Flexibility Up Surplus") +
            scale_color_brewer("",palette='Set1') +
            theme(text = element_text(size = 18)) +
            scale_x_continuous(breaks = c(0,max(master.flex.summary[,Interval])/2))
        
        assign(sprintf("p_diup_%s",i.name),p)
        
        p = ggplot() + 
            geom_line(data = master.flex.summary[i == int,], aes(x = Interval, y = InventoryDown, color = scenario), size = 1.1) + 
            facet_grid(.~Quarter, scales = 'free') +
            labs(x = NULL, y = "Flexibility Down Surplus") +
            scale_color_brewer("",palette='Set1') +
            theme(text = element_text(size = 18)) +
            scale_x_continuous(breaks = c(0,max(master.flex.summary[,Interval])/2))
        
        assign(sprintf("p_didown_%s",i.name),p)
        
        ## total flexibility sources by type and scenario
        t = flex.inventory.timeseries[i == int & !(Type %in% re.types), .(FlexibilityUp = sum(FlexibilityUp),FlexibilityDown = sum(FlexibilityDown)), by = .(scenario,i,Type)]
        t = melt.data.table(t, id  = c("scenario","i","Type"))
        p = ggplot() +
            geom_bar(data = t[variable=="FlexibilityUp",], aes( x = scenario, y = value/1000.0, fill = Type),position = "dodge", stat = "identity") +
            geom_bar(data = t[variable=="FlexibilityDown",], aes( x = scenario, y = value/1000.0*-1.0, fill = Type),position = "dodge", stat = "identity") +
            geom_abline(slope = 0, intercept = 0, size = 1.3) + 
            scale_fill_manual("",values = gen.color2) + 
            labs(x = NULL, y = "Flexibility Available (GW)") +
            theme(text = element_text(size = 18),
                  axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 0.5)) 
        
        assign(sprintf("p_totalflex_%s",i.name),p)
        
        ##total flexibility sources compared to total generator capacities
        v = flex.inventory.timeseries[i == int & !(Type %in% re.types), .(FlexibilityUp = sum(FlexibilityUp),FlexibilityDown = sum(FlexibilityDown),Capacity=max(Capacity)), by = .(scenario,i,Type)]
        agg = v[,.(TotalFlexUp=sum(FlexibilityUp),TotalFlexDown=sum(FlexibilityDown),TotalCapacity=sum(Capacity)),by = .(scenario,i)]
        setkey(v,scenario,i)
        setkey(agg,scenario,i)
        v = agg[v]
        v[,`Flex Up Fraction`:=FlexibilityUp/TotalFlexUp]
        v[,`Flex Down Fraction`:=FlexibilityDown/TotalFlexDown]
        v[,`Capacity Fraction`:=Capacity/TotalCapacity]
        v = v[,.(i,scenario,Type,`Flex Up Fraction`,`Flex Down Fraction`,`Capacity Fraction`)]
        v = melt.data.table(v, id = c('i','scenario','Type'))
        p = ggplot() + 
          geom_bar(data = v, aes(x = variable, y = value*100.0, fill = Type), position = "stack",stat = "identity") +
          scale_fill_manual("",values = gen.color2) +
          labs(x = NULL, y  = "%") +
          facet_grid(scenario~.)+
          theme(text = element_text(size = 18),
                axis.text.x = element_text(angle=90,vjust = 0.5, hjust = 0.5))
        
        assign(sprintf("p_percflex_%s",i.name),p)
        
        
        ## diurnal flexibility of sources by type and scenario?
        u = flex.inventory.timeseries[i == int & !(Type %in% re.types), .(FlexibilityUp = mean(FlexibilityUp),FlexibilityDown = mean(FlexibilityDown)), by = .(scenario,i,Type,Quarter,Interval)]
        u = melt.data.table(u, id  = c("scenario","i","Type","Quarter","Interval"))
        p = ggplot() +
            geom_bar(data = u[variable=="FlexibilityUp",], aes( x = Interval, y = value, fill = Type), stat = "identity") +
            geom_bar(data = u[variable=="FlexibilityDown",], aes( x = Interval, y = value*-1.0, fill = Type), stat = "identity") +
            facet_grid(scenario~Quarter) + 
            geom_abline(slope = 0, intercept = 0, size = 1.3) + 
            scale_fill_manual("",values = gen.color2) + 
            labs(x = NULL, y = "Mean Available Flexibility (MW)") +
            theme(text = element_text(size = 18)) +
            scale_x_continuous(breaks = c(0,max(master.flex.summary[,Interval])/2)) 
        
        assign(sprintf("p_diurnalflex_%s",i.name),p)
        
        ## create plots of how largest up ramp and down ramp are being accomodated
        v = important.periods.timeseries[i == int,.(value = sum(Generation),category='Generation'), by = .(scenario,time,Type,id,i)]
        for(k in scenario.names){
            vv = v[scenario == k,.(interval=i,variable=Type,value,category,scenario,time,id)]
            r = ramp_day[scenario == k & interval == int & variable %in% c("Net Load","Potential Net Load"),.(id="Biggest Ramp Up",category='Net Load'), by = .(scenario,time,variable,value,interval)]
            r = rbind(r,ramp_day_down[scenario == k & interval == int & variable %in% c("Net Load","Potential Net Load"),.(id="Biggest Ramp Down",category='Net Load'), by = .(scenario,time,variable,value,interval)])
            r = rbind(r,vv)
            rr= rect[scenario == k & interval == int,.(id="Biggest Ramp Up",category='Net Load'), by = .(scenario,start,end,interval)]
            rr= rbind(rr,rect_down[scenario == k & interval == int, .(id = "Biggest Ramp Down",category='Net Load'), by = .(scenario,start,end,interval)])
            rr = rbind(copy(rr),copy(rr[,category:='Generation']))
            r[,category:=factor(category,levels = c('Net Load','Generation'))]
            rr[,category:=factor(category,levels = c('Net Load','Generation'))]
            r[,lt:='Net Load']
            r$lt[which(r$variable=='Potential Net Load')]='Potential Net Load'
            p = ggplot() +
                geom_line(data = r[variable %in% c('Potential Net Load','Net Load')],aes(x = time, y= value, linetype = lt),size = 1.1) + 
                geom_line(data = r[!(variable %in% c('Potential Net Load','Net Load'))],aes(x = time, y= value, color = variable),size = 1.1) + 
                geom_rect(data = rr, inherit.aes = FALSE, aes(xmin = start, xmax = end, ymin = pmin(0,min(ramp_day$value)), ymax = max(ramp_day$value)),fill = 'orange', alpha = 0.3) +
                scale_linetype_discrete("") +
                scale_color_manual("",values = c(gen.color2,'Net Load' = 'black', 'Potential Net Load' = 'black')) +
                ylim(NA, max(ramp_day$value)) +
                labs(x = NULL, y = "MW") +
                theme(text = element_text(size = 18)) +
                facet_grid(category~id,scales = "free", space = "free_x") +
                scale_x_datetime(breaks = date_breaks(width = '24 hour'), labels = date_format("%m-%d\n%H:%M"),
                                 expand = c(0,0), timezone = 'UTC') +
                guides(linetype = guide_legend(order = 1),
                       color = guide_legend(order = 2))

            assign(sprintf("p_rampability_%s_%s",k,int),p)
            
            flex.ribbon = master.flex[scenario == k & i == int & !(is.na(DemandUp)),]
            ##aggregated results
            flex.ribbon.agg = flex.ribbon[,.(Netload=mean(value),FlexUp=mean(FlexibilityUp),FlexDown=mean(FlexibilityDown)), by = .(Quarter,Interval,scenario,i)]
            
            p = ggplot() + 
              geom_ribbon(data = flex.ribbon.agg, aes(x = Interval, ymin = Netload - FlexDown, ymax = Netload + FlexUp, fill =scenario), alpha = 0.5) +
              geom_line(data = flex.ribbon.agg, aes(x = Interval, y = Netload)) +
              facet_grid(~Quarter) +
              #scale_fill_discrete('Flexibility Range') +
              scale_fill_brewer("",palette='Set1') +
              scale_x_continuous(breaks = c(0,12)) +
              theme(text = element_text(size = 18)) + 
              labs(y = "Net Load (MW)", x = "Hour of Day") + 
              ylim(0,NA)
            
            assign(sprintf("p_diurnal_flexribbon_%s_%s",k,int),p)
            
            small.time=flex.ribbon[FlexibilityUp==min(flex.ribbon$FlexibilityUp),time]
            if(grepl("h",int)){
              small.time.end=small.time+hours(as.numeric(sapply(strsplit(int, " "), "[[", 1)))
            }
            if(grepl("m",int)){
              small.time.end=small.time+minutes(as.numeric(sapply(strsplit(int, " "), "[[", 1)))
            }
            small.date=sapply(strsplit(as.character(small.time)," "),"[[",1)
            if(grepl("h",int)){
              if(as.numeric(sapply(strsplit(int, " "), "[[", 1))>12){
                small.date=c(small.date,sapply(strsplit(as.character(small.time+hours(24))," "),"[[",1))
              }
            }
            if(length(small.date)==1){
              flex.ribbon.smallup = flex.ribbon[grepl(small.date,time),]
            }
            if(length(small.date)>1){
              flex.ribbon.smallup = NULL
              for(m in small.date){
                if(!is.null(flex.ribbon.smallup)){
                  flex.ribbon.smallup=rbind(flex.ribbon.smallup,flex.ribbon[grepl(m,time),])
                }
                else{
                  flex.ribbon.smallup=flex.ribbon[grepl(m,time),]
                }
              }
            }
            p = ggplot() + 
              geom_rect(data = flex.ribbon.smallup, inherit.aes = FALSE, aes(xmin = small.time, xmax = small.time.end, ymin = 0, ymax = max(value+FlexibilityUp)),fill = 'orange', alpha = 0.3) +
              geom_ribbon(data = flex.ribbon.smallup,aes(x=time, ymin=value - FlexibilityDown, ymax= value + FlexibilityUp, fill = scenario), alpha = 0.5) + 
              geom_line(data = flex.ribbon.smallup, aes(x=time, y = value)) +
              scale_color_brewer("",palette='Set1') +
              theme(text = element_text(size = 18)) + 
              labs(y = "Net Load (MW)", x = NULL) +
              ylim(0, NA) +
              scale_x_datetime(breaks = date_breaks(width = '12 hour'), labels = date_format("%m-%d\n%H:%M"),
                               expand = c(0,0), timezone = 'UTC') 
            
            assign(sprintf("p_minup_flexribbon_%s_%s",k,int),p)
            
            small.time=flex.ribbon[FlexibilityDown==min(flex.ribbon$FlexibilityDown),time]
            if(grepl("h",int)){
              small.time.end=small.time+hours(as.numeric(sapply(strsplit(int, " "), "[[", 1)))
            }
            if(grepl("m",int)){
              small.time.end=small.time+minutes(as.numeric(sapply(strsplit(int, " "), "[[", 1)))
            }
            small.date=sapply(strsplit(as.character(small.time)," "),"[[",1)
            if(grepl("h",int)){
              if(as.numeric(sapply(strsplit(int, " "), "[[", 1))>12){
                small.date=c(small.date,sapply(strsplit(as.character(small.time+hours(24))," "),"[[",1))
              }
            }
            if(length(small.date)==1){
              flex.ribbon.smallup = flex.ribbon[grepl(small.date,time),]
            }
            if(length(small.date)>1){
              flex.ribbon.smallup = NULL
              for(m in small.date){
                if(!is.null(flex.ribbon.smallup)){
                  flex.ribbon.smallup=rbind(flex.ribbon.smallup,flex.ribbon[grepl(m,time),])
                }
                else{
                  flex.ribbon.smallup=flex.ribbon[grepl(m,time),]
                }
              }
            }
            if(grepl("h",int)){
              small.time.end=small.time+hours(as.numeric(sapply(strsplit(int, " "), "[[", 1)))
            }
            if(grepl("m",int)){
              small.time.end=small.time+minutes(as.numeric(sapply(strsplit(int, " "), "[[", 1)))
            }
            small.date=sapply(strsplit(as.character(small.time)," "),"[[",1)
            flex.ribbon.smalldown = flex.ribbon[grepl(small.date,time),]
            p = ggplot() + 
              geom_rect(data = flex.ribbon.smalldown, inherit.aes = FALSE, aes(xmin = small.time, xmax = small.time.end, ymin = 0, ymax = max(value+FlexibilityUp)),fill = 'orange', alpha=0.3) +
              geom_ribbon(data = flex.ribbon.smalldown,aes(x=time, ymin=value - FlexibilityDown, ymax= value + FlexibilityUp, fill = scenario), alpha = 0.5) + 
              geom_line(data = flex.ribbon.smalldown, aes(x=time, y = value)) +
              scale_color_brewer("",palette='Set1') +
              theme(text = element_text(size = 18)) + 
              labs(y = "Net Load (MW)", x = NULL) +
              ylim(0, NA) +
              scale_x_datetime(breaks = date_breaks(width = '12 hour'), labels = date_format("%m-%d\n%H:%M"),
                               expand = c(0,0), timezone = 'UTC') 
            
            assign(sprintf("p_mindown_flexribbon_%s_%s",k,int),p)

        }
    }
}
