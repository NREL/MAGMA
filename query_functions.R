
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This file contains functions for the general PLEXOS solution analysis that reports an HTML of common figures.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generation by type
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This function returns total generation by type and curtailment. Curtailment is calculated according to the renewable types specified in the input file. 

gen_by_type = function(total.generation, total.avail.cap) {
  
  setkey(total.generation,'name')
  setkey(total.avail.cap,'name')
  
  # Filter out generation and available capacity data and add generation type by matching generator name. 
  yr.gen = total.generation[property == 'Generation',][, Type:=gen.type.mapping[name] ][,.(scenario,Type,property,value)]
  
  avail = total.avail.cap[property == 'Available Energy',][, Type:=gen.type.mapping[name] ][,.(scenario,Type,property,value)]
  avail = avail[Type %in% re.types,.(value=sum(value)),by=.(scenario,Type,property)]
  avail[,property:=NULL]
  
  # Pull out generation data for types used in curtailment calculation.
  re.gen = yr.gen[Type %in% re.types, .(value=sum(value)),by=.(scenario,Type,property)]
  re.gen[,property:=NULL]

  # Sum up generation by type
  yr.gen = yr.gen[,.(GWh=sum(value)),by=.(scenario,Type)]
    
  if(typeof(avail)=='double' & typeof(re.gen)=='double') {
    # Calculate curtailment
    curt = avail - re.gen
    curt.tot = sum(curt)
    
    # Combine everything before returning the resulting data.
    yr.gen = rbind(yr.gen, data.table(scenario = unique(yr.gen[,scenario]), Type = 'Curtailment', GWh = curt.tot))
    
  } else if (length(avail[,1])>0 & length(re.gen[,1])>0) {
    # Calculate curtailment
    setkey(avail, Type, scenario)
    setkey(re.gen, Type, scenario)
    curt = avail[re.gen][,curt:=value-i.value]
    curt.tot = curt[,.(Type='Curtailment',GWh=sum(curt)), by=.(scenario)]
    
    # Combine everything before returning the resulting data.
    yr.gen = rbindlist(list(yr.gen, curt.tot))
  }

  return(yr.gen)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Region and Zone Generation by type according to generator name
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This function returns total generation separated by type but also by region and zone.  

region_zone_gen = function(total.generation, total.avail.cap) {
  
  setkey(total.generation,'name')
  setkey(total.avail.cap,'name')
  gen.type.zone.region = region.zone.mapping[, Type:=gen.type.mapping[name]]
  
  # Filter out generation and available capacity data and add generation type by matching generator name.
  # Also add region and zone by matching generator name in the region and zone mapping file. 
  gen.data = total.generation[property=='Generation', .(scenario,name,category,value)][gen.type.zone.region]
  gen.data = gen.data[, .(value=sum(value)), by=.(scenario,Type, Region, Zone)]
  
  avail.data = total.avail.cap[property == 'Available Energy', 
                               .(scenario,name,category,value)][gen.type.zone.region]
  avail.data = avail.data[Type %in% re.types, .(Avail = sum(value)), by=.(scenario,Type, Region, Zone)]
    

  # Curtailment calculation based on renewable types specified in input file
  setkey(avail.data,scenario,Type,Region,Zone)
  curt = gen.data[Type %in% re.types, ]
  setkey(curt,scenario,Type,Region,Zone)
  curt = curt[avail.data]
  curt[,Type := 'Curtailment']
  curt[,value := Avail - value]
  curt[,Avail := NULL ]
   
  # Combine generation and curtailment and return.
  gen.data = rbindlist(list(gen.data, curt))
  
  return(gen.data)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Key Period Generation by Type 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This function returns interval level generation and curtailment used for the key period time series dispatch stacks. 

interval_generation = function(interval.region.load, interval.zone.load, interval.generation, interval.avail.cap) {

  gen.type.zone.region = region.zone.mapping[, Type:=gen.type.mapping[name]]
  setkey(interval.generation,name)
  setkey(interval.avail.cap,name)
  
  # Either sum up load for each region or each zone, depending on which there are more of. 
  if (length(region.names)>=length(zone.names)){
    load = interval.region.load[,.(value=sum(value)),by=.(time,name,property)]
    setnames(load,"name","Region")
    spatialcol = "Region"    
  } else {
    load = interval.zone.load[,.(value=sum(value)),by=.(time,name,property)]
    setnames(load,"name","Zone")
    spatialcol = "Zone"
  }
  setkeyv(rz.unique,spatialcol)
  setkeyv(load,spatialcol)
  load = load[rz.unique]
  setkeyv(load,c('time',spatialcol))
  setnames(load,'property','Type')
  
  # Pull out interval generation data, and add generation type and region and zone according to generator name. Then add load data.
  int.gen = interval.generation[,.(name, time, value, category)][gen.type.zone.region]
  int.gen = int.gen[,.(value=sum(value,na.rm=TRUE)),by=.(time,Region,Zone,Type)] 
  setkeyv(int.gen,c('time',spatialcol))
  int.gen = rbindlist(list(int.gen,load),use.names=TRUE)
  
  #make sure that the right zones and regions are there...
  dropcol=names(rz.unique)[names(rz.unique) != spatialcol]
  setkeyv(int.gen,spatialcol)
  int.gen = merge(int.gen[,!dropcol,with=FALSE],rz.unique,all.y=TRUE)
  
  # Pull out interval generation capacity and add generation type, region, and zone based on matching generator names.
  int.avail = interval.avail.cap[,.(name, time, value, category)][gen.type.zone.region]
  int.avail = int.avail[,.(value=sum(value,na.rm=TRUE)),by=.(time,Region,Zone,Type)]
 
  if (re.types!='none_specified'){
    #  Pull out renewable data for curtilment calculations. 
    re.gen = int.gen[Type %in% re.types, ]
    setkey(re.gen,time,Region,Zone,Type)
  
    int.avail = int.avail[Type %in% re.types, ]
    setkey(int.avail,time,Region,Zone,Type)
    
    # Calculate curtailment and add it to generation and load data from above.
    curtailed = merge(int.avail, re.gen, all=TRUE)
    curtailed[is.na(curtailed)] = 0
    curtailed=curtailed[,.(Type='Curtailment',value=sum(value.x-value.y)),by=.(time,Region,Zone)]
    
    int.gen = rbindlist(list(int.gen, curtailed), use.names=TRUE)
  
  } 
 
  return(int.gen)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Total Curtailment
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Calculates interval level total curtailment

total_curtailment = function(interval.generation, interval.avail.cap) {
  
  setkey(interval.generation,name)
  setkey(interval.avail.cap,name)
  
  # Separate generation and available capacity data by type for each interval.
  c.gen = interval.generation[, Type:=gen.type.mapping[name] ][Type %in% re.types,.(value=sum(value)),by=.(time, Type)]
  c.avail = interval.avail.cap[, Type:=gen.type.mapping[name] ][Type %in% re.types,.(value=sum(value)),by=.(time, Type)]

  if (typeof(c.avail)=='double' & typeof(c.gen)=='double') {
    curt.tot = c.avail - c.gen
    curt.tot = data.table(curt.tot)
    curt.tot[,day := rep(1:length(curt.tot)/intervals.per.day,each=intervals.per.day)]
    curt.tot[,interval := 1:intervals.per.day,by=.(day)]
  } else {
    # Summing up total curtailment for each interval
    setkey(c.avail,time,Type)
    setkey(c.gen,time,Type)
    curt = c.avail[c.gen][,curt := value-i.value]
    curt.tot = curt[,.(Curtailment=sum(curt)),by=.(time)]
    curt.tot[,day := as.POSIXlt(time)[[8]]]
    curt.tot[,interval := 1:intervals.per.day,by=.(day)]
  }

  return(curt.tot)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Cost 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Returns a table of total run costs

costs = function(total.emissions.cost, total.fuel.cost, total.ss.cost, total.vom.cost) {
  
  cost.data = rbindlist(list(total.emissions.cost, total.fuel.cost, total.ss.cost, total.vom.cost))
  cost.table = cost.data[,.(Cost = sum(value/1000)), by=.(scenario,property)]
  cost.table[, property:=gsub("Cost","",property)]
  tot.cost = cost.table[,.(property = "Total", Cost = sum(Cost)), by=.(scenario)]
  cost.table = rbindlist(list(cost.table,tot.cost))
  cost.table[,Type:=factor(Type,levels=unique(Type))]

  setnames(cost.table, "property","Type")
  setnames(cost.table, "Cost", "Cost (MM$)")

  if (length(unique(cost.table$scenario))==1){
    cost.table[, scenario := NULL]
  }

return(cost.table)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Annual Reserve Provisions
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Calculates total reserve provision and shortage for each reserve type. 

annual_reserves = function(total.reserve.provision, total.reserve.shortage) {
    
  provision = total.reserve.provision[, .(Type = name,`Provisions (GWh)` = sum(value)),by = .(scenario, name)]
  provision[,name:=NULL]
  shortage = total.reserve.shortage[, .(Type = name, `Shortage (GWh)` = sum(value)),by = .(scenario, name)]
  shortage[,name:=NULL]
  
  setkey(provision,scenario,Type)
  setkey(shortage,scenario,Type)
  r.data = provision[shortage]
  
  return(r.data)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Interval Reserve Provisions 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Calculates the interval level reserve provision

interval_reserves = function(interval.reserve.provision) {
  provision = interval.reserve.provision[, .(provision = sum(value)), by = .(time)]
  return(provision)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Interface Flows 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Total run and interval level interface flow data, for specific interfaces that are specified in the input file. 

annual_interface_flows = function(total.interface.flow) {
  
  year.flows = total.interface.flow[name %in% interfaces,.(name,time,value)]  
  year.flows[,Type := 'Annual_Flow']

  return(year.flows)
}

interval_interface_flows = function(interval.interface.flow) {
  
  int.flows = interval.interface.flow[name %in% interfaces,.(name,time,value)]   
  int.flows[,Type := 'Interval_Flow']
  
  return(int.flows)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Region and Zone Stats
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This function sums up region and zone stats for the entire run. 
# zones are defined either in PLEXOS, or using the region and zone mapping file. 

region_stats = function(total.region.load, total.region.imports, total.region.exports, total.region.ue) {
  r.data = rbindlist(list(total.region.load, total.region.imports, total.region.exports, total.region.ue))
  r.data = r.data[, .(value=sum(value)), by=.(name,property)]
  r.stats = dcast(r.data, name~property, value.var = 'value')
  return(r.stats)
}

zone_stats = function(total.region.load, total.region.imports, total.region.exports, total.region.ue, total.zone.load, total.zone.imports, total.zone.exports, total.zone.ue) {
  if (reassign.zones==TRUE | total.zone.load=='ERROR'){
    z.data = rbindlist(list(total.region.load, total.region.imports, total.region.exports, total.region.ue))
    setnames(z.data,'name','Region')
    setkey(z.data,Region)
    setkey(rz.unique,Region)
    z.stats = z.data[rz.unique][, .(value = sum(value)), by = .(Zone,property)] %>%
      dcast(Zone~property, value.var = 'value') 
    setnames(z.stats,'Zone','name')
  } else {
    z.data = rbindlist(list(total.zone.load, total.zone.imports, total.zone.exports, total.zone.ue))
    z.data = z.data[,.(value=sum(value)),by=.(name,property)]
    z.stats = dcast(z.data, name~property, value.var = 'value')
  }
  return(z.stats)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Region and Zone Load
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Returns region level and zone level load data for the entire run. 

region_load = function(total.region.load) {
  r.load = total.region.load[,.(value=sum(value)), by=.(scenario,name)]
  return(r.load)
}

zone_load = function(total.region.load, total.zone.load) {
  if (reassign.zones==TRUE | total.zone.load=='ERROR'){
    setkey(total.region.load,name)
    setkey(rz.unique,Region)
    z.load = rz.unique[total.region.load][, .(value=sum(value)), by=.(scenario, Zone)]
    setnames(z.load,"Zone","name")
  } else {
    z.load = total.zone.load[,.(value=sum(value)), by=.(scenario, name)]
  }
  return(z.load)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Capacity Factor
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Calculates the capacity factor of all the generation types for the full run. 

capacity_factor = function(total.generation, total.installed.cap) {
  
  setkey(total.installed.cap,name)
  setkey(total.generation,name)
  
  # Pull out installed capacity and generation and match them to generation type by generator name. 
  mc = total.installed.cap[, Type:=gen.type.mapping[name] ]
  setnames(mc,'value','MaxCap')
  
  gen = total.generation[, Type:=gen.type.mapping[name] ]
  setnames(gen,'value','Gen')
        
  mc[, Type := factor(Type, levels = rev(c(gen.order)))]
  
  # Calculates generation type total capacity and generation for the full run
  c.factor = mc[,.(name,MaxCap,Type)][gen[,.(name,Gen)]]
  c.factor = c.factor[,.(MaxCap=sum(MaxCap),Gen=sum(Gen)),by=.(Type)]

  # Calculate capacity factor for each generation type
  n.int = length(seq(from = first.day, to = last.day, by = 'day'))*intervals.per.day
  c.factor[,.(`Capacity Factor (%)` := Gen/(MaxCap/1000*n.int)*100),by=.(Type)]
  
  return(c.factor)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Committed capacity 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This function just pulls out available capacity at the interval level for use int he DA-RT committment and dispatch plots

cap_committed = function(interval.da.committment) {
  
  if (length(region.names)>=length(zone.names)){
    spatialcol = "Region"
  } else {
    spatialcol = "Zone"    
  }

  gen.type.zone.region = region.zone.mapping[, Type:=gen.type.mapping[name]]
  setkey(interval.da.committment,name)
  
  # Query available capacity at the interval level, add generation type and region and zone by matching mapping file with generator names.
  commit.data = interval.da.committment[,.(time,name,category,value)][gen.type.zone.region]
  commit.data = commit.data[,.(committed.cap=sum(value)),by=.(time,Region,Zone,Type)]
  
  return(commit.data)
} 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Query General Data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# These functions are called from the setup data queries file. They use the rplexos package to query the solution database.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generator total run data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Full run generation data
total_generation = function(database) {
  total.gen = data.table(query_year(database, 'Generator', 'Generation', columns = c('category', 'name')))
  return(total.gen[, .(scenario, property, name, category, value)])
}

# Full run available capacity
total_avail_cap = function(database) {
  total.avail.cap = data.table(query_year(database, 'Generator', 'Available Energy', columns = c('category', 'name')))
  return(total.avail.cap[, .(scenario, property, name, category, value)])
}

# Full run emissions cost
total_emissions = function(database) {
  total.emissions.cost = data.table(query_year(database, 'Generator', 'Emissions Cost', columns = c('category', 'name')))
  return(total.emissions.cost[, .(scenario, property, name, category, value)])
}

# Full run fuel cost
total_fuel = function(database) {
  total.fuel.cost = data.table(query_year(database, 'Generator', 'Fuel Cost', columns = c('category', 'name')))
  return(total.fuel.cost[, .(scenario, property, name, category, value)])
}

# Full run S&S cost
total_ss = function(database) {
  total.ss.cost = data.table(query_year(database, 'Generator', 'Start & Shutdown Cost', columns = c('category', 'name')))
  return(total.ss.cost[, .(scenario, property, name, category, value)])
}

# Full run VO&M cost
total_vom = function(database) {
  total.vom.cost = data.table(query_year(database, 'Generator', 'VO&M Cost', columns = c('category', 'name')))
  return(total.vom.cost[, .(scenario, property, name, category, value)])
}

# Full run installed capacity
total_installed_cap = function(database) {
  total.installed.cap = data.table(query_year(database, 'Generator', 'Installed Capacity', columns = c('category', 'name')))
  return(total.installed.cap[, .(scenario, property, name, category, value)])
}

# Full run reserve provision
total_gen_reserve_provision = function(database) {
  total.res.provision = data.table(query_year(database, 'Reserve.Generators', 'Provision', columns = c('category', 'name')))
  return(total.res.provision[, .(scenario, property, name, category, value)])
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Region total run data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Full run region load
total_region_load = function(database) {
  total.region.load = data.table(query_year(database, 'Region', 'Load'))
  return(total.region.load[, .(scenario, property, name, value)])
}

# Full run region imports
total_region_imports = function(database) {
  total.region.imports = data.table(query_year(database, 'Region', 'Imports'))
  return(total.region.imports[, .(scenario, property, name, value)])
}

# Full run region exports
total_region_exports = function(database) {
  total.region.exports = data.table(query_year(database, 'Region', 'Exports'))
  return(total.region.exports[, .(scenario, property, name, value)])
}

# Full run region unserved energy
total_region_ue = function(database) {
  total.region.ue = data.table(query_year(database, 'Region', 'Unserved Energy'))
  return(total.region.ue[, .(scenario, property, name, value)])
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Zone total run data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Full run zone load
total_zone_load = function(database) {
  total.zone.load = data.table(query_year(database, 'Zone', 'Load'))
  return(total.zone.load[, .(scenario, property, name, value)])
}

# Full run zone imports
total_zone_imports = function(database) {
  total.zone.imports = data.table(query_year(database, 'Zone', 'Imports'))
  return(total.zone.imports[, .(scenario, property, name, value)])
}

# Full run zone exports
total_zone_exports = function(database) {
  total.zone.exports = data.table(query_year(database, 'Zone', 'Exports'))
  return(total.zone.exports[, .(scenario, property, name, value)])
}

# Full run zone unserved energy
total_zone_ue = function(database) {
  total.zone.ue = data.table(query_year(database, 'Zone', 'Unserved Energy'))
  return(total.zone.ue[, .(scenario, property, name, value)])
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Reserves total run data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Full run reserves provision
total_reserve_provision = function(database) {
  total.reserve.provision = data.table(query_year(database, 'Reserve', 'Provision'))
  return(total.reserve.provision[, .(scenario, property, name, value)])
}

# Full run reserves shortage
total_reserve_shortage = function(database) {
  total.reserve.shortage = data.table(query_year(database, 'Reserve', 'Shortage'))
  return(total.reserve.shortage[, .(scenario, property, name, value)])
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Selected interface total run data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Full run interface flows
total_interface_flow = function(database) {
  total.interface = data.table(query_year(database, 'Interface', 'Flow'))
  return(total.interface[, .(scenario, property, name, value, time)])
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Interval queries
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Interval level generator generation
interval_gen = function(database) {
  interval.gen = data.table(query_interval(database, 'Generator', 'Generation', columns = c('category', 'name'))) 
  return(inteval.gen[,.(scenario, property, name, value, time, category) ])
}

# Interval level generator capacity
interval_avail_cap = function(database) {
  interval.avail.cap = data.table(query_interval(database, 'Generator', 'Available Capacity', columns = c('category', 'name'))) 
  return(interval.aail.cap[,.(scenario, property, name, value, time, category) ])
}

# Interval level region load 
interval_region_load = function(database) {
  interval.region.load = data.table(query_interval(database, 'Region', 'Load'))
  return(interval.region.load[, .(scenario, property, name, time, value)])
}

# Interval level region load and price
interval_region_price = function(database) {
  interval.region.price = data.table(query_interval(database, 'Region', 'Price'))
  return(interval.region.price[, .(scenario, property, name, time, value)])
}

# Interval level zone load
interval_zone_load = function(database) {
  interval.zone.load = data.table(query_interval(database, 'Zone', 'Load'))
  return(interval.zone.load[, .(scenario, property, name, time, value)])
}

# Interval level interface flows
interval_interface_flow = function(database) {
  interval.interface.flow = data.table(query_interval(database, 'Interface', 'Flow'))
  return(interval.interface.flow[, .(scenario, property, name, time, value)])
}

# Interval level reserve provisions
interval_reserve_provision = function(database) {
  interval.reserve.provision = data.table(query_interval(database, 'Reserve', 'Provision'))
  return(interval.reserve.provision[, .(scenario, property, name, time, value)])
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Runtime queries
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Runtime data
interval_runtime = function(database){
  interval.runtime = data.table(query_log_steps(database))
  return(interval.runtime[,.(scenario,phase,step,time)] )
}

# Timestep data
model_timesteps = function(database){
  timesteps = data.table(query_time(database))
  timesteps = timesteps[phase=="ST", ]
  return(timesteps[, .(scenario, phase, start, end, count, timestep)])
}
