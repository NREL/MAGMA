
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This file contains functions for the general PLEXOS solution analysis that reports an HTML of common figures.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generation by type
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This function returns total generation by type and curtailment. Curtailment is calculated according to the renewable types specified in the input file. 

gen_by_type = function(total.generation, total.avail.cap) {
  
  yr.data = rbind(total.generation, total.avail.cap)
  
  # Filter out generation and available capacity data and add generation type by matching generator name. 
  if ( use.gen.type.mapping.csv ) {
    yr.gen = yr.data %>%
      filter(property == 'Generation') %>%
      join(gen.type.mapping, by = 'name') %>%
      select(Type, property, value)
    
    avail = yr.data %>%
      filter(property == 'Available Energy') %>%
      join(gen.type.mapping, by = 'name') %>%
      select(Type, property, value) %>%
      filter(Type %in% re.types ) %>%
      dcast(property ~ Type, sum)
    
    # Same as above section except this is for if you are using the PLEXOS category to match generation type instead of a mapping CSV file.
  } else {
    yr.gen = yr.data %>%
      filter(property == 'Generation') %>%
      join(category2type, by = 'category') %>%
      select(Type, property, value)
    
    avail = yr.data %>%
      filter(property == 'Available Energy') %>%
      join(category2type, by = 'category') %>%
      select(Type, property, value) %>%
      filter(Type %in% re.types ) %>%
      dcast(property ~ Type, sum)
  }
  avail = avail[,2:ncol(avail)]
  
  # Pull out generation data for types used in curtailment calculation.
  re.gen = yr.gen %>%
    filter(Type %in% re.types ) %>%
    dcast(property ~ Type, sum)
  re.gen = re.gen[,2:ncol(re.gen)]

  # Sum up generation by type
  yr.gen = yr.gen %>%
    dcast(Type ~ property, sum) %>%
    rename(GWh = Generation) %>%
    ddply('Type', numcolwise(sum)) 
    
  # Calculate curtailment
  curt = avail - re.gen
  curt.tot = sum(curt)

  # Combine everything before returning the resulting data.
  yr.gen = rbind(yr.gen, data.frame(Type = 'Curtailment', GWh = curt.tot))
  
  return(yr.gen)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Region and Zone Generation by type according to generator name
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This function returns total generation separated by type but also by region and zone.  

region_zone_gen = function(total.generation, total.avail.cap) {
  
  r.z.gen = rbind(total.generation, total.avail.cap)
  
  # Filter out generation and available capacity data and add generation type by matching generator name.
  # Also add region and zone by matching generator name in the region and zone mapping file. 
  if ( use.gen.type.mapping.csv ) {
    gen.data = r.z.gen %>%
      filter(property=='Generation') %>%
      select(name, category, value) %>%
      plyr::join(region.zone.mapping, by='name') %>%
      plyr::join(gen.type.mapping, by = 'name')  
    
    avail.data = r.z.gen %>%
      filter(property == 'Available Energy') %>%
      select(name, category, value) %>%
      plyr::join(region.zone.mapping, by='name') %>%
      plyr::join(gen.type.mapping, by = 'name')  %>%
      filter(Type %in% re.types) %>%
      select(name, Avail = value)
    
    # Same as above section except this is for if you are using the PLEXOS category to match generation type instead of a mapping CSV file.
  } else {
    gen.data = r.z.gen %>%
      filter(property=='Generation') %>%
      select(name, category, value) %>%
      plyr::join(region.zone.mapping, by='name') %>%
      plyr::join(category2type, by = 'category')
    
    avail.data = r.z.gen %>%
      filter(property == 'Available Energy') %>%
      select(name, category, value) %>%
      plyr::join(region.zone.mapping, by='name') %>%
      plyr::join(category2type, by = 'category') %>%
      filter(Type %in% re.types) %>%
      select(name, Avail = value)
  }

  # Curtailment calculation based on renewable types specified in input file
  curt = gen.data %>%
    filter(Type %in% re.types ) %>%
    join(avail.data, by = 'name')
  
  curt$Type = 'Curtailment'
  curt$value = curt$Avail - curt$value
  curt$Avail = NULL
  
  # Combine generation and curtailment and return.
  gen.data = rbind(gen.data, curt)

  return(gen.data)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Key Period Generation by Type 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This function returns interval level generation and curtailment used for the key period time series dispatch stacks. 

interval_generation = function(interval.region.load, interval.zone.load, interval.generation, interval.avail.cap) {
  
  # Either sum up load for each region or each zone, depending on which there are more of. 
  if (length(region.names)>=length(zone.names)){
    load = dcast(interval.region.load, time+name~property, value.var = 'value', fun.aggregate = sum)
    colnames(load)[colnames(load)=='name'] = 'Region'
    spatialcol = "Region"
    
  } else {
    load = dcast(interval.zone.load, time+name~property, value.var = 'value', fun.aggregate = sum)
    colnames(load)[colnames(load)=='name'] = "Zone"
    spatialcol = "Zone"
    
  }
  
  colnames(load)[colnames(load)=='property'] = 'Type'
  
  # Pull out interval generation data, and add generation type and region and zone according to generator name. Then add load data.
  if ( use.gen.type.mapping.csv ) {
    int.gen = interval.generation %>%
      select(name, time, value, category) %>%
      join(gen.type.mapping, by = 'name') %>%
      join(region.zone.mapping, by = 'name') %>%
      dcast(time+Region+Zone ~ Type, value.var = 'value', fun.aggregate = sum) %>%
      join(load,by=c('time',spatialcol),type='full')
  } else {
    int.gen = interval.generation %>%
      select(name, time, value, category) %>%
      join(category2type, by = 'category') %>%
      select(name, time, value, Type) %>%
      join(region.zone.mapping, by = 'name') %>%
      dcast(time+Region+Zone ~ Type, value.var = 'value', fun.aggregate = sum) %>%
      join(load,by=c('time',spatialcol),type='full')
  }
  
  #make sure that the right zones and regions are there...
  int.gen = merge(int.gen[,!names(int.gen) %in% names(rz.unique)[names(rz.unique) != spatialcol]],
                  rz.unique,by=spatialcol,all.y=T)
  
  # Pull out renewable data for curtilment calculations. 
  re.gen = subset(int.gen, select = c('time',spatialcol,re.types))

  # Pull out interval generation capacity and add generation type, region, and zone based on matching generator names.
  if ( use.gen.type.mapping.csv ) {
    int.avail = interval.avail.cap %>%
      select(name, time, value, category) %>%
      join(gen.type.mapping, by = 'name') %>%
      join(region.zone.mapping, by = 'name') %>%
      dcast(time+Region+Zone ~ Type, sum) %>%
      subset(select = c('time',spatialcol,re.types)) %>%
      join(re.gen[,c('time',spatialcol)],by=c('time',spatialcol),type='full')
    
  } else {
    int.avail = interval.avail.cap %>%
      select(name, time, value, category) %>%
      join(category2type, by = 'category') %>%
      join(region.zone.mapping, by = 'name') %>%
      dcast(time+Region+Zone ~ Type, sum) %>%
      subset(select = c('time',spatialcol,re.types)) %>%
      join(re.gen[,c('time',spatialcol)],by=c('time',spatialcol),type='full')
  }
  
  # Calculate curtailment and add it to generation and load data from above. 
  int.avail[is.na(int.avail)] = 0
  re.gen[is.na(re.gen)] = 0
  curtailed = merge(int.avail,re.gen,by=c('time',spatialcol))
  for (i in re.types){
    curtailed[,i] = curtailed[,paste(i,'x',sep='.')]-curtailed[,paste(i,'y',sep='.')]
  }
  curtailed$Curtailment = rowSums(curtailed[,re.types])
  int.gen = merge(int.gen,curtailed[,c('time',spatialcol,'Curtailment')],by=c('time',spatialcol))
  int.gen[is.na(int.gen)] = 0
  

  return(int.gen)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Total Curtailment
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Calculates interval level total curtailment

daily_curtailment = function(interval.generation, interval.avail.cap) {
  
  gen.data = interval.generation
  avail.data = interval.avail.cap
  
  # Separate generation and available capacity data by type for each interval.
  if ( use.gen.type.mapping.csv ) {
    c.gen = gen.data %>%
      join(gen.type.mapping, by = 'name') %>%
      select(time, Type, value) %>%
      dcast(time ~ Type, value.var = 'value', fun.aggregate = sum) %>%
      subset(select = re.types)
    
    c.avail = avail.data %>%
      join(gen.type.mapping, by = 'name') %>%
      select(time, Type, value) %>%
      dcast(time ~ Type, sum) %>%
      subset(select = re.types)
    
    # Below does the same thing as above except matches generation type using PLEXOS categories instead of a mapping file.
  } else {
    c.gen = gen.data %>%
      join(category2type, by = 'category') %>%
      select(time, Type, value) %>%
      dcast(time ~ Type, value.var = 'value', fun.aggregate = sum) %>%
      subset(select = re.types)
    
    c.avail = avail.data %>%
      join(category2type, by = 'category') %>%
      select(time, Type, value) %>%
      dcast(time ~ Type, sum) %>%
      subset(select = re.types)  
  }

  # Summing up total curtailment for each interval
  curt = c.avail - c.gen
  curt.tot = data.frame(rowSums(curt))
  colnames(curt.tot) = 'Curtailment'
  curt.tot = t(curt.tot)
  dim(curt.tot) = c(intervals.per.day, length(curt.tot)/intervals.per.day)
  curt.tot = data.frame(curt.tot)

  return(curt.tot)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Cost 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Returns a table of total run costs

costs = function(total.emissions.cost, total.fuel.cost, total.ss.cost, total.vom.cost) {

  cost = rbind(total.emissions.cost, total.fuel.cost, total.ss.cost, total.vom.cost)
  cost['value'] = cost['value'] / 1000000
  
  e.cost = filter(cost, property == 'Emissions Cost')
  e.cost = e.cost['value'] * 1000
  
  f.cost = filter(cost, property == 'Fuel Cost')
  f.cost = f.cost['value'] * 1000
  
  s.s.cost = filter(cost, property == 'Start & Shutdown Cost')
  s.s.cost = s.s.cost['value'] * 1000
  
  VOM.cost = filter(cost, property == 'VO&M Cost')
  VOM.cost = VOM.cost['value'] * 1000

  tot.cost = e.cost + f.cost + s.s.cost + VOM.cost

  cost.table = data.frame(Type = c('Emissions', 'Fuel', 'Start_Shutdown', 'VOM', 'Total_Cost'), 
                          Cost = c(sum(e.cost), sum(f.cost), sum(s.s.cost), sum(VOM.cost), sum(tot.cost)))
  cost.table = rename(cost.table, `Cost (MM$)` = Cost)

return(cost.table)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Annual Reserve Provisions
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Calculates total reserve provision and shortage for each reserve type. 

annual_reserves = function(total.reserve.provision, total.reserve.shortage) {
  
  r.data = rbind(total.reserve.provision, total.reserve.shortage)
  
  provision =  select( filter(r.data, property == 'Provision'), name, value)
  colnames(provision) = c('Type', 'Provisions (GWh)')  
  
  shortage = select( filter(r.data, property == 'Shortage'), name, value)
  colnames(shortage) = c('Type', 'Shortage (GWh)')
  
  r.data = join(provision, shortage, by = 'Type')
  
  return(r.data)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Interval Reserve Provisions 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Calculates the interval level reserve provision

interval_reserves = function(interval.reserve.provision) {
  provision = interval.reserve.provision
  provision = dcast(provision, time ~ name, value.var = 'value')
  return(provision)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Interface Flows 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Total run and interval level interface flow data, for specific interfaces that are specified in the input file. 

annual_interface_flows = function(total.interface.flow) {
  
  year.flows = total.interface.flow %>%
    select(name, time, value) %>%
    filter(name %in% interfaces)
  
  year.flows$Type = 'Annual_Flow'

  return(year.flows)
}

interval_interface_flows = function(interval.interface.flow) {
  
  int.flows = interval.interface.flow %>%
    select(name, time, value) %>%
    filter(name %in% interfaces)
  
  int.flows$Type = 'Interval_Flow'
  
  return(int.flows)
  
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Region and Zone Stats
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This function sums up region and zone stats for the entire run. 
# zones are defined either in PLEXOS, or using the region and zone mapping file. 

region_stats = function(total.region.load, total.region.imports, total.region.exports, total.region.ue) {
  r.data = rbind(total.region.load, total.region.imports, total.region.exports, total.region.ue)
  r.stats = dcast(r.data, name~property, value.var = 'value', fun.aggregate = sum)
  return(r.stats)
}

zone_stats = function(total.region.load, total.region.imports, total.region.exports, total.region.ue, total.zone.load, total.zone.imports, total.zone.exports, total.zone.ue) {
  if (reassign.zones==TRUE | yr.data.zone=='ERROR'){
    z.data = rbind(total.region.load, total.region.imports, total.region.exports, total.region.ue)
    z.stats = z.data %>%
      join(select(region.zone.mapping, name=Region, Zone), by='name', match='first') %>%
      dcast(Zone~property, value.var = 'value', fun.aggregate = sum) 
    colnames(z.stats)[colnames(z.stats)=='Zone']='name'
  } else {
    z.data = rbind(total.zone.load, total.zone.imports, total.zone.exports, total.zone.ue)
    z.stats = z.data %>%
      dcast(name~property, value.var = 'value', fun.aggregate = sum)
  }
  return(z.stats)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Region and Zone Load
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Returns region level and zone level load data for the entire run. 

region_load = function(total.region.load) {
  r.load = select(total.region.load, name, value)
  return(r.load)
}

zone_load = function(total.region.load, total.zone.load) {
  if (reassign.zones==TRUE | total.zone.load=='ERROR'){
    z.load = total.region.load %>%
      join(select(region.zone.mapping, name=Region, Zone), by='name', match='first') %>%
      select(name=Zone, value) %>%
      group_by(name) %>%
      summarise(value = sum(value))
  } else {
    z.load = select(total.zone.load, name, value)
  }
  return(z.load)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Capacity Factor
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Calculates the capacity factor of all the generation types for the full run. 

capacity_factor = function(total.generation, total.installed.cap) {
  
  cf = rbind(total.generation, total.installed.cap)
  
  # Pull out installed capacity and generation and match them to generation type by generator name. 
  if ( use.gen.type.mapping.csv ) {
    mc = cf %>%
      filter(property == 'Installed Capacity') %>%
      rename(MaxCap = value) %>%
      join(gen.type.mapping, by = 'name')
    
    gen = cf %>%
      filter(property == 'Generation') %>%
      rename(Gen = value) %>%
      join(gen.type.mapping, by = 'name')
    
    # Same as above, but matches generation type according to PLEXOS type. 
  } else {
    mc = cf %>%
      filter(property == 'Installed Capacity') %>%
      rename(MaxCap = value) %>%
      join(category2type, by = 'category')
    
    gen = cf %>%
      filter(property == 'Generation') %>%
      rename(Gen = value) %>%
      join(category2type, by = 'category')
  }
    
  mc$Type = factor(mc$Type, levels = rev(c(gen.order)))
  
  # Calculates generation type total capacity and generation for the full run
  c.factor = mc %>%
    select(name, MaxCap, Type) %>%
    join(gen[,c('name', 'Gen')], by = 'name') %>%
    select(Type, MaxCap, Gen) %>%
    ddply('Type', summarise, MaxCap=sum(MaxCap), Gen=sum(Gen))  

  # Calculate capacity factor for each generation type
  n.int = length(seq(from = first.day, to = last.day, by = 'day'))*intervals.per.day
  c.factor$`Capacity Factor (%)` = c.factor$Gen/(c.factor$MaxCap/1000*n.int)*100
  
  c.factor = select(c.factor, Type, `Capacity Factor (%)`)
  
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
  
  # Query available capacity at the interval level, add generation type and region and zone by matching mapping file with generator names.
  if ( use.gen.type.mapping.csv ) {
    commit.data = interval.da.committment %>%
      select(time, name, category, value) %>%
      plyr::join(region.zone.mapping, by='name') %>%
      plyr::join(gen.type.mapping, by = 'name') %>%
      dcast(time+Region+Zone ~ Type, value.var = 'value', fun.aggregate = sum)
    
  } else {
    commit.data = interval.da.committment %>%
      select(time, name, category, value) %>%
      plyr::join(region.zone.mapping, by='name') %>%
      plyr::join(category2type, by = 'category') %>%
      dcast(time+Region+Zone ~ Type, value.var = 'value', fun.aggregate = sum)
  }
  
#   #make sure that the right zones and regions are there...
#   commit.data = merge(commit.data[,!names(commit.data) %in% names(rz.unique)[names(rz.unique) != spatialcol]],
#                   rz.unique,by=spatialcol,all.y=T)
  commit.data = melt(commit.data, id.vars = .(time, Region, Zone), variable.name = 'Type', value.name = 'committed.cap')
  
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
  total.gen = select(query_year(database, 'Generator', 'Generation', columns = c('category', 'name')), property, name, category, value)
  return(total.gen)
}

# Full run available capacity
total_avail_cap = function(database) {
  total.avail.cap = select(query_year(database, 'Generator', 'Available Energy', columns = c('category', 'name')), property, name, category, value)
  return(total.avail.cap)
}

# Full run emissions cost
total_emissions = function(database) {
  total.emissions.cost = select(query_year(database, 'Generator', 'Emissions Cost', columns = c('category', 'name')), property, name, category, value)
  return(total.emissions.cost)
}

# Full run fuel cost
total_fuel = function(database) {
  total.fuel.cost = select(query_year(database, 'Generator', 'Fuel Cost', columns = c('category', 'name')), property, name, category, value)
  return(total.fuel.cost)
}

# Full run S&S cost
total_ss = function(database) {
  total.ss.cost = select(query_year(database, 'Generator', 'Start & Shutdown Cost', columns = c('category', 'name')), property, name, category, value)
  return(total.ss.cost)
}

# Full run VO&M cost
total_vom = function(database) {
  total.vom.cost = select(query_year(database, 'Generator', 'VO&M Cost', columns = c('category', 'name')), property, name, category, value)
  return(total.vom.cost)
}

# Full run installed capacity
total_installed_cap = function(database) {
  total.installed.cap = select(query_year(database, 'Generator', 'Installed Capacity', columns = c('category', 'name')), property, name, category, value)
  return(total.installed.cap)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Region total run data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Full run region load
total_region_load = function(database) {
  total.region.load = select(query_year(database, 'Region', 'Load'), property, name, value)
  return(total.region.load)
}

# Full run region imports
total_region_imports = function(database) {
  total.region.imports = select(query_year(database, 'Region', 'Imports'), property, name, value)
  return(total.region.imports)
}

# Full run region exports
total_region_exports = function(database) {
  total.region.exports = select(query_year(database, 'Region', 'Exports'), property, name, value)
  return(total.region.exports)
}

# Full run region unserved energy
total_region_ue = function(database) {
  total.region.ue = select(query_year(database, 'Region', 'Unserved Energy'), property, name, value)
  return(total.region.ue)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Zone total run data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Full run zone load
total_zone_load = function(database) {
  total.zone.load = select(query_year(database, 'Zone', 'Load'), property, name, value)
  return(total.zone.load)
}

# Full run zone imports
total_zone_imports = function(database) {
  total.zone.imports = select(query_year(database, 'Zone', 'Imports'), property, name, value)
  return(total.zone.imports)
}

# Full run zone exports
total_zone_exports = function(database) {
  total.zone.exports = select(query_year(database, 'Zone', 'Exports'), property, name, value)
  return(total.zone.exports)
}

# Full run zone unserved energy
total_zone_ue = function(database) {
  total.zone.ue = select(query_year(database, 'Zone', 'Unserved Energy'), property, name, value)
  return(total.zone.ue)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Reserves total run data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Full run reserves provision
total_reserve_provision = function(database) {
  total.reserve.provision = select(query_year(database, 'Reserve', 'Provision'), property, name, value)
  return(total.reserve.provision)
}

# Full run reserves shortage
total_reserve_shortage = function(database) {
  total.reserve.shortage = select(query_year(database, 'Reserve', 'Shortage'), property, name, value)
  return(total.reserve.shortage)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Selected interface total run data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Full run interface flows
total_interface_flow = function(database) {
  total.interface = select(query_year(database, 'Interface', 'Flow'), property, name, value, time)
  return(total.interface)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Interval queries
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Interval level generator generation
interval_gen = function(database) {
  interval.gen = select(query_interval(database, 'Generator', 'Generation', columns = c('category', 'name')), 
                        property, name, value, time, category)
  return(interval.gen)
}

# Interval level generator capacity
interval_avail_cap = function(database) {
  interval.avail.cap = select(query_interval(database, 'Generator', 'Available Capacity', columns = c('category', 'name')), 
                              property, name, value, time, category)
  return(interval.avail.cap)
}

# Interval level region load 
interval_region_load = function(database) {
  interval.region.load = select(query_interval(database, 'Region', 'Load'), property, name, time, value)
  return(interval.region.load)
}

# Interval level region load and price
interval_region_price = function(database) {
  interval.region.price = select(query_interval(database, 'Region', 'Price'), property, name, time, value)
  return(interval.region.price)
}

# Interval level zone load
interval_zone_load = function(database) {
  interval.zone.load = select(query_interval(database, 'Zone', 'Load'), property, name, time, value)
  return(interval.zone.load)
}

# Interval level interface flows
interval_interface_flow = function(database) {
  interval.interface.flow = select(query_interval(database, 'Interface', 'Flow'), property, name, time, value)
  return(interval.interface.flow)
}

# Interval level reserve provisions
interval_reserve_provision = function(database) {
  interval.reserve.provision = select(query_interval(database, 'Reserve', 'Provision'), property, name, time, value)
  return(interval.reserve.provision)
}