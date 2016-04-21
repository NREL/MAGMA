
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This file contains functions for the general PLEXOS solution analysis that reports an HTML of common figures.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Query General Data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

yr_gen_query = function(database) {
  yr.data.generator = select(query_year(database, 'Generator', 
                                        prop = c('Generation', 'Available Energy', 'Emissions Cost', 'Fuel Cost', 'Start & Shutdown Cost', 'VO&M Cost', 'Installed Capacity'),
                                        columns = c('category', 'name')), property, name, category, value)
  return(yr.data.generator)
}

yr_region_query = function(database) {
  yr.data.region = select(query_year(database, 'Region', c('Load', 'Imports', 'Exports', 'Unserved Energy')), property, name, value)
  return(yr.data.region)
}
  
yr_zone_query = function(database) {
  yr.data.zone = select(query_year(database, 'Zone', c('Load', 'Imports', 'Exports', 'Unserved Energy')), property, name, value)
  return(yr.data.zone)
}
  
yr_reserve_query = function(database) {
  yr.data.reserve = select(query_year(database, 'Reserve', c('Provision', 'Shortage')), property, name, value)
  return(yr.data.reserve)
}

yr_interface_query = function(database) {
  yr.data.interface.flow = select(query_year(database, 'Interface', 'Flow'), property, name, value, time)
  return(yr.data.interface.flow)
}

int_gen_query = function(database) {
  int.data.gen = select(query_interval(database, 'Generator', 'Generation', columns = c('category', 'name')), 
                              property, name, value, time, category)
  return(int.data.gen)
}

int_avail_cap_query = function(database) {
  int.data.avail.cap = select(query_interval(database, 'Generator', 'Available Capacity', columns = c('category', 'name')), 
                              property, name, value, time, category)
  return(int.data.avail.cap)
}

int_region_query = function(database) {
  int.data.region = select(query_interval(database, 'Region', 'Load'), property, name, time, value)
  return(int.data.region)
}

int_interface_query = function(database) {
  int.data.interface = select(query_interval(database, 'Interface', 'Flow'), property, name, time, value)
  return(int.data.interface)
}

int_reserve_query = function(database) {
  int.data.reserve = select(query_interval(database, 'Reserve', 'Provision'), property, name, time, value)
  return(int.data.reserve)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Region Zone Load
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_by_type = function() {
  
  yr.data = yr.data.generator
  
  if ( use.gen.type.mapping.csv ) {
    yr.gen = yr.data %>%
      filter(property == 'Generation') %>%
      join(gen.type.mapping, by = 'name') %>%
      select(Type, property, value)
  } else {
    yr.gen = yr.data %>%
      filter(property == 'Generation') %>%
      join(category2type, by = 'category') %>%
      select(Type, property, value)
  }
  
  gen = yr.gen
  
  yr.gen = yr.gen %>%
    dcast(Type ~ property, sum) %>%
    rename(GWh = Generation) %>%
    ddply('Type', numcolwise(sum)) 
  
  if ( use.gen.type.mapping.csv ) {
    avail = yr.data %>%
      filter(property == 'Available Energy') %>%
      join(gen.type.mapping, by = 'name') %>%
      select(Type, property, value) %>%
      filter(Type %in% re.types ) %>%
      dcast(property ~ Type, sum)
  } else {
    avail = yr.data %>%
      filter(property == 'Available Energy') %>%
      join(category2type, by = 'category') %>%
      select(Type, property, value) %>%
      filter(Type %in% re.types ) %>%
      dcast(property ~ Type, sum)
  }
  avail = avail[,2:ncol(avail)]
  
  gen = gen %>%
    filter(Type %in% re.types ) %>%
    dcast(property ~ Type, sum)
  gen = gen[,2:ncol(gen)]
  
  curt = avail - gen
  curt.tot = sum(curt)

  yr.gen = rbind(yr.gen, data.frame(Type = 'Curtailment', GWh = curt.tot))
  
  return(yr.gen)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Region and Zone name mapping
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
region_zone_gen = function() {
  
  if ( use.gen.type.mapping.csv ) {
    gen.data = yr.data.generator %>%
      filter(property=='Generation') %>%
      select(name, category, value) %>%
      plyr::join(region.zone.mapping, by='name') %>%
      plyr::join(gen.type.mapping, by = 'name')  
    
  } else {
    gen.data = yr.data.generator %>%
      filter(property=='Generation') %>%
      select(name, category, value) %>%
      plyr::join(region.zone.mapping, by='name') %>%
      plyr::join(category2type, by = 'category')
  }

  return(gen.data)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Key Period Generation by Type 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interval_gen = function() {
  
  int.data = int.data.gen
  load.data = int.data.region
  
  if ( use.gen.type.mapping.csv ) {
    int.gen = int.data %>%
      filter(property == 'Generation') %>%
      select(name, time, value, category) %>%
      join(gen.type.mapping, by = 'name') %>%
      dcast(time ~ Type, value.var = 'value', fun.aggregate = sum)
    
  } else {
    int.gen = int.data %>%
      filter(property == 'Generation') %>%
      select(name, time, value, category) %>%
      join(category2type, by = 'category') %>%
      dcast(time ~ Type, value.var = 'value', fun.aggregate = sum)
  }
  
  re.gen = subset(int.gen, select = re.types)

  if ( use.gen.type.mapping.csv ) {
    int.avail = int.data %>%
      filter(property == 'Available Capacity') %>%
      select(name, time, value, category) %>%
      join(gen.type.mapping, by = 'name') %>%
      dcast(time ~ Type, sum) %>%
      subset(select = re.types)
    
  } else {
    int.avail = int.data %>%
      filter(property == 'Available Capacity') %>%
      select(name, time, value, category) %>%
      join(category2type, by = 'category') %>%
      dcast(time ~ Type, sum) %>%
      subset(select = re.types)
  }

  curtailed = int.avail - re.gen
  curtailed.total = data.frame(rowSums(curtailed))
  colnames(curtailed.total) = 'Curtailment'

  load = dcast(load.data, time~property, value.var = 'value', fun.aggregate = sum)

  int.gen = int.gen %>%
    cbind(curtailed.total) %>%
    join(load, by = 'time') %>%
    melt(id.vars = 'time', variable.name = 'Type', value.name = 'value')

  return(int.gen)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Hourly Curtailment
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

daily_curtailment = function() {
  
  if ( use.gen.type.mapping.csv ) {
    c.gen = int.data.gen %>%
      filter(property == 'Generation') %>%
      join(gen.type.mapping, by = 'name') %>%
      select(time, Type, value) %>%
      dcast(time ~ Type, value.var = 'value', fun.aggregate = sum) %>%
      subset(select = re.types)
    
    c.avail = int.data.avail.cap %>%
      filter(property == 'Available Capacity') %>%
      join(gen.type.mapping, by = 'name') %>%
      select(time, Type, value) %>%
      dcast(time ~ Type, sum) %>%
      subset(select = re.types)
    
  } else {
    c.gen = int.data.gen %>%
      filter(property == 'Generation') %>%
      join(category2type, by = 'category') %>%
      select(time, Type, value) %>%
      dcast(time ~ Type, value.var = 'value', fun.aggregate = sum) %>%
      subset(select = re.types)
    
    c.avail = int.data.avail.cap %>%
      filter(property == 'Available Capacity') %>%
      join(category2type, by = 'category') %>%
      select(time, Type, value) %>%
      dcast(time ~ Type, sum) %>%
      subset(select = re.types)  
  }

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

costs = function() {

  cost = yr.data.generator
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
# Regional and Zone Stats
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

region_zone_stats = function() {
  
  r.data = yr.data.region
  z.data = yr.data.zone
  
  r.stats = dcast(r.data, name~property, value.var = 'value', fun.aggregate = sum) 
  z.stats = dcast(z.data, name~property, value.var = 'value', fun.aggregate = sum)
  
  r.stats$Type = 'Region'
  z.stats$Type = 'Zone'
  
  stats = rbind(r.stats, z.stats)
  
  return(stats)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Annual Reserve Provisions
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

annual_reserves = function() {
  
  r.data = yr.data.reserve
  
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

interval_reserves = function() {
  
  provision = int.data.reserve
  provision = dcast(provision, time ~ name, value.var = 'value')
  return(provision)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Zone Interface Flows 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

zone_interface_flows = function() {
  
  zonal.interfaces = c('ER_NER_Interface', 'ER_SR_Interface', 'ER_W3_Interface', 'NR_ER_Interface', 'NR_WR_Interface', 'S1_S2_Interface', 'WR_SR_Interface')
  int.flows = int.data.interface
  year.flows = yr.data.interface.flow

  zonal.flow = int.flows %>%
    filter(name %in% zonal.interfaces) %>%
    select(name, time, value)

  region.flow = int.flows %>%
    filter(!name %in% zonal.interfaces) %>%
    select(name, time, value)

  zonal.flow$Type = 'Zone'
  region.flow$Type = 'Region'

  int.flows = rbind(zonal.flow, region.flow)

  year.flows = year.flows %>%
    filter(name %in% zonal.interfaces) %>%
    select(name, time, value)
  year.flows$Type = 'Annual_Zone'

  flows = rbind(int.flows, year.flows)
  flows$name = gsub('_Interface', '', flows$name)
  
  return(flows)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Region Zone Load
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

region_zone_load = function() {
  
  r.load = yr.data.region %>%
    filter(property == 'Load') %>%
    select(name, value)
  
  z.load = yr.data.zone %>%
    filter(property == 'Load') %>%
    select(name, value)
  
  r.load$Type = 'Region'
  z.load$Type = 'Zone'
  
  r.z.load = rbind(r.load, z.load)
  
  return(r.z.load)
  
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Capacity Factor
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

capacity_factor = function() {
  
  cf = yr.data.generator
  
  if ( use.gen.type.mapping.csv ) {
    mc = cf %>%
      filter(property == 'Installed Capacity') %>%
      rename(MaxCap = value) %>%
      join(gen.type.mapping, by = 'name')
    
    gen = cf %>%
      filter(property == 'Generation') %>%
      rename(Gen = value) %>%
      join(gen.type.mapping, by = 'name')
    
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
  
  c.factor = mc %>%
    select(name, MaxCap, Type) %>%
    join(gen[,c('name', 'Gen')], by = 'name') %>%
    select(Type, MaxCap, Gen) %>%
    ddply('Type', summarise, MaxCap=sum(MaxCap), Gen=sum(Gen))  

  n.int = length(seq(from = first.day, to = last.day, by = 'day'))*intervals.per.day
  c.factor$`Capacity Factor (%)` = c.factor$Gen/(c.factor$MaxCap/1000*n.int)*100
  
  c.factor = select(c.factor, Type, `Capacity Factor (%)`)
  
  return(c.factor)
  
}
