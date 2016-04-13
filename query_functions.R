
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This file contains functions for the general PLEXOS solution analysis that reports an HTML of common figures.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Query General Data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# gen_query = function(database) {
#   
#   yr.data.generator = select(query_year(database, 'Generator', prop = c('Generation', 'Available Energy', 'Emissions Cost', 'Fuel Cost', 'Start & Shutdown Cost', 'VO&M Cost'), 
#                       columns = c('category', 'name')), property, name, category, value)
#   
#   yr.gen           = filter(yr.data.generator, property == 'Generation')
#   yr.avail.energy  = filter(yr.data.generator, property == 'Available Energy')
#   yr.emission.cost = filter(yr.data.generator, property == 'Emissions Cost')
#   yr.fuel.cost     = filter(yr.data.generator, property == 'Fuel Cost')
#   yr.ss.cost       = filter(yr.data.generator, property == 'Start & Shutdown Cost')
#   yr.vom.cost      = filter(yr.data.generator, property == 'VO&M Cost')
#   
#   yr.data.region = select(query_year
#   
# }

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Region Zone Load
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_by_type = function(database) {
  
  yr.data = query_year(database, 'Generator', prop = c('Generation', 'Available Energy'), columns = c('category', 'name') )
  
  yr.gen = yr.data %>%
    filter(property == 'Generation') %>%
    join(category2type, by = 'category') %>%
    select(Type, property, value)
  
  gen = yr.gen
  
  yr.gen = yr.gen %>%
    dcast(Type ~ property, sum) %>%
    rename(GWh = Generation) %>%
    ddply('Type', numcolwise(sum)) 
  
  avail = yr.data %>%
    filter(property == 'Available Energy') %>%
    join(category2type, by = 'category') %>%
    select(Type, property, value) %>%
    filter(Type %in% re.types ) %>%
    dcast(property ~ Type, sum)
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
 
region_zone_gen = function(database) {
  
  yr.data = query_year(database, 'Generator', prop = c('Generation', 'Available Energy'), columns = c('category', 'name') )
  
  gen.data = yr.data %>%
    filter(property=='Generation') %>%
    select(name, category, value) %>%
    plyr::join(region.zone.mapping, by='name') %>%
    plyr::join(category2type, by = 'category')

  return(gen.data)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Key Period Generation by Type 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interval_gen = function(database) {
  
  int.data = query_interval(database, 'Generator', prop = c('Generation', 'Available Capacity'), columns = c('category', 'name') )

  int.gen = int.data %>%
    filter(property == 'Generation') %>%
    select(name, time, value, category) %>%
    join(category2type, by = 'category') %>%
    dcast(time ~ Type, value.var = 'value', fun.aggregate = sum)
  
  re.gen = subset(int.gen, select = re.types)

  int.avail = int.data %>%
    filter(property == 'Available Capacity') %>%
    select(name, time, value, category) %>%
    join(category2type, by = 'category') %>%
    dcast(time ~ Type, sum) %>%
    subset(select = re.types)

  curtailed = int.avail - re.gen
  curtailed.total = data.frame(rowSums(curtailed))
  colnames(curtailed.total) = 'Curtailment'

  load = query_interval(database, 'Region', 'Load', columns = c('category', 'name') )
  load = dcast(load, time~property, value.var = 'value', fun.aggregate = sum)

  int.gen = int.gen %>%
    cbind(curtailed.total) %>%
    join(load, by = 'time') %>%
    melt(id.vars = 'time', variable.name = 'Type', value.name = 'value')

  return(int.gen)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Hourly Curtailment
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

daily_curtailment = function(database) {
  
  c.data = query_interval(database, 'Generator', prop = c('Generation', 'Available Capacity'), columns = c('category', 'name') )

  c.gen = c.data %>%
    filter(property == 'Generation') %>%
    join(category2type, by = 'category') %>%
    select(time, Type, value) %>%
    dcast(time ~ Type, value.var = 'value', fun.aggregate = sum) %>%
    subset(select = re.types)

  c.avail = c.data %>%
    filter(property == 'Available Capacity') %>%
    join(category2type, by = 'category') %>%
    select(time, Type, value) %>%
    dcast(time ~ Type, sum) %>%
    subset(select = re.types)

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

costs = function(database) {

  cost = sum_year(database, col = 'Generator', prop = c('Emissions Cost', 'Fuel Cost', 'Start & Shutdown Cost', 'VO&M Cost') ) 
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

region_zone_stats = function(database) {
  
  r.stats = query_year(database, 'Region', c('Load', 'Imports', 'Exports', 'Unserved Energy')  )
  z.stats = query_year(database, 'Zone', c('Load', 'Imports', 'Exports', 'Unserved Energy') )
  
  r.stats = dcast(r.stats, name~property, value.var = 'value', fun.aggregate = sum) 
  z.stats = dcast(z.stats, name~property, value.var = 'value', fun.aggregate = sum)
  
  r.stats$Type = 'Region'
  z.stats$Type = 'Zone'
  
  stats = rbind(r.stats, z.stats)
  
  return(stats)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Annual Reserve Provisions
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

annual_reserves = function(database) {
  
  r.data = query_year(database, col = 'Reserve', prop = c('Provision', 'Shortage') )
  
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

interval_reserves = function(database) {
  
  provision = query_interval(database, 'Reserve', 'Provision')
  provision = dcast(provision, time ~ name, value.var = 'value')
  return(provision)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Zone Interface Flows 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

zone_interface_flows = function(database) {
  
  zonal.interfaces = c('ER_NER_Interface', 'ER_SR_Interface', 'ER_W3_Interface', 'NR_ER_Interface', 'NR_WR_Interface', 'S1_S2_Interface', 'WR_SR_Interface')
  int.flows = query_interval(db, 'Interface', 'Flow')

  zonal.flow = int.flows %>%
    filter(name %in% zonal.interfaces) %>%
    select(name, time, value)

  region.flow = int.flows %>%
    filter(!name %in% zonal.interfaces) %>%
    select(name, time, value)

  zonal.flow$Type = 'Zone'
  region.flow$Type = 'Region'

  int.flows = rbind(zonal.flow, region.flow)

  year.flows = query_year(db, 'Interface', 'Flow', filter = list(name = zonal.interfaces))
  year.flows = select(year.flows, name, time, value)
  year.flows$Type = 'Annual_Zone'

  flows = rbind(int.flows, year.flows)
  flows$name = gsub('_Interface', '', flows$name)
  
  return(flows)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Region Zone Load
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

region_zone_load = function(database) {
  
  r.load = select(query_year(db, 'Region', 'Load'), name, value)
  z.load = select(query_year(db, 'Zone', 'Load'), name, value)
  
  r.load$Type = 'Region'
  z.load$Type = 'Zone'
  
  r.z.load = rbind(r.load, z.load)
  
  return(r.z.load)
  
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Capacity Factor
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

capacity_factor = function(database) {
  
  cf = select(query_year(db, 'Generator', c('Max Capacity', 'Generation'), columns = c('name', 'category')), property, name, value, category)
  
  mc = cf %>%
    filter(property == 'Max Capacity') %>%
    rename(MaxCap = value) %>%
    join(category2type, by = 'category')
  
  gen = cf %>%
    filter(property == 'Generation') %>%
    rename(Gen = value) %>%
    join(category2type, by = 'category')
    
  mc$Type = factor(mc$Type, levels = rev(c(gen.order)))
  
  c.factor = mc %>%
    select(name, MaxCap, Type) %>%
    join(gen[,c('name', 'Gen')], by = 'name') %>%
    select(Type, MaxCap, Gen) %>%
    ddply('Type', summarise, MaxCap=sum(MaxCap), Gen=sum(Gen))
  
  c.factor$`Capacity Factor (%)` = c.factor$Gen/(c.factor$MaxCap/1000*8760)*100
  
  c.factor = select(c.factor, Type, `Capacity Factor (%)`)
  
  return(c.factor)
  
}
