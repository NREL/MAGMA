
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This file contains functions for comparing PLEXOS solutions that reports an HTML of common figures.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generation Difference by type
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_diff_by_type = function(total.generation, total.avail.cap) {
  
  #*************************** Potentially make this an input
  yr.gen = tryCatch( gen_by_type(total.generation, total.avail.cap), error = function(cond) { return('ERROR') } )
  all.combos = data.table(expand.grid(unique(yr.gen$scenario), unique(yr.gen$Type)))
  setkey(all.combos,Var1,Var2)
  setkey(yr.gen,scenario,Type)
  yr.gen = yr.gen[all.combos]
  yr.gen[is.na(GWh), GWh:=0]

  gen.diff = yr.gen[, GWh:=GWh-GWh[scenario==ref.scenario], by=.(Type)]

  return(gen.diff)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Region and Zone Generation by type according to generator name
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
region_zone_gen = function() {
  
  r.z.gen = yr.data.generator
  
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

  curt = gen.data %>%
    filter(Type %in% re.types ) %>%
    join(avail.data, by = 'name')
  
  curt$Type = 'Curtailment'
  curt$value = curt$Avail - curt$value
  curt$Avail = NULL
  
  gen.data = rbind(gen.data, curt)

  return(gen.data)
}



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Key Period Generation by Type 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interval_gen = function() {
  
  if ( use.gen.type.mapping.csv ) {
    int.gen = int.data.gen %>%
      filter(property == 'Generation') %>%
      select(name, time, value, category) %>%
      join(gen.type.mapping, by = 'name') %>%
      join(region.zone.mapping, by = 'name') %>%
      dcast(time+Region+Zone ~ Type, value.var = 'value', fun.aggregate = sum)
    
  } else {
    int.gen = int.data.gen %>%
      filter(property == 'Generation') %>%
      select(name, time, value, category) %>%
      join(category2type, by = 'category') %>%
      join(region.zone.mapping, by = 'name') %>%
      dcast(time+Region+Zone ~ Type, value.var = 'value', fun.aggregate = sum)
  }
  
  re.gen = subset(int.gen, select = re.types)

  if ( use.gen.type.mapping.csv ) {
    int.avail = int.data.avail.cap %>%
      filter(property == 'Available Capacity') %>%
      select(name, time, value, category) %>%
      join(gen.type.mapping, by = 'name') %>%
      join(region.zone.mapping, by = 'name') %>%
      dcast(time+Region+Zone ~ Type, sum) %>%
      subset(select = re.types)
    
  } else {
    int.avail = int.data.avail.cap %>%
      filter(property == 'Available Capacity') %>%
      select(name, time, value, category) %>%
      join(category2type, by = 'category') %>%
      join(region.zone.mapping, by = 'name') %>%
      dcast(time+Region+Zone ~ Type, sum) %>%
      subset(select = re.types)
  }

  curtailed = int.avail - re.gen
  curtailed = data.frame(rowSums(curtailed))
  colnames(curtailed) = 'Curtailment'
  
  load = dcast(int.data.region, time+name~property, value.var = 'value', fun.aggregate = sum)
  colnames(load)[colnames(load)=='name'] = 'Region'
  
  int.gen = int.gen %>%
    cbind(curtailed) %>%
    join(load, by = c('time', 'Region')) 
    # melt(id.vars = c('time', 'Region', 'Zone'), variable.name = 'Type', value.name = 'value') # Not necessary to melt here. 

  return(int.gen)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Total Curtailment
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

daily_curtailment = function() {
  
  gen.data = int.data.gen
  avail.data = int.data.avail.cap
  
  if ( use.gen.type.mapping.csv ) {
    c.gen = gen.data %>%
      filter(property == 'Generation') %>%
      join(gen.type.mapping, by = 'name') %>%
      select(time, Type, value) %>%
      dcast(time ~ Type, value.var = 'value', fun.aggregate = sum) %>%
      subset(select = re.types)
    
    c.avail = avail.data %>%
      filter(property == 'Available Capacity') %>%
      join(gen.type.mapping, by = 'name') %>%
      select(time, Type, value) %>%
      dcast(time ~ Type, sum) %>%
      subset(select = re.types)
    
  } else {
    c.gen = gen.data %>%
      filter(property == 'Generation') %>%
      join(category2type, by = 'category') %>%
      select(time, Type, value) %>%
      dcast(time ~ Type, value.var = 'value', fun.aggregate = sum) %>%
      subset(select = re.types)
    
    c.avail = avail.data %>%
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

costs_diff = function() {

  cost.table = tryCatch( costs(total.emissions.cost, total.fuel.cost, total.ss.cost, total.vom.cost), 
                         error = function(cond) { return('ERROR: costs function not returning correct results.') })
  cost.diff = cost.table[, .(scenario, `Cost (MM$)` = `Cost (MM$)` - `Cost (MM$)`[scenario == ref.scenario]), by=.(Type)]
  cost.diff.table = dcast.data.table(cost.diff, Type~scenario, value.var = 'Cost (MM$)')

return(cost.diff.table)
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
# Interface Flows 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interface_flows = function() {
  
  int.flows = int.data.interface %>%
    select(name, time, value) %>%
    filter(name %in% interfaces)
  
  year.flows = yr.data.interface.flow %>%
    select(name, time, value) %>%
    filter(name %in% interfaces)
  
  int.flows$Type = 'Interval_Flow'
  year.flows$Type = 'Annual_Flow'
    
  flows = rbind(int.flows, year.flows)
  
  return(flows)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Region and Zone Stats
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

region_stats = function() {
  r.data = yr.data.region
  r.stats = dcast(r.data, name~property, value.var = 'value', fun.aggregate = sum)
  return(r.stats)
}

zone_stats = function() {
  z.data = yr.data.region
  z.stats = z.data %>%
    join(select(region.zone.mapping, name=Region, Zone), by='name', match='first') %>%
    dcast(Zone~property, value.var = 'value', fun.aggregate = sum)
  colnames(z.stats)[colnames(z.stats)=='Zone']='name'
  return(z.stats)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Region and Zone Load
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

region_load = function() {
  r.data = yr.data.region
  r.load = r.data %>%
    filter(property == 'Load') %>%
    select(name, value)
  return(r.load)
}

zone_load = function() {
  z.data = yr.data.region
  z.load = z.data %>%
    filter(property == 'Load') %>%
    join(select(region.zone.mapping, name=Region, Zone), by='name', match='first') %>%
    select(name=Zone, value) %>%
    group_by(name) %>%
    summarise(value = sum(value))
  return(z.load)
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
