# This file has all the actual plexos database queries (from the rplexos R package)
# Some automatically happen while others (the ones that take long) are dependent on certain chunks being selected to run.
# See the query_functions.R file to see how each property is being queried.

total.generation     = tryCatch( total_generation(db), error = function(cond) { return('ERROR') } ) # Total generation 
total.avail.cap      = tryCatch( total_avail_cap(db), error = function(cond) { return('ERROR') } ) # Total available energy
total.emissions.cost = tryCatch( total_emissions(db), error = function(cond) { return('ERROR') } ) # Total emissions cost
total.fuel.cost      = tryCatch( total_fuel(db), error = function(cond) { return('ERROR') } ) # Total fuel cost
total.ss.cost        = tryCatch( total_ss(db), error = function(cond) { return('ERROR') } ) # Total start and shutdown cost
total.vom.cost       = tryCatch( total_vom(db), error = function(cond) { return('ERROR') } ) # Total VO&M cost
total.installed.cap  = tryCatch( total_installed_cap(db), error = function(cond) { return('ERROR') } ) # Total installed capacity

total.region.load    = tryCatch( total_region_load(db), error = function(cond) { return('ERROR') } ) # Total region load 
total.region.imports = tryCatch( total_region_imports(db), error = function(cond) { return('ERROR') } ) # Total region imports
total.region.exports = tryCatch( total_region_exports(db), error = function(cond) { return('ERROR') } ) # Total region exports
total.region.ue      = tryCatch( total_region_ue(db), error = function(cond) { return('ERROR') } ) # Total region unserved energy.

total.zone.load    = tryCatch( total_zone_load(db), error = function(cond) { return('ERROR') } ) # Total zone load
total.zone.imports = tryCatch( total_zone_imports(db), error = function(cond) { return('ERROR') } ) # Total zone imports.
total.zone.exports = tryCatch( total_zone_exports(db), error = function(cond) { return('ERROR') } ) # Total zone exports
total.zone.ue      = tryCatch( total_zone_ue(db), error = function(cond) { return('ERROR') } ) # Total zone unserved energy.

total.reserve.provision = tryCatch( total_reserve_provision(db), error = function(cond) { return('ERROR') } ) # Total reserve provision.
total.reserve.shortage  = tryCatch( total_reserve_shortage(db), error = function(cond) { return('ERROR') } ) # Total reserve shortage.

total.interface.flow = tryCatch( total_interface_flow(db), error = function(cond) { return('ERROR') } ) # Total interface flow for each selected interface


# If certain logicals are true, then the below interval queries will be called.

if ( key.period.dispatch.total.log | key.period.dispatch.region.log | key.period.dispatch.zone.log |
     yearly.curtailment | daily.curtailment | commit.dispatch.zone | commit.dispatch.region ) {
  
  interval.generation   = tryCatch( interval_gen(db), error = function(cond) { return('ERROR') } ) # Interval level generation for each generator.
  interval.avail.cap    = tryCatch( interval_avail_cap(db), error = function(cond) { return('ERROR') } ) # Interval level available capacity for each generator.
  interval.region.load  = tryCatch( interval_region_load(db), error = function(cond) { return('ERROR') } ) # interval level region load.
  interval.region.price = tryCatch( interval_region_price(db), error = function(cond) { return('ERROR') } ) # interval level region price.
  interval.zone.load    = tryCatch( interval_zone_load(db), error = function(cond) { return('ERROR') } ) # Interval level zone load
}

if ( interface.flow.plots | key.period.interface.flow.plots ) {
  interval.interface.flow = tryCatch( interval_interface_flow(db), error = function(cond) { return('ERROR') } ) # Interval level interface flow for selected interfaces.
}

if ( annual.reserves.table | reserves.plots ) {
  interval.reserve.provision = tryCatch( interval_reserve_provision(db), error = function(cond) { return('ERROR') } ) # Interval level reserve provision
}

if ( price.duration.curve & !exists('interval.region.price') ) {
  interval.region.price = tryCatch( interval_region_price(db), error = function(cond) { return('ERROR') } ) # Interval level region price. This is only called if one logical is true and it doesn't already exist.
}

if ( commit.dispatch.zone | commit.dispatch.region ) {
  interval.da.committment = tryCatch( interval_avail_cap(db.day.ahead), error = function(cond) { return('ERROR') } ) # Interval level day ahead generator available capacity.
}

r.load = tryCatch( region_load(yr.data.region), error = function(cond) { return('ERROR') } ) # Total region load
z.load = tryCatch( zone_load(yr.data.region, yr.data.zone), error = function(cond) { return('ERROR') } ) # Total zone load

region.names = region_stats(yr.data.region)$name # Assign region names based on PLEXOS regions.
zone.names = zone_stats(yr.data.region, yr.data.zone)$name # Assign zone names based on PLEXOS regions or region to zone mapping file. 



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Check that all queries necessary worked. If not, return an error stating what data is not being output by PLEXOS
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if (typeof(total.generation)=='character') { print('Missing total generator generation data from solution .db file.')}
if (typeof(total.avail.cap)=='character') { print('Missing total generator available capacity data from solution .db file.')}
if (typeof(total.emissions.cost)=='character') { print('Missing total generator emissions cost data from solution .db file.')}
if (typeof(total.fuel.cost)=='character') { print('Missing total generator fuel cost data from solution .db file.')}
if (typeof(total.ss.cost)=='character') { print('Missing total generator start and shutdown cost data from solution .db file.')}
if (typeof(total.vom.cost)=='character') { print('Missing total generator VO&M cost data from solution .db file.')}
if (typeof(total.installed.cap)=='character') { print('Missing total generator installed capacity data from solution .db file.')}
if (typeof(total.region.load)=='character') { print('Missing total region load data from solution .db file.')}
if (typeof(total.region.imports)=='character') { print('Missing total region imports data from solution .db file.')}
if (typeof(total.region.exports)=='character') { print('Missing total region exports data from solution .db file.')}
if (typeof(total.region.ue)=='character') { print('Missing total region unserved energy data from solution .db file.')}
if (typeof(total.zone.load)=='character') { print('Missing total zone load data from solution .db file. Ok if reassign zones is TRUE and region data is found.')}
if (typeof(total.zone.imports)=='character') { print('Missing total zone imports data from solution .db file. Ok if reassign zones is TRUE and region data is found.')}
if (typeof(total.zone.exports)=='character') { print('Missing total zone exports data from solution .db file. Ok if reassign zones is TRUE and region data is found.')}
if (typeof(total.zone.ue)=='character') { print('Missing total zone unserved energy data from solution .db file. Ok if reassign zones is TRUE and region data is found.')}
if (typeof(total.reserve.provision)=='character') { print('Missing total reserve provision data from solution .db file.')}
if (typeof(total.reserve.shortage)=='character') { print('Missing total reserve shortage data from solution .db file.')}
if (typeof(total.interface.flow)=='character') { print('Missing total interface flow data from solution .db file.')}

if (exists('interval.generation') & typeof(interval.generation)=='character') { print('Missing interval generator generation data from solution .db file.')}
if (exists('interval.avail.cap') & typeof(interval.avail.cap)=='character') { print('Missing interval generator available capacity data from solution .db file.')}
if (exists('interval.region.load') & typeof(interval.region.load)=='character') { print('Missing interval region load data from solution .db file.')}
if (exists('interval.region.price') & typeof(interval.region.price)=='character') { print('Missing interval region price data from solution .db file.')}
if (exists('interval.zone.load') & typeof(interval.zone.load)=='character') { print('Missing interval zone load data from solution .db file.')}
if (exists('interval.interface.flow') & typeof(interval.interface.flow)=='character') { print('Missing interval interface flow data from solution .db file.')}
if (exists('interval.reserve.provision') & typeof(interval.reserve.provision)=='character') { print('Missing interval reserve provision data from solution .db file.')}
if (exists('interval.da.committment') & typeof(interval.da.committment)=='character') { print('Missing interval available capacity from day ahead solution .db file.')}

