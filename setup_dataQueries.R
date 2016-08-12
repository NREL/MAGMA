# This file has all the actual plexos database queries (from the rplexos R package)
# Some automatically happen while others (the ones that take long) are dependent on certain chunks being selected to run.
# See the query_functions.R file to see how each property is being queried.

total.generation     = tryCatch( total_generation(db), error = function(cond) { return('ERROR') } ) # Total generation 
total.avail.cap      = tryCatch( total_avail_cap(db), error = function(cond) { return('ERROR') } ) # Total available energy
total.emissions.cost = tryCatch( total_emissions(db), error = function(cond) { return('ERROR') } ) # Total emissions cost
total.fuel.cost      = tryCatch( total_fuel(db), error = function(cond) { return('ERROR') } ) # Total fuel cost
total.ss.cost        = tryCatch( total_ss(db), error = function(cond) { return('ERROR') } ) # Total start and shutdown cost
total.vom.cost       = tryCatch( total_vom(db), error = function(cond) { return('ERROR') } ) # Total VO&M cost
total.gen.res        = tryCatch( total_gen_reserve_provision(db), error = function(cond) { return('ERROR') } ) # Total reserve provision by generator
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
     daily.curtailment | interval.curtailment | commit.dispatch.zone | commit.dispatch.region ) {
  interval.generation   = tryCatch( interval_gen(db), error = function(cond) { return('ERROR') } ) # Interval level generation for each generator.
  interval.avail.cap    = tryCatch( interval_avail_cap(db), error = function(cond) { return('ERROR') } ) # Interval level available capacity for each generator.
}

if ( key.period.dispatch.total.log | key.period.dispatch.region.log | key.period.dispatch.zone.log |
     commit.dispatch.zone | commit.dispatch.region ){
  interval.region.load  = tryCatch( interval_region_load(db), error = function(cond) { return('ERROR') } ) # interval level region load.
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

r.load = tryCatch( region_load(total.region.load), error = function(cond) { return('ERROR') } ) # Total region load
z.load = tryCatch( zone_load(total.region.load, total.zone.load), error = function(cond) { return('ERROR') } ) # Total zone load

region.names = unique(r.load$name) # Assign region names based on PLEXOS regions.
zone.names = unique(z.load$name) # Assign zone names based on PLEXOS regions or region to zone mapping file. 
reserve.names = unique(total.reserve.provision$name) # Get all reserve types

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Check that all queries necessary worked. If not, return an error stating what data is not being output by PLEXOS
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if (typeof(total.generation)=='character') { message('\nMissing total generator generation data from solution .db file.')}
if (typeof(total.avail.cap)=='character') { message('\nMissing total generator available capacity data from solution .db file.')}
if (typeof(total.emissions.cost)=='character') { message('\nMissing total generator emissions cost data from solution .db file.')}
if (typeof(total.fuel.cost)=='character') { message('\nMissing total generator fuel cost data from solution .db file.')}
if (typeof(total.ss.cost)=='character') { message('\nMissing total generator start and shutdown cost data from solution .db file.')}
if (typeof(total.vom.cost)=='character') { message('\nMissing total generator VO&M cost data from solution .db file.')}
if (typeof(total.installed.cap)=='character') { message('\nMissing total generator installed capacity data from solution .db file.')}
if (typeof(total.region.load)=='character') { message('\nMissing total region load data from solution .db file.')}
if (typeof(total.region.imports)=='character') { message('\nMissing total region imports data from solution .db file.')}
if (typeof(total.region.exports)=='character') { message('\nMissing total region exports data from solution .db file.')}
if (typeof(total.region.ue)=='character') { message('\nMissing total region unserved energy data from solution .db file.')}
if (typeof(total.zone.load)=='character') { message('\nMissing total zone load data from solution .db file. Ok if reassign zones is TRUE and region data is found.')}
if (typeof(total.zone.imports)=='character') { message('\nMissing total zone imports data from solution .db file. Ok if reassign zones is TRUE and region data is found.')}
if (typeof(total.zone.exports)=='character') { message('\nMissing total zone exports data from solution .db file. Ok if reassign zones is TRUE and region data is found.')}
if (typeof(total.zone.ue)=='character') { message('\nMissing total zone unserved energy data from solution .db file. Ok if reassign zones is TRUE and region data is found.')}
if (typeof(total.reserve.provision)=='character') { message('\nMissing total reserve provision data from solution .db file.')}
if (typeof(total.reserve.shortage)=='character') { message('\nMissing total reserve shortage data from solution .db file.')}
if (typeof(total.interface.flow)=='character') { message('\nMissing total interface flow data from solution .db file.')}

if (exists('interval.generation')) { if (typeof(interval.generation)=='character') { message('\nMissing interval generator generation data from solution .db file.')}}
if (exists('interval.avail.cap')) { if (typeof(interval.avail.cap)=='character') { message('\nMissing interval generator available capacity data from solution .db file.')}}
if (exists('interval.region.load')) { if (typeof(interval.region.load)=='character') { message('\nMissing interval region load data from solution .db file.')}}
if (exists('interval.region.price')) { if (typeof(interval.region.price)=='character') { message('\nMissing interval region price data from solution .db file.')}}
if (exists('interval.zone.load')) { if (typeof(interval.zone.load)=='character') { message('\nMissing interval zone load data from solution .db file.')}}
if (exists('interval.interface.flow')) { if (typeof(interval.interface.flow)=='character') { message('\nMissing interval interface flow data from solution .db file.')}}
if (exists('interval.reserve.provision')) { if (typeof(interval.reserve.provision)=='character') { message('\nMissing interval reserve provision data from solution .db file.')}}
if (exists('interval.da.committment')) { if (typeof(interval.da.committment)=='character') { message('\nMissing interval available capacity from day ahead solution .db file.')}}

if( length(unique(rz.unique$Region))!=length(region.names) ) { message('\nWarning: Number of regions in generation to region/zone mapping file different than number of regions from region load query! Check region.names object.') }
if( length(unique(rz.unique$Zone))!=length(zone.names) ) { message('\nWarning: Number of zones in generation to region/zone mapping file different than number of zones from zone load query! Check zone.names object.') }
