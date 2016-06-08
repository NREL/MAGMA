# This file has all the actual plexos database queries (from the rplexos R package)
# Some automatically happen while others (the ones that take long) are dependent on certain chunks being selected to run.
# See the query_functions.R file to see how each property is being queried.

yr.data.generator      = tryCatch( yr_gen_query(db), error = function(cond) { return('ERROR') } ) # Total generation, available energy, emissions cost, fuel cost, start and shutdown cost, variable operating and maintenane cost, and installed capacity.
yr.data.region         = tryCatch( yr_region_query(db), error = function(cond) { return('ERROR') } ) # Total region load, imports, exports, and unserved energy. 
yr.data.zone           = tryCatch( yr_zone_query(db), error = function(cond) { return('ERROR') } ) # Total zone load, ipmorts, exports, and unserved energy.
yr.data.reserve        = tryCatch( yr_reserve_query(db), error = function(cond) { return('ERROR') } ) # Total reserve provision and shortage.
yr.data.interface      = tryCatch( yr_interface_query(db), error = function(cond) { return('ERROR') } ) # Total interface flow for each selected interface


# If certain logicals are true, then the below interval queries will be called.

if ( key.period.dispatch.total.log | key.period.dispatch.region.log | key.period.dispatch.zone.log |
     yearly.curtailment | daily.curtailment | commit.dispatch ) {
  int.data.gen       = tryCatch( int_gen_query(db), error = function(cond) { return('ERROR') } ) # Interval level generation for each generator.
  int.data.avail.cap = tryCatch( int_avail_cap_query(db), error = function(cond) { return('ERROR') } ) # Interval level available capacity for each generator.
  int.data.region    = tryCatch( int_region_query(db), error = function(cond) { return('ERROR') } ) # interval level region load and price.
  int.data.zone    = tryCatch( int_zone_query(db), error = function(cond) { return('ERROR') } ) # Interval level zone load
}

if ( interface.flow.plots | key.period.interface.flow.plots ) {
  int.data.interface = tryCatch( int_interface_query(db), error = function(cond) { return('ERROR') } ) # Interval level interface flow for selected interfaces.
}

if ( annual.reserves.table | reserves.plots ) {
  int.data.reserve = tryCatch( int_reserve_query(db), error = function(cond) { return('ERROR') } ) # Interval level reserve provision
}

if ( price.duration.curve & !exists('int.data.region') ) {
  int.data.region = tryCatch( int_region_query(db), error = function(cond) { return('ERROR') } ) # Interval level region load and price. This is only called if one logical is true and it doesn't already exist.
}

if ( commit.dispatch ) {
  int.data.commit = tryCatch( int_avail_cap_query(db.day.ahead), error = function(cond) { return('ERROR') } ) # Interval level day ahead generator available capacity.
}

r.load = tryCatch( region_load(yr.data.region), error = function(cond) { return('ERROR') } ) # Total region load
z.load = tryCatch( zone_load(yr.data.region, yr.data.zone), error = function(cond) { return('ERROR') } ) # Total zone load

region.names = region_stats(yr.data.region)$name # Assign region names based on PLEXOS regions.
zone.names = zone_stats(yr.data.region, yr.data.zone)$name # Assign zone names based on PLEXOS regions or region to zone mapping file. 