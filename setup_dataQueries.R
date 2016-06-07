
yr.data.generator      = tryCatch( yr_gen_query(db), error = function(cond) { return('ERROR') } )
yr.data.region         = tryCatch( yr_region_query(db), error = function(cond) { return('ERROR') } )
yr.data.zone           = tryCatch( yr_zone_query(db), error = function(cond) { return('ERROR') } )
yr.data.reserve        = tryCatch( yr_reserve_query(db), error = function(cond) { return('ERROR') } )
yr.data.interface      = tryCatch( yr_interface_query(db), error = function(cond) { return('ERROR') } )

if ( key.period.dispatch.total | key.period.dispatch.region | key.period.dispatch.zone |
     yearly.curtailment | daily.curtailment | commit.dispatch ) {
  int.data.gen       = tryCatch( int_gen_query(db), error = function(cond) { return('ERROR') } )
  int.data.avail.cap = tryCatch( int_avail_cap_query(db), error = function(cond) { return('ERROR') } )
  int.data.region    = tryCatch( int_region_query(db), error = function(cond) { return('ERROR') } )
  int.data.zone    = tryCatch( int_zone_query(db), error = function(cond) { return('ERROR') } )
}

if ( interface.flow.plots | key.period.interface.flow.plots ) {
  int.data.interface = tryCatch( int_interface_query(db), error = function(cond) { return('ERROR') } )
}

if ( annual.reserves.table | reserves.plots ) {
  int.data.reserve = tryCatch( int_reserve_query(db), error = function(cond) { return('ERROR') } )
}

if ( price.duration.curve & !exists('int.data.region') ) {
  int.data.region = tryCatch( int_region_query(db), error = function(cond) { return('ERROR') } )
}

if ( commit.dispatch ) {
  int.data.commit = tryCatch( int_avail_cap_query(db.day.ahead), error = function(cond) { return('ERROR') } )
}

r.load = tryCatch( region_load(yr.data.region), error = function(cond) { return('ERROR') } )
z.load = tryCatch( zone_load(yr.data.region, yr.data.zone), error = function(cond) { return('ERROR') } )

region.names = region_stats(yr.data.region)$name
zone.names = zone_stats(yr.data.region, yr.data.zone)$name