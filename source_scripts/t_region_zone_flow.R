# Check if this section was selected to run in the input file
if (region.zone.flow.table) {

  # Query region and zone stats.
  r.stats = tryCatch( region_stats(yr.data.region), error = function(cond) { return('ERROR: region_stats function not returning correct results.') })
  z.stats = tryCatch( zone_stats(yr.data.region, yr.data.zone), error = function(cond) { return('ERROR: zone_stats function not returning correct results.') })
  
  if ( !typeof(r.stats)=='character' ) { 
    colnames(r.stats)[which(colnames(r.stats)=='name')] = 'Region Name'
  }
  
  if ( !typeof(z.stats)=='character' ) { 
    colnames(z.stats)[which(colnames(z.stats)=='name')] = 'Zone Name'
  }

} else { print('Section not run according to input file.') }
