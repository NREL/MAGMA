
if(interface.flow.table) {

  interface.flows = tryCatch( annual_interface_flows(yr.data.interface), error=function(cond) {return('ERROR: interface_flows query not returning correct results.')})
  
  if ( typeof(interface.flows)=='character' ) {
    annual.flows = interface.flows
  } else {
  
  annual.flows = interface.flows %>% 
    select(name, value)
  colnames(annual.flows)[2] = 'GWh'

  }

} else { print('Section not run according to input file.') }
