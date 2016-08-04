# Check if this section was selected to run in the input file
if(interface.flow.table) {

  # Call the query function to get interface flows for the interfaces selected in the input file.
  interface.flows = tryCatch( annual_interface_flows(total.interface.flow), error=function(cond) {return('ERROR: interface_flows query not returning correct results.')})
  
  if ( typeof(interface.flows)=='character' ) {
    annual.flows = interface.flows
  } else {
  
  annual.flows = interface.flows[,.(name,GWh=value)]

  }

} else { print('Section not run according to input file.') }
