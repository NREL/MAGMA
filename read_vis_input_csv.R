pacman::p_load(ggplot2,reshape2,plyr,lubridate,scales,RSQLite,grid,knitr,
               markdown,rmarkdown,grid,gridExtra,RColorBrewer,snow,doParallel,xtable,
               data.table,dplyr,extrafont,tidyr,stringr,rplexos,rmarkdown,mailR) 

#-----------------------------------------------------------------------
  # Size for plot text
  text.plot = 11

# -----------------------------------------------------------------------
# Set ggplot theme
theme_set(theme_bw())

# Color scheme for line plots
scen.pal = c("goldenrod2", "blue", "darkblue", "firebrick3", "deeppink", "chartreuse2", "seagreen4")


read_vis_input_csv = function(input.csv){
  # -----------------------------------------------------------------------
  # Read CSV file with all inputs
  inputs <<- read.csv(file.path(input.csv))
  inputs[inputs==""]<<-NA
  
  # -----------------------------------------------------------------------
  # Read in the data from the input_data.csv file that was just loaded
  
  # Using CSV file to map generator types to names?
  use.gen.type.mapping.csv <<- as.logical(na.exclude(inputs$Using.Gen.Type.Mapping.CSV))
  
  # Reassign zones based on region to zone mapping file?
  reassign.zones <<- as.logical(na.exclude(inputs$reassign.zones))
  
  if ( use.gen.type.mapping.csv ) {
    # Read mapping tile to map generator names to generation type
    gen.type.mapping <<- read.csv(as.character(na.exclude(inputs$CSV.Gen.Type.File.Location)), stringsAsFactors=FALSE)
    gen.type.mapping <<- select(gen.type.mapping, name, Type)
  } else {
    # Assign generation type according to PLEXOS category
    category2type <<- data.frame(category = as.character(na.omit(inputs$PLEXOS.Gen.Category)), Type = as.character(na.omit(inputs$PLEXOS.Desired.Type)) )  
  }
  
  # Read mapping file to map generator names to region and zone (can be same file as gen name to type).
  region.zone.mapping <<- read.csv(as.character(na.exclude(inputs$Gen.Region.Zone.Mapping.Filename)), stringsAsFactors=FALSE)
  region.zone.mapping <<- select(region.zone.mapping, name, Region, Zone)
  rz.unique <<- unique(region.zone.mapping[,c('Region','Zone')])
  
  # Set plot color for each generation type
  Gen.col <<- data.frame(Type = na.omit(inputs$Gen.Type), Color = na.omit(inputs$Plot.Color) )
  gen.color<<-as.character(Gen.col$Color)
  names(gen.color)<<-Gen.col$Type
  
  # Generat type order for plots
  gen.order <<- rev(as.character(na.omit(inputs$Gen.Order))) 
  
  # Types of renewables to be considered for curtailment calculations
  re.types <<- as.character(na.omit(inputs$Renewable.Types.for.Curtailment)) 
  
  # Names of key periods
  period.names <<- as.character(na.omit(inputs$Key.Periods)) 
  
  # Number of key periods
  n.periods <<- length(period.names) 
  
  # Start and end times for key periods
  start.end.times <<- data.frame(start = as.POSIXct( strptime( na.omit(inputs$Start.Time), format = '%m/%d/%Y %H:%M'), tz='UTC'), 
                               end = as.POSIXct( strptime( na.omit(inputs$End.Time), format = '%m/%d/%Y %H:%M'), tz='UTC' ) )
  
  # First and last day of simulation
  first.day <<- as.POSIXct( strptime( na.omit(inputs$Start.Day), format = '%m/%d/%Y %H:%M'), tz='UTC' )
  last.day <<- as.POSIXct( strptime( na.omit(inputs$End.Day), format = '%m/%d/%Y %H:%M'), tz='UTC' )
  
  # Number of intervals per day
  intervals.per.day <<- as.numeric(na.omit(inputs$Intervals.Per.Day))
  
  # What sections to execute code chunks for
  run.sections <<- na.omit(inputs$Sections.to.Run)
  
  # Location for saved figures
  fig.path.name <<- paste0(as.character(na.omit(inputs$Fig.Path)),'\\')
  
  # Location for saved caches
  cache.path.name <<- paste0(as.character(na.omit(inputs$Cache.Path)),'\\')
  
  # Zones to ignore for plotting
  ignore.zones <<- as.character(na.omit(inputs$Ignore.Zones))
  
  # Regions to ignore for plotting
  ignore.regions <<- as.character(na.omit(inputs$Ignore.Regions))
  
  # Interfaces to look at flows for
  interfaces <<- as.character(na.omit(inputs$Interfaces.for.Flows))
  
}