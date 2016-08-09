
# Main setup file for general PLEXOS run analysis.

# -----------------------------------------------------------------------
library(pacman)
p_load(ggplot2, reshape2, plyr, lubridate, scales, RSQLite, grid, knitr, markdown, grid, gridExtra, RColorBrewer, snow,
       doParallel, xtable, data.table, dplyr, extrafont, tidyr, stringr, rplexos, rmarkdown)

# -----------------------------------------------------------------------
# Size for plot text
text.plot = 11

# -----------------------------------------------------------------------
# Set ggplot theme
theme_set(theme_bw())

# Color scheme for line plots
scen.pal = c("goldenrod2", "blue", "darkblue", "firebrick3", "deeppink", "chartreuse2", "seagreen4")

# -----------------------------------------------------------------------
# Read CSV file with all inputs
inputs = read.csv(file.path(input.csv))
inputs[inputs==""]=NA

# Assign a logical to each chunk run selector. 
run.sections = na.omit(inputs$Sections.to.Run)
if(1 %in% run.sections)  {total.gen.stack=TRUE}                 else {total.gen.stack=FALSE} 
if(2 %in% run.sections)  {zone.gen.stacks=TRUE}                 else {zone.gen.stacks=FALSE}
if(3 %in% run.sections)  {region.gen.stacks=TRUE}               else {region.gen.stacks=FALSE}
if(4 %in% run.sections)  {individual.region.stacks.log=TRUE}    else {individual.region.stacks.log=FALSE}
if(5 %in% run.sections)  {key.period.dispatch.total.log=TRUE}   else {key.period.dispatch.total.log=FALSE}
if(6 %in% run.sections)  {key.period.dispatch.zone.log=TRUE}    else {key.period.dispatch.zone.log=FALSE}
if(7 %in% run.sections)  {key.period.dispatch.region.log=TRUE}  else {key.period.dispatch.region.log=FALSE}
if(8 %in% run.sections)  {daily.curtailment=TRUE}               else {daily.curtailment=FALSE}
if(9 %in% run.sections)  {interval.curtailment=TRUE}            else {interval.curtailment=FALSE}
if(10 %in% run.sections) {annual.generation.table=TRUE}         else {annual.generation.table=FALSE}
if(11 %in% run.sections) {annual.cost.table=TRUE}               else {annual.cost.table=FALSE}
if(12 %in% run.sections) {region.zone.flow.table=TRUE}          else {region.zone.flow.table=FALSE}
if(13 %in% run.sections) {interface.flow.table=TRUE}            else {interface.flow.table=FALSE}
if(14 %in% run.sections) {interface.flow.plots=TRUE}            else {interface.flow.plots=FALSE}
if(15 %in% run.sections) {key.period.interface.flow.plots=TRUE} else {key.period.interface.flow.plots=FALSE}
if(16 %in% run.sections) {annual.reserves.table=TRUE}           else {annual.reserves.table=FALSE}
if(17 %in% run.sections) {reserves.plots=TRUE}                  else {reserves.plots=FALSE}
if(18 %in% run.sections) {region.zone.gen.table=TRUE}           else {region.zone.gen.table=FALSE}
if(19 %in% run.sections) {capacity.factor.table=TRUE}           else {capacity.factor.table=FALSE}
if(20 %in% run.sections) {price.duration.curve=TRUE}            else {price.duration.curve=FALSE}
if(21 %in% run.sections) {commit.dispatch.zone=TRUE}            else {commit.dispatch.zone=FALSE}
if(22 %in% run.sections) {commit.dispatch.region=TRUE}          else {commit.dispatch.region=FALSE}

# -----------------------------------------------------------------------
# Read in the data from the input_data.csv file that was just loaded


# location of database
db.loc = file.path(as.character(na.exclude(inputs$Database.Location))) 
db.day.ahead.loc = file.path(as.character(na.exclude(inputs$DayAhead.Database.Location)))
if (length(db.day.ahead.loc)==0) { db.day.ahead.loc = db.loc }

# Using CSV file to map generator types to names?
use.gen.type.mapping.csv = as.logical(na.exclude(inputs$Using.Gen.Type.Mapping.CSV))
if (length(use.gen.type.mapping.csv)==0) { 
  use.gen.type.mapping.csv = FALSE
  message('\nMust select TRUE or FALSE for if using generator generation type mapping file!') 
}

# Reassign zones based on region to zone mapping file?
reassign.zones = as.logical(na.exclude(inputs$reassign.zones))
if (length(reassign.zones)==0) { 
  reassign.zones = FALSE
  message('\nMust select TRUE or FALSE for if reassigning what regions are in what zones!')
}

if ( use.gen.type.mapping.csv ) {
  # Read mapping tile to map generator names to generation type
  gen.type.mapping = read.csv(as.character(na.exclude(inputs$CSV.Gen.Type.File.Location)), stringsAsFactors=FALSE)
  gen.type.mapping = select(gen.type.mapping, name, Type)
} else {
  # Assign generation type according to PLEXOS category
  category2type = data.frame(category = as.character(na.omit(inputs$PLEXOS.Gen.Category)), Type = as.character(na.omit(inputs$PLEXOS.Desired.Type)) )  
  if (length(category2type[,1])==0) { message('\nIf not using generator name to type mapping CSV, you must specify PLEXOS categories and desired generation type.') }
}

# Read mapping file to map generator names to region and zone (can be same file as gen name to type).
region.zone.mapping = read.csv(as.character(na.exclude(inputs$Gen.Region.Zone.Mapping.Filename)), stringsAsFactors=FALSE)
region.zone.mapping = select(region.zone.mapping, name, Region, Zone)
rz.unique = unique(region.zone.mapping[,c('Region','Zone')])

# Set plot color for each generation type
Gen.col = data.frame(Type = na.omit(inputs$Gen.Type), Color = na.omit(inputs$Plot.Color) )
gen.color<-as.character(Gen.col$Color)
names(gen.color)<-Gen.col$Type

# Generation type order for plots
gen.order = rev(as.character(na.omit(inputs$Gen.Order))) 

# Types of renewables to be considered for curtailment calculations
re.types = as.character(na.omit(inputs$Renewable.Types.for.Curtailment)) 
if (length(re.types)==0) { 
  message('\nNo variable generation types specified for curtailment.')
  interval.curtailment = FALSE
  daily.curtailment = FALSE
  re.types = 'none_specified'
}

# Types of generation to be plotted in the DA-RT committmet dispatch plots
da.rt.types = as.character(na.omit(inputs$DA.RT.Plot.Types))
if (length(da.rt.types)==0) {
  message('\nNo generation types specified for DA-RT plots. Plots will not be created.')
  commit.dispatch.region=FALSE
  commit.dispatch.zone=FALSE
}

# Names of key periods
period.names = as.character(na.omit(inputs$Key.Periods)) 
if (length(period.names)==0) {
  message('\nNo key periods specified. No plots will be created for these.')
  key.period.dispatch.total.log = FALSE
  key.period.dispatch.zone.log = FALSE
  key.period.dispatch.region.log = FALSE
  key.period.interface.flow.plots = FALSE
}

# Number of key periods
n.periods = length(period.names) 

# Start and end times for key periods
start.end.times = data.frame(start = as.POSIXct( strptime( na.omit(inputs$Start.Time), format = '%m/%d/%Y %H:%M'), tz='UTC'), 
                             end = as.POSIXct( strptime( na.omit(inputs$End.Time), format = '%m/%d/%Y %H:%M'), tz='UTC' ) )

# Location for saved figures
fig.path.name = paste0(as.character(na.omit(inputs$Fig.Path)),'\\')

# Zones to ignore for plotting
ignore.zones = as.character(na.omit(inputs$Ignore.Zones))

# Regions to ignore for plotting
ignore.regions = as.character(na.omit(inputs$Ignore.Regions))

# Interfaces to look at flows for
interfaces = as.character(na.omit(inputs$Interfaces.for.Flows))
if (length(interfaces)==0) {
  message('\nNo interfaces specified. No interface data will be shown.')
  interface.flow.table = FALSE
  interface.flow.plots = FALSE
  key.period.interface.flow.plots = FALSE
}

run.rplx=F
if(length(list.files(pattern = "\\.zip$",path=db.loc))!=0 ) {
  if(length(list.files(pattern = "\\.db$",path=db.loc))==0) {
    message(paste0('The .db file is absent from ',db.loc))
    run.rplx=T
  } else if(file.info(file.path(db.loc,list.files(pattern = "\\.db$",path=db.loc)))$mtime <
              file.info(file.path(db.loc,list.files(pattern = "\\.zip$",path=db.loc)))$mtime) {
    message(paste0('The db is older than the zip or the .db file in ',db.loc))
    run.rplx=T
  } else {message(paste0('\nFound .db solution file: ', list.files(pattern='\\.db$',path=db.loc), '\n'))}
  if(run.rplx) {
    if(readline('Do you want to run the rPLEXOS db creation tool now? (y/n):')=='y'){
      message('Running process_folder')
      process_folder(db.loc)
    } else {message('You need to run rPLEXOS to process your solution or point to the correct solution folder.')}
  } 
} else if (length(list.files(pattern = '\\.db$', path=db.loc))!=0 ) {
  message(paste0('\nFound .db solution file: ', list.files(pattern='\\.db$',path=db.loc), '\n'))
} else {message('No .zip or .db file... are you in the right directory?')}
# -----------------------------------------------------------------------
# Open the database file ( must already have created this using rplexos ) 
db = plexos_open(db.loc)
# db = db[1,] # This line queries only the first solution .db file if there are multiple in one location. 
attributes(db)$class = c("rplexos","data.frame","tbl_df")

# Open the day ahead database file
db.day.ahead = tryCatch(plexos_open(db.day.ahead.loc), error = function(cond) { return(data.frame('ERROR'))})
# db.day.ahead = db.day.ahead[1,] # This line queries only the first solution .db file if there are multiple in one location. 
attributes(db.day.ahead)$class = c('rplexos', 'data.frame', 'tbl_df')


# Calculate First and last day of simulation and interval length
model.timesteps = model_timesteps(db)
model.intervals = model.timesteps[,.(scenario,timestep)]

# Check to make sure no overlapping periods are created
if (nrow(model.timesteps[, .(unique(start)), by=.(scenario)]) > nrow(model.timesteps)){
  message("Warning: You have overlapping solutions")
}
model.timesteps = model.timesteps[,.(start=min(start),end=max(end)),by=.(scenario)]
# Check and make sure all scenarios have same start and end times
if (!(length(unique(model.timesteps$start))==1 & length(unique(model.timesteps$end))==1)){
  message("Warning: All specified scenarios do not have the same time horizons")
}
first.day = min(model.timesteps$start)
last.day = max(model.timesteps$end)

# Check to make sure all solutions have the same length
if (length(unique(model.intervals$timestep))!=1){
  message("Warning: Your databases do not have the same time intervals")
}
# Number of intervals per day
intervals.per.day = 24 / unique(as.numeric(model.intervals$timestep,units='hours'))





