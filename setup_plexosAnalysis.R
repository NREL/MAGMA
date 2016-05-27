
# Main setup file for general PLEXOS run analysis.

# -----------------------------------------------------------------------
library(ggplot2, quietly=TRUE, warn.conflicts=FALSE)
library(reshape2, quietly=TRUE, warn.conflicts=FALSE)
library(plyr, quietly=FALSE, warn.conflicts=FALSE)
library(lubridate, quietly=TRUE, warn.conflicts=FALSE)
library(scales, quietly=TRUE, warn.conflicts=FALSE)
library(RSQLite, quietly=TRUE, warn.conflicts=FALSE)
library(grid, quietly=TRUE, warn.conflicts=FALSE)
library(knitr, quietly=TRUE, warn.conflicts=FALSE)
library(markdown, quietly=TRUE, warn.conflicts=FALSE)
library(grid, quietly=TRUE, warn.conflicts=FALSE)
library(gridExtra, quietly=TRUE, warn.conflicts=FALSE)
library(RColorBrewer, quietly=TRUE, warn.conflicts=FALSE)
#library(RSQLite.extfuns, quietly=TRUE, warn.conflicts=FALSE)
library(snow, quietly=TRUE, warn.conflicts=FALSE)
library(doParallel, quietly=TRUE, warn.conflicts=FALSE)
library(xtable, quietly=TRUE, warn.conflicts=FALSE)
library(data.table, quietly=TRUE, warn.conflicts=FALSE)
library(dplyr, quietly=FALSE, warn.conflicts=FALSE)
library(extrafont, quietly = TRUE, warn.conflicts = FALSE)
library(tidyr, quietly = T, warn.conflicts = T)
library(stringr, quietly = T, warn.conflicts = T)
library(rplexos, quietly = F, warn.conflicts = F)
library(rmarkdown)

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

# -----------------------------------------------------------------------
# Read in the data from the input_data.csv file that was just loaded

# location of database
db.loc = file.path(as.character(na.exclude(inputs$Database.Location))) 

# Using CSV file to map generator types to names?
use.gen.type.mapping.csv = as.logical(na.exclude(inputs$Using.Gen.Type.Mapping.CSV))

# Reassign zones based on region to zone mapping file?
reassign.zones = as.logical(na.exclude(inputs$reassign.zones))

if ( use.gen.type.mapping.csv ) {
  # Read mapping tile to map generator names to generation type
  gen.type.mapping = read.csv(as.character(na.exclude(inputs$CSV.Gen.Type.File.Location)), stringsAsFactors=FALSE)
  gen.type.mapping = select(gen.type.mapping, name, Type)
} else {
  # Assign generation type according to PLEXOS category
  category2type = data.frame(category = as.character(na.omit(inputs$PLEXOS.Gen.Category)), Type = as.character(na.omit(inputs$PLEXOS.Desired.Type)) )  
}

# Read mapping file to map generator names to region and zone (can be same file as gen name to type).
region.zone.mapping = read.csv(as.character(na.exclude(inputs$Gen.Region.Zone.Mapping.Filename)), stringsAsFactors=FALSE)
region.zone.mapping = select(region.zone.mapping, name, Region, Zone)
rz.unique = unique(region.zone.mapping[,c('Region','Zone')])

# Set plot color for each generation type
Gen.col = data.frame(Type = na.omit(inputs$Gen.Type), Color = na.omit(inputs$Plot.Color) )
gen.color<-as.character(Gen.col$Color)
names(gen.color)<-Gen.col$Type

# Generat type order for plots
gen.order = rev(as.character(na.omit(inputs$Gen.Order))) 

# Types of renewables to be considered for curtailment calculations
re.types = as.character(na.omit(inputs$Renewable.Types.for.Curtailment)) 

# Names of key periods
period.names = as.character(na.omit(inputs$Key.Periods)) 

# Number of key periods
n.periods = length(period.names) 

# Start and end times for key periods
start.end.times = data.frame(start = as.POSIXct( strptime( na.omit(inputs$Start.Time), format = '%m/%d/%Y %H:%M'), tz='UTC'), 
                             end = as.POSIXct( strptime( na.omit(inputs$End.Time), format = '%m/%d/%Y %H:%M'), tz='UTC' ) )

# First and last day of simulation
first.day = as.POSIXct( strptime( na.omit(inputs$Start.Day), format = '%m/%d/%Y %H:%M'), tz='UTC' )
last.day = as.POSIXct( strptime( na.omit(inputs$End.Day), format = '%m/%d/%Y %H:%M'), tz='UTC' )

# Number of intervals per day
intervals.per.day = as.numeric(na.omit(inputs$Intervals.Per.Day))

# What sections to execute code chunks for
run.sections = na.omit(inputs$Sections.to.Run)

# Location for saved figures
fig.path.name = paste0(as.character(na.omit(inputs$Fig.Path)),'\\')

# Location for saved caches
cache.path.name = paste0(as.character(na.omit(inputs$Cache.Path)),'\\')

# Zones to ignore for plotting
ignore.zones = as.character(na.omit(inputs$Ignore.Zones))

# Regions to ignore for plotting
ignore.regions = as.character(na.omit(inputs$Ignore.Regions))

# Interfaces to look at flows for
interfaces = as.character(na.omit(inputs$Interfaces.for.Flows))

run.rplx=F
if(length(list.files(pattern = "\\.zip$",path=db.loc))!=0 ) {
  if(length(list.files(pattern = "\\.db$",path=db.loc))==0) {
    message(paste0('The .db file is absent from ',db.loc))
    run.rplx=T
  } else if(file.info(file.path(db.loc,list.files(pattern = "\\.db$",path=db.loc)))$mtime <
              file.info(file.path(db.loc,list.files(pattern = "\\.zip$",path=db.loc)))$mtime) {
    message(paste0('The db is older than the zip or the .db file in ',db.loc))
    run.rplx=T
  }
  if(run.rplx) {
    if(readline('Do you want to run the rPLEXOS db creation tool now? (y/n):')=='y'){
      message('Running process_folder')
      process_folder(db.loc)
    } else {message('You need to run rPLEXOS to process your solution or point to the correct solution folder.')}
  } 
} else if (length(list.files(pattern = '\\.db$', path=db.loc))!=0 ) {
  message(paste0('Found .db solution file: ', list.files(pattern='\\.db$',path=db.loc)))
} else {message('No .zip or .db file... are you in the right directory?')}
# -----------------------------------------------------------------------
# Open the database file ( must already have created this using rplexos ) 
db = plexos_open(db.loc)
db = db[1,]
attributes(db)$class = c("rplexos","data.frame","tbl_df")

run.sections = na.omit(inputs$Sections.to.Run)
if(1 %in% run.sections)  {total.gen.stack=TRUE}                 else {total.gen.stack=FALSE} 
if(2 %in% run.sections)  {zone.gen.stacks=TRUE}                 else {zone.gen.stacks=FALSE}
if(3 %in% run.sections)  {region.gen.stacks=TRUE}               else {region.gen.stacks=FALSE}
if(4 %in% run.sections)  {individual.region.stacks=TRUE}        else {individual.region.stacks=FALSE}
if(5 %in% run.sections)  {key.period.dispatch.total=TRUE}       else {key.period.dispatch.total=FALSE}
if(6 %in% run.sections)  {key.period.dispatch.region=TRUE}      else {key.period.dispatch.region=FALSE}
if(7 %in% run.sections)  {key.period.dispatch.zone=TRUE}        else {key.period.dispatch.zone=FALSE}
if(8 %in% run.sections)  {yearly.curtailment=TRUE}              else {yearly.curtailment=FALSE}
if(9 %in% run.sections)  {daily.curtailment=TRUE}               else {daily.curtailment=FALSE}
if(10 %in% run.sections) {annual.generation.table=TRUE}         else {annual.generation.table=FALSE}
if(11 %in% run.sections) {annual.cost.table=TRUE}               else {annual.cost.table=FALSE}
if(12 %in% run.sections) {region.zone.flow.tables=TRUE}         else {region.zone.flow.tables=FALSE}
if(13 %in% run.sections) {interface.flow.table=TRUE}            else {interface.flow.table=FALSE}
if(14 %in% run.sections) {interface.flow.plots=TRUE}            else {interface.flow.plots=FALSE}
if(15 %in% run.sections) {key.period.interface.flow.plots=TRUE} else {key.period.interface.flow.plots=FALSE}
if(16 %in% run.sections) {annual.reserves.table=TRUE}           else {annual.reserves.table=FALSE}
if(17 %in% run.sections) {reserves.plots=TRUE}                  else {reserves.plots=FALSE}
if(18 %in% run.sections) {region.zone.gen.table=TRUE}           else {region.zone.gen.table=FALSE}
if(19 %in% run.sections) {capacity.factor.table=TRUE}           else {capacity.factor.table=FALSE}
if(20 %in% run.sections) {price.duration.curve=TRUE}            else {price.duration.curve=FALSE}




