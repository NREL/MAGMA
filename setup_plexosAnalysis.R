
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

if ( use.gen.type.mapping.csv ) {
# Read mapping tile to map generator names to generation type
gen.type.mapping = read.csv(as.character(na.exclude(inputs$CSV.Gen.Type.File.Location)), stringsAsFactors=FALSE)
}

# Read mapping file to map generator names to region and zone (can be same file as gen name to type).
region.zone.mapping = read.csv(as.character(na.exclude(inputs$Gen.Region.Zone.Mapping.Filename)), stringsAsFactors=FALSE)

# Reassign generation type for plotting if desired, if using CSV to map generator names to generation type
reassign.type = data.frame(csv.type = as.character(na.omit(inputs$CSV.Gen.Type)), plot.type = as.character(na.omit(inputs$CSV.Desired.Type)) )

# Assign generation type according to PLEXOS category
category2type = data.frame(category = as.character(na.omit(inputs$PLEXOS.Gen.Category)), Type = as.character(na.omit(inputs$PLEXOS.Desired.Type)) )       

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

run.rplx=F
if(length(list.files(pattern = "\\.zip$",path=db.loc))!=0) {
  if(length(list.files(pattern = "\\.db$",path=db.loc))==0) {
    message(paste0('The .db file is absent from ',db.loc))
    run.rplx=T
  } else if(file.info(file.path(db.loc,list.files(pattern = "\\.db$",path=db.loc)))$mtime <
             file.info(file.path(db.loc,list.files(pattern = "\\.zip$",path=db.loc)))$mtime) {
    message(paste0('The db is older than the zip or the .db file in ',db.loc))
    run.rplx=T
  }
  if(run.rplx & readline('Do you want to run the rPLEXOS db creation tool now? (y/n):')=='y'){
    message('Running process_folder')
    process_folder(db.loc)
  } else {message('You need to run rPLEXOS to process your solution or point to the correct solution folder.')}
} else {message('No zip file... are you in the right directory?')}
# -----------------------------------------------------------------------
# Open the database file ( must already have created this using rplexos ) 
db = plexos_open(db.loc)
db = db[1,]
attributes(db)$class = c("rplexos","data.frame","tbl_df")
