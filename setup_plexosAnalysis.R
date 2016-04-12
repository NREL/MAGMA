
# Main setup file for general PLEXOS run analysis. You can change these settings to suit your needs.

#########################
# Must Report These items:
#########################

### Annual:

## Generator:
# Generation
# Available Energy
# Emission Cost
# Fuel Cost
# Start and Shutdown Cost
# VOM Cost

## Region:
# Load
# Imports
# Exports
# Unserved Energy

## Reserves:
# Provisions
# Shortage

### Interval:

## Generator:
# Generation
# Available Capacity

## Region:
# Load  

## Reserves:
# Provisions


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

# -----------------------------------------------------------------------
# Set ggplot theme
theme_set(theme_bw())

rainbow12equal = c("#BF4D4D", "#BF864D", "#BFBF4D", "#86BF4D", "#4DBF4D", "#4DBF86", "#4DBFBF", "#4D86BF", "#4D4DBF", "#864DBF", "#BF4DBF", "#BF4D86")
tol3qualitative=c("#4477AA", "#DDCC77", "#CC6677")
tol4qualitative=c("#4477AA", "#117733", "#DDCC77", "#CC6677")
tol5qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677")
tol6qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol7qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol12qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499")
tol21rainbow= c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")

pales=c("lightsteelblue1", "lightsalmon", "honeydew1", "darkseagreen1", "plum1", "thistle", "mistyrose1", "honeydew2", "khaki", "sienna1", "rosybrown1", "lightcyan1", "lightgoldenrod1")

Map.pal<-brewer.pal(9,'YlOrBr')
scen.pal = c("goldenrod2", "blue", "darkblue", "firebrick3", "deeppink", "chartreuse2", "seagreen4")

theme.PPT = theme(legend.key = element_rect(color = "grey80", size = 0.4),
                  legend.key.size = grid::unit(0.6, "lines"), text=element_text(size=12),
                  axis.text=element_text(size=7), axis.title=element_text(size=8, face=2), axis.title.x=element_text(vjust=-0.3))

# -----------------------------------------------------------------------
# Size for plot text
text.plot = 11

# -----------------------------------------------------------------------
# Read CSV file with all inputs
inputs = read.csv('../vis_input_data.csv')
inputs[inputs==""]=NA

# -----------------------------------------------------------------------
# Read in the data from the input_data.csv file that was just loaded

# location of database
db.loc = as.character(na.exclude(inputs$Database.Location)) 

# Using CSV file to map generator types to names?
use.gen.type.mapping.csv = as.logical(na.exclude(inputs$Using.Gen.Type.Mapping.CSV))

# Read mapping tile to map generator names to generation type
gen.type.mapping = read.csv(as.character(na.exclude(inputs$CSV.Gen.Type.File.Location)), stringsAsFactors=FALSE)

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

# -----------------------------------------------------------------------
# Open the database file ( must already have created this using rplexos ) 
db = plexos_open(db.loc)
db = db[1,]
attributes(db)$class = c("rplexos","data.frame","tbl_df")