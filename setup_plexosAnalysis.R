
# Main setup file for general PLEXOS run analysis.

currency = "$"

# -----------------------------------------------------------------------
# Load packages
# -----------------------------------------------------------------------
if (!require(pacman)){
  install.packages("pacman", dependencies=TRUE, repos = "http://cran.rstudio.com/")
  library(pacman)
}else{
  library(pacman)
}
p_load(ggplot2, reshape2, plyr, lubridate, scales, RSQLite, grid, knitr, markdown, grid, gridExtra, RColorBrewer, snow,
       doParallel, xtable, data.table, dplyr, extrafont, tidyr, stringr, rplexos, rmarkdown, yaml, datetime, kableExtra)

# Convert inputs to data table
inputs = data.table(inputs)

# -----------------------------------------------------------------------
# Set ploting parameters
# -----------------------------------------------------------------------
# Size for plot text
if(!exists('text.plot')){
  text.plot = 14
}

# Set ggplot theme
theme_set(theme_bw())

# Color scheme for line plots
# Color palette for colorblind friendly colors, max of 8. Otherwise, will pick rainbow colors
scen.pal = c(rgb(0.0000000, 0.0000000, 0.0000000), rgb(0.9019608, 0.6235294, 0.0000000), 
             rgb(0.3372549, 0.7058824, 0.9137255), rgb(0.0000000, 0.6196078, 0.4509804), 
             rgb(0.9411765, 0.8941176, 0.2588235), rgb(0.0000000, 0.4470588, 0.6980392), 
             rgb(0.8352941, 0.3686275, 0.0000000), rgb(0.8000000, 0.4745098, 0.6549020))

# -----------------------------------------------------------------------
# Determine what sections to run code for. Assign a logical to each chunk run selector. 
# -----------------------------------------------------------------------
run.sections = na.omit(inputs$Sections.to.Run)
if(1 %in% run.sections)  {total.gen.stack=TRUE}                 else {total.gen.stack=FALSE} 
if(2 %in% run.sections)  {zone.gen.stacks=TRUE}                 else {zone.gen.stacks=FALSE}
if(3 %in% run.sections)  {region.gen.stacks=TRUE}               else {region.gen.stacks=FALSE}
if(4 %in% run.sections)  {individual.region.stacks.log=TRUE}    else {individual.region.stacks.log=FALSE}
if(5 %in% run.sections)  {key.period.dispatch.total.log=TRUE}   else {key.period.dispatch.total.log=FALSE}
if(6 %in% run.sections)  {key.period.dispatch.zone.log=TRUE}    else {key.period.dispatch.zone.log=FALSE}
if(7 %in% run.sections)  {key.period.dispatch.region.log=TRUE}  else {key.period.dispatch.region.log=FALSE}
if(8 %in% run.sections)  {daily.curtailment=TRUE}               else {daily.curtailment=FALSE}
if(9 %in% run.sections)  {daily.curtailment.type=TRUE}          else {daily.curtailment.type=FALSE}
if(10 %in% run.sections) {interval.curtailment=TRUE}            else {interval.curtailment=FALSE}
if(11 %in% run.sections) {interval.curtailment.type=TRUE}       else {interval.curtailment.type=FALSE}
if(12 %in% run.sections) {annual.generation.table=TRUE}         else {annual.generation.table=FALSE}
if(13 %in% run.sections) {annual.curtailment.table=TRUE}        else {annual.curtailment.table=FALSE}
if(14 %in% run.sections) {annual.cost.table=TRUE}               else {annual.cost.table=FALSE}
if(15 %in% run.sections) {region.zone.flow.table=TRUE}          else {region.zone.flow.table=FALSE}
if(16 %in% run.sections) {interface.flow.table=TRUE}            else {interface.flow.table=FALSE}
if(17 %in% run.sections) {interface.flow.plots=TRUE}            else {interface.flow.plots=FALSE}
if(18 %in% run.sections) {key.period.interface.flow.plots=TRUE} else {key.period.interface.flow.plots=FALSE}
if(19 %in% run.sections) {line.flow.table=TRUE}                 else {line.flow.table=FALSE}
if(20 %in% run.sections) {line.flow.plots=TRUE}                 else {line.flow.plots=FALSE}
if(21 %in% run.sections) {key.period.line.flow.plots=TRUE}      else {key.period.line.flow.plots=FALSE}
if(22 %in% run.sections) {annual.reserves.table=TRUE}           else {annual.reserves.table=FALSE}
if(23 %in% run.sections) {annual.res.short.table=TRUE}          else {annual.res.short.table=FALSE}
if(24 %in% run.sections) {reserves.plots=TRUE}                  else {reserves.plots=FALSE}
if(25 %in% run.sections) {reserve.stack=TRUE}                   else {reserve.stack=FALSE}
if(26 %in% run.sections) {region.zone.gen.table=TRUE}           else {region.zone.gen.table=FALSE}
if(27 %in% run.sections) {capacity.factor.table=TRUE}           else {capacity.factor.table=FALSE}
if(28 %in% run.sections) {price.duration.curve=TRUE}            else {price.duration.curve=FALSE}
if(29 %in% run.sections) {res.price.duration.curve=TRUE}        else {res.price.duration.curve=FALSE}
if(30 %in% run.sections) {commit.dispatch.zone=TRUE}            else {commit.dispatch.zone=FALSE}
if(31 %in% run.sections) {commit.dispatch.region=TRUE}          else {commit.dispatch.region=FALSE}
if(32 %in% run.sections) {revenue.plots=TRUE}                   else {revenue.plots=FALSE}
if(33 %in% run.sections) {compare.dispatch.zone=TRUE}           else {compare.dispatch.zone=FALSE}
if(34 %in% run.sections) {compare.dispatch.region=TRUE}         else {compare.dispatch.region=FALSE}
if(35 %in% run.sections) {runtime.table=TRUE}                   else {runtime.table=FALSE}
if(36 %in% run.sections) {installed.cap.plot=TRUE}              else {installed.cap.plot=FALSE}

# -----------------------------------------------------------------------
# Read in the data from the input_data.csv file that was just loaded
# -----------------------------------------------------------------------
# set rplexos tiebreak if specified in input file
if ('rplexos.tiebreak'%in%names(inputs)){
  options('rplexos.tiebreak' = 'first')
}

# Determine if Reassign zones based on region to zone mapping file
if (!exists("reassign.zones")) { 
  if ('reassign.zones' %in% names(inputs)){
    reassign.zones = as.logical(na.exclude(inputs$reassign.zones))
  } else{ 
      reassign.zones = FALSE
      warning('You did not state if you plan on reassigning zones. Assuming FALSE.')
    }
} 
if (length(reassign.zones)==0) { 
  reassign.zones = FALSE
  warning('You did not state if you plan on reassigning zones. Assuming FALSE.')
}

# Generation type order for plots
gen.order = (as.character(na.omit(inputs$Gen.Order))) 
# Add Curtailment if not included
if (! 'Curtailment' %in% gen.order){
  gen.order = c('Curtailment',gen.order)
}

# Types of renewables to be considered for curtailment calculations
re.types = as.character(na.omit(inputs$Renewable.Types.for.Curtailment)) 
if (length(re.types)==0) { 
  message('\nNo variable generation types specified for curtailment.')
  re.types = 'none_specified'
}

# Types of generation to be plotted in the DA-RT committmet dispatch plots
da.rt.types = as.character(na.omit(inputs$DA.RT.Plot.Types))
if (length(da.rt.types)==0) {
  message('\nNo generation types specified for DA-RT plots. Plots will not be created.')
}

# Names of key periods
period.names = as.character(na.omit(inputs$Key.Periods)) 
if (length(period.names)==0) {
  message('\nNo key periods specified. No plots will be created for these.')
}

# Number of key periods
n.periods = length(period.names) 

# Start and end times for key periods
if(length(na.omit(inputs$Start.Time)) > 0){
  # Check if year is provided as 4-digit or 2-digit year
  if(nchar(strsplit(as.character(inputs$Start.Time[1]),'[ ,/]')[[1]][3])==2){
    start.end.times = data.table(start = as.POSIXct( na.omit(inputs$Start.Time), format = '%m/%d/%y %H:%M', tz='UTC'), 
                                 end = as.POSIXct( na.omit(inputs$End.Time), format = '%m/%d/%y %H:%M', tz='UTC' ) )
  }else if(nchar(strsplit(as.character(inputs$Start.Time[1]),'[ ,/]')[[1]][3])==4){
    start.end.times = data.table(start = as.POSIXct( na.omit(inputs$Start.Time), format = '%m/%d/%Y %H:%M', tz='UTC'), 
                                 end = as.POSIXct( na.omit(inputs$End.Time), format = '%m/%d/%Y %H:%M', tz='UTC' ) )
  }
  # If is NA, try without hour and minute in date format
  if (any(is.na(start.end.times))){
    if(nchar(strsplit(as.character(inputs$Start.Time[1]),'[ ,/]')[[1]][3])==2){
      start.end.times.temp = data.table(start = as.POSIXct( na.omit(inputs$Start.Time), format = '%m/%d/%y', tz='UTC'), 
                                        end = as.POSIXct( na.omit(inputs$End.Time), format = '%m/%d/%y', tz='UTC' ) )
    }else if(nchar(strsplit(as.character(inputs$Start.Time[1]),'[ ,/]')[[1]][3])==4){
      start.end.times.temp = data.table(start = as.POSIXct( na.omit(inputs$Start.Time), format = '%m/%d/%Y', tz='UTC'), 
                                        end = as.POSIXct( na.omit(inputs$End.Time), format = '%m/%d/%Y', tz='UTC' ) )
    }
    start.end.times[is.na(start), start:=start.end.times.temp[is.na(start.end.times$start),start]]
    start.end.times[is.na(end), end:=start.end.times.temp[is.na(start.end.times$end),end]]
  }
}

# Interfaces to look at flows for
interfaces = as.character(na.omit(inputs$Interfaces.for.Flows))
if (length(interfaces)==0) {
  message('\nNo interfaces specified. No interface data will be shown.')
} else if (any(toupper(interfaces) == 'ALL')){
  interfaces = unique(query_class_member(db,'Interface')$name)
}

# lines to look at flows for
lines = as.character(na.omit(inputs$Lines.for.Flows))
if (length(lines)==0) {
  message('\nNo lines specified. No line data will be shown.')
} else if (any(lines == 'ALL')){
  lines = unique(query_class_member(db,'Line')$name)
}
# Update scen.pal to account for larger number of lines. Give warning about large number of entries
if (length(lines) > length(scen.pal)){
  message('\nYou have specified a large number of lines. Color palette may be hard to differentiate.')
  scen.pal = rainbow(length(lines))
}

# -----------------------------------------------------------------------
# Set up things for plotting
# -----------------------------------------------------------------------
# Location for saved figures
tryCatch({
  dir.create(fig.path.name)
  fig.path.name
}, warning = function(w){
  if(!dir.exists(fig.path.name)){
    print("Cannot create that figure directory, putting figures in subdirectory 'plots' in database location")
    # If figure path does not exist, assume figures should go in a directory 
    # called "plots" in the folder containing the database
    file.path(db.loc,'plots')
  } else{
    fig.path.name
  }
},
error = function(e){
  print("Cannot create that figure directory, putting figures in subdirectory 'plots' in database location")
  # If figure path does not exist, assume figures should go in a directory 
  # called "plots" in the folder containing the database
  file.path(db.loc,'plots')
})
# Ensure path has a / at the end
fig.path.name = file.path(fig.path.name,.Platform$file.sep)

# Zones to ignore for plotting
ignore.zones = as.character(na.omit(inputs$Ignore.Zones))

# Regions to ignore for plotting
ignore.regions = as.character(na.omit(inputs$Ignore.Regions))


# -----------------------------------------------------------------------
# Get database locations
# -----------------------------------------------------------------------
# location of database
if (!exists('db.loc')){
  if ('Database.Location' %in% names(inputs)){
    db.loc = file.path(as.character(na.exclude(inputs$Database.Location))) 
  } else { warning('You have not supplied a database location. This is required.')}
}
if (!exists('db.day.ahead.loc')){
  if ('DayAhead.Database.Location' %in% names(inputs)){
    db.day.ahead.loc = file.path(as.character(na.exclude(inputs$DayAhead.Database.Location)))
  } 
}
if (length(db.day.ahead.loc)==0 | !exists('db.day.ahead.loc')) { db.day.ahead.loc = db.loc }

# -----------------------------------------------------------------------
# Process databases if needed
# -----------------------------------------------------------------------
# Remove trailing / if present
db.loc <- unlist(lapply(db.loc, function(x) { 
  if (substr(x,nchar(x),nchar(x))=='/'){
    substr(x,1,nchar(x)-1)
  }else{x} 
}))

run.rplx.all=F
first.missing.db=T
ok.to.query=T
for (i in 1:length(db.loc)) { 
  run.rplx=F
  if(length(list.files(pattern = "\\.zip$",path=db.loc[i]))!=0 ) {
    if(length(list.files(pattern = "\\.db$",path=db.loc[i]))==0) {
      message(paste0('The .db file is absent from ',db.loc[i]))
      run.rplx=T
    } else if(any(file.info(file.path(db.loc[i],list.files(pattern = "\\.db$",path=db.loc[i])))$mtime <
                  file.info(file.path(db.loc[i],list.files(pattern = "\\.zip$",path=db.loc[i])))$mtime, na.rm=TRUE)) {
      message(paste0('The db is older than the zip or the .db file in ',db.loc[i]))
      run.rplx=T
    } else {message(paste0('\nFound .db solution file: ', list.files(pattern='\\.db$',path=db.loc[i]), '\n'))}
    if(run.rplx) {
      if(first.missing.db){
        run.rplx.all = (readline('Do you want to run the rPLEXOS db creation tool for all zip files without db files? (y/n):')=='y' | !interactive())
        first.missing.db=F
      }
      if(run.rplx.all){
        message('Running process_folder')
        process_folder(db.loc[i])
      } else if(readline('Do you want to run the rPLEXOS db creation tool now? (y/n):')=='y' | !interactive()){
        message('Running process_folder')
        process_folder(db.loc[i])
      } else {message('You need to run rPLEXOS to process your solution or point to the correct solution folder.')}
    } 
  } else if (length(list.files(pattern = '\\.db$', path=db.loc[i]))!=0 ) {
    message(paste0('\nFound .db solution file: ', list.files(pattern='\\.db$',path=db.loc[i]), '\n'))
    ok.to.query = T
  } else {
    message('No .zip or .db file... are you in the right directory?')
    ok.to.query = F
    }
}
# -----------------------------------------------------------------------
# Open the database file ( must already have created this using rplexos ) 
# -----------------------------------------------------------------------
# Get scenario names if specified
if(!exists('scenario.names') & ok.to.query){
  scenario.names = basename(db.loc)
}
if ('Scenario.Names'%in%names(inputs)){
  if (length(na.omit(inputs$Scenario.Name))>0){
    scenario.names = as.character(inputs$Scenario.Name[!is.na(inputs$Scenario.Name)])
  }
}
if (length(scenario.names)!=length(db.loc)){
  print("You specified a different number of scenario names than databases.")
  print("Using database names as scenarios")
  scenario.names = basename(db.loc)
}
  
db.day.ahead.loc <- unlist(lapply(db.day.ahead.loc, function(x) { 
  if (substr(x,nchar(x),nchar(x))=='/'){
    substr(x,1,nchar(x)-1)
  }else{x} 
}))

# Open database
if(ok.to.query){
  db = plexos_open(db.loc, scenario.names)
  attributes(db)$class = c("rplexos","data.frame","tbl_df")
  
}else{
  warning(paste("Your database doesn't exist. We will not plot anything involving that database.",
                "If you want those plots created, please fix your database name",sep="\n"))
  db = data.table(scenario = scenario.names,position='EMPTY',filename=db.loc,tables=0,properties=0)
}
# Open the day ahead database file
db.day.ahead = tryCatch(plexos_open(db.day.ahead.loc, scenario.names), error = function(cond) { return(data.frame('ERROR'))} )
if (is.character(db.day.ahead)){
  warning(paste("Your Day Ahead database doesn't exist. We will not plot anything involving that database.",
                "If you want those plots created, please fix your database name",sep="\n"))
}
attributes(db.day.ahead)$class = c('rplexos', 'data.frame', 'tbl_df')

# -----------------------------------------------------------------------
# Set up for comparisons if using
# -----------------------------------------------------------------------
# Determine if you have multiple scenarios and should run the comparison version
has.multiple.scenarios = (length(db.loc)>1)
# reference scenario, used if comparing scenarios
if (has.multiple.scenarios){
  if ('ref.scenario' %in% names(inputs)){
    ref.scenario = as.character(inputs$ref.scenario[!is.na(inputs$ref.scenario)])
    if (length(ref.scenario)==0) {
      ref.scenario = scenario.names[1]
    }
    if (length(ref.scenario)>1){
      message('\nYou have more than one reference scenario. This is likely to cause problems with comparison calculations')
    } else{
      if (length(ref.scenario)==0 & has.multiple.scenarios){
        message('\nYou have not provided a reference scenario. Comparison plots will not work.')
      }
    }
  } else {
    message('\nYou did not specify a reference scenario for your comparisons. We will use the first scenario listed.')
    ref.scenario <- scenario.names[1]
  }
} else{ ref.scenario = scenario.names[1]}

# -----------------------------------------------------------------------
# Get properties available
# -----------------------------------------------------------------------
# get available properties
if(ok.to.query){
  properties = data.table(query_property(db))
  if (db.day.ahead[1,1]!='ERROR'){
    properties.day.ahead = data.table(query_property(db.day.ahead))
  }
}
# -----------------------------------------------------------------------
# Create Generator-Type mapping 
# -----------------------------------------------------------------------
# Using CSV file to map generator types to names?
if (!exists("use.gen.type.csv")) { 
  if ('Using.Gen.Type.Mapping.CSV' %in% names(inputs)){
    use.gen.type.csv = as.logical(na.exclude(inputs$Using.Gen.Type.Mapping.CSV))
  } else{ 
    use.gen.type.csv = FALSE
    warning('You did not state if you are using a generator-type mapping file. Assuming FALSE.')
  }
} 
if (!exists("gen.type.csv.loc")) { 
  if ('CSV.Gen.Type.File.Location' %in% names(inputs)){
    gen.type.csv.loc = as.character(na.exclude(inputs$CSV.Gen.Type.File.Location))
  } else{ 
      gen.type.csv.loc = NULL
      use.gen.type.csv = FALSE
    }
} 

# Create generator name to type mapping
if ( (use.gen.type.csv & length(gen.type.csv.loc>0)) | flex.inventory ) {
  # Read mapping file to map generator names to generation type
  gen.type.mapping = data.table(read.csv(gen.type.csv.loc, 
                                         stringsAsFactors=FALSE))
  gen.type.mapping = unique(gen.type.mapping[,.(name, Type)])
  gen.type.mapping = setNames(gen.type.mapping$Type, gen.type.mapping$name)
  if( flex.inventory){
      gen.property.mapping = data.table(read.csv(gen.type.csv.loc, 
                                                 stringsAsFactors = FALSE))
      gen.property.mapping[,Type:=NULL]
      gen.property.mapping[,Region:=NULL]
      gen.property.mapping[,Zone:=NULL]
      ## Make sure gen.property.mapping has all required fields
      if( !("Min.Stable.Level" %in% names(gen.property.mapping))){
          warning("Flexibility Inventory requires Min.Stable.Level column in generator mapping csv. Using 0 MW for all generators")
          gen.property.mapping[,`Min.Stable.Level`:=0]
      }
      if( !("Max.Ramp.Up" %in% names(gen.property.mapping))){
          warning("Flexibility Inventory requires Max.Ramp.Up column in generator mapping csv. Using 1E+30 for all generators")
          gen.property.mapping[,`Max.Ramp.Up`:=1e30]
      }
      if( !("Max.Ramp.Down" %in% names(gen.property.mapping))){
          warning("Flexibility Inventory requires Max.Ramp.Down column in generator mapping csv. Using 1E+30 for all generators")
          gen.property.mapping[,`Max.Ramp.Down`:=1e30]
      }
      if( !("Min.Up.Time" %in% names(gen.property.mapping))){
          warning("Flexibility Inventory requires Min.Up.Time column in generator mapping csv. Using 0 hrs for all generators")
          gen.property.mapping[,`Min.Up.Time`:=1e30]
      }
      if( !("Min.Down.Time" %in% names(gen.property.mapping))){
          warning("Flexibility Inventory requires Min.Down.Time column in generator mapping csv. Using 0 hrs for all generators")
          gen.property.mapping[,`Min.Down.Time`:=1e30]
      }
      if( !("Start.Up.Time" %in% names(gen.property.mapping))){
          warning("Flexibility Inventory requires Start.Up.Time column in generator mapping csv. Using Ramp Rate to determine start up time")
      }
      if( !("Shut.Down.Time" %in% names(gen.property.mapping))){
          warning("Flexibility Inventory requires Shut.Down.Time column in generator mapping csv. Using Ramp Rate to determine shut down time")
      }
      if( !("Must.Run" %in% names(gen.property.mapping))){
          warning("Flexibility Inventory requires commit parameter for must-run-units. Using must run = false for all generators")
          gen.property.mapping[,`Must.Run`:=0]
      }
      setkey(gen.property.mapping, name)
  }
  } else {
  # Assign generation type according to PLEXOS category
    sql <- "SELECT DISTINCT name, category FROM key WHERE class = 'Generator'"
    gen.cat.plexos = query_sql(db,sql) 
    gen.cat.plexos = unique(gen.cat.plexos[,c("name","category")])
    # If PLEXOS Gen Category mapping defined use that
    if (length(inputs$PLEXOS.Gen.Category[!is.na(inputs$PLEXOS.Gen.Category)]) > 0) {
      gen.cat.mapping = data.table(name = as.character(na.omit(inputs$PLEXOS.Gen.Category)), 
                                   Type = as.character(na.omit(inputs$PLEXOS.Desired.Type)) )  
      gen.cat.mapping = setNames(gen.cat.mapping$Type, gen.cat.mapping$name)
      gen.cat.plexos$Type = gen.cat.mapping[gen.cat.plexos$category]
      gen.type.mapping = setNames(gen.cat.plexos$Type, gen.cat.plexos$name)
      } else{
        # If csv not provided and PLEXOS mapping not defined, use PLEXOS categories
        message("Generator Type mapping not provided. Will use PLEXOS categories")
        gen.type.mapping = setNames(gen.cat.plexos$category, gen.cat.plexos$name)
    }
  if (length(gen.type.mapping)==0) { message(paste('\nIf not using generator name to type mapping CSV, \n',
                                             'you must specify PLEXOS categories and desired generation type in input csv file.')) }
}

# -----------------------------------------------------------------------
# Create Generator-Region mapping 
# -----------------------------------------------------------------------
# Read mapping file to map generator names to region and zone (can be same file as gen name to type).
# if (is.na(inputs$Gen.Region.Zone.Mapping.Filename)[1]){
if ( !exists("gen.region.zone") ) {
  if ('Gen.Region.Zone.Mapping.Filename'%in%names(inputs)){
    gen.region.zone <- as.character(na.exclude(inputs$Gen.Region.Zone.Mapping.Filename))
  } else{ 
    gen.region.zone <- NULL
  }
}
if (length(gen.region.zone)==0) {
  warning("You did not supply a Generator-Region mapping. We will create one for you from the rplexos database")
  gen.mapping <- query_generator(db)
  if(all(c('Region.Name','Zone.Name')%in%names(inputs))){
      region.zone = data.table(inputs[,.(Region=as.character(Region.Name),
                                         Zone=as.character(Zone.Name))])
      region.zone.mapping = merge(region.zone, data.table(unique(gen.mapping[,c('name','region')])),
                                  by.y='region', by.x='Region')
  } else{
    warning(paste("You did not supply a Region-Zone mapping. We will create one for you from the rplexos database",
                  "However, rplexos reassigns Zone names to the Region category. If you do not want this behavior,",
                  "please create your own mapping file. You may use the file tools/make_region_zone_csv.py to do so,",
                  "which will require COAD to be installed and part of your path. You can also use the 'Region.Name' ",
                  "and 'Zone.Name' columns in the input sheet to map Region names to Zone names."))
    region.zone.mapping = data.table(unique(gen.mapping[,c('name','region','zone')]))
    setnames(region.zone.mapping, c("region","zone"), c("Region","Zone"))
  }
} else{
  if(length(gen.region.zone)>1){
    warning("More than one Gen.Region.Zone.Mapping.Filename found... I'll create a unique combination for you.")
    region.zone.mapping = data.table()
    for (i in 1:length(gen.region.zone)){
      region.zone.mapping = rbindlist(list(region.zone.mapping,data.table(read.csv(gen.region.zone[i], 
                                                                          stringsAsFactors=FALSE))),fill=TRUE)
    }
  } else{
    region.zone.mapping = data.table(read.csv(gen.region.zone, 
                                              stringsAsFactors=FALSE))
  }
  if ( typeof(region.zone.mapping$Region)!="character" | typeof(region.zone.mapping$Zone)!="character" ) {
    region.zone.mapping$Region = as.character(region.zone.mapping$Region)
    region.zone.mapping$Zone = as.character(region.zone.mapping$Zone)
  }
}
region.zone.mapping = unique(region.zone.mapping, by=c('name','Region','Zone'))
setkey(region.zone.mapping,name)

# -----------------------------------------------------------------------
# Get Region and Zone orders
# -----------------------------------------------------------------------
if ('Region.Order'%in%names(inputs)){
  region.order <- as.character(na.exclude(inputs$Region.Order))
  missing.regions <- unique(region.zone.mapping$Region)[!unique(region.zone.mapping$Region) %in% region.order]
  if (length(missing.regions) > 0){
    warning(sprintf('Your Region order does not contain all regions. You are missing %s. Adding those regions.',
                    missing.regions))
    region.order <- c(region.order,missing.regions)
  }
} else{
  region.order <- sort(unique(region.zone.mapping$Region))
}

if('Zone.Order'%in%names(inputs)){
  zone.order <- as.character(na.exclude(inputs$Zone.Order))
  missing.zones <- unique(region.zone.mapping$Zone)[!unique(region.zone.mapping$Zone) %in% zone.order]
  if (length(missing.zones) > 0){
    warning(sprintf('Your Zone order does not contain all zones. You are missing %s. Adding those zones.',
                    missing.zones))
    zone.order <- c(zone.order,missing.zones)
  }
} else{
  zone.order <- sort(unique(region.zone.mapping$Zone))
}
region.zone.mapping[, Region:=factor(Region,levels=region.order)]
region.zone.mapping[, Zone:=factor(Zone,levels=zone.order)]
rz.unique = unique(region.zone.mapping[,.(Region,Zone)])

# -----------------------------------------------------------------------
# Create Generator-Color mapping and scenario colors
# -----------------------------------------------------------------------
  # Set plot color for each generation type. Use rainbow() if mapping not provided
if(all(is.na(inputs$Gen.Type)) | all(is.na(inputs$Plot.Color))){
  types <- unique(gen.type.mapping)
  gen.color <- setNames(rainbow(length(types)), types)
} else{
  Gen.col = data.table(Type = na.omit(inputs$Gen.Type), Color = na.omit(inputs$Plot.Color) )
  gen.color<-setNames(as.character(Gen.col$Color),Gen.col$Type)
}
# Add Curtailment if not included
if (! 'Curtailment' %in% names(gen.color)){
  gen.color['Curtailment'] = 'red'
}

# Update scen.pal to account for larger number of scenarios Give warning about large number of entries
if (length(scenario.names) > length(scen.pal)){
  message('\nYou have specified a large number of scenarios Color palette may be hard to differentiate.')
  scen.pal = rainbow(length(scenario.names))
}

# -----------------------------------------------------------------------
# Get some time properties of your model
# -----------------------------------------------------------------------
# Calculate First and last day of simulation and interval length
if(ok.to.query) model.timesteps = model_timesteps(db)
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

# Check to make sure all solutions have the same length
if (length(unique(model.intervals$timestep))!=1){
  message("Warning: Your databases do not have the same time intervals")
}
# Number of intervals per day
intervals.per.day = 24 / unique(as.numeric(model.intervals$timestep,units='hours'))
