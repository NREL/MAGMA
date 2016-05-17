#------------------------------------------------------------------------------|
# USER INPUT: set input parameters ----
#------------------------------------------------------------------------------|

setwd('C:/users/moconnel/documents/PLEXOS-Vis')
# point to location of PSSE2PLEXOS master script
master.script.dir <- 'PLEXOS-Vis'
input.csv = '//plexossql/data/moconnel/gtg/html/input_data.csv'
# Sourcing the setup file and required functions
source(file.path('query_functions.R'))
source(file.path('setup_plexosAnalysis.R'))
output.dir = file.path((na.exclude(inputs$Fig.Path)))
# render(input=file.path('HTML_output.Rmd'), c("html_document"), output_dir = output.dir)

#------------------------------------------------------------------------------|
# Create data files with queries
#------------------------------------------------------------------------------|

yr.data.generator      = tryCatch( yr_gen_query(db), error = function(cond) { return('ERROR') } )
yr.data.region         = tryCatch( yr_region_query(db), error = function(cond) { return('ERROR') } )
yr.data.reserve        = tryCatch( yr_reserve_query(db), error = function(cond) { return('ERROR') } )
yr.data.interface.flow = tryCatch( yr_interface_query(db), error = function(cond) { return('ERROR') } )

int.data.gen           = tryCatch( int_gen_query(db), error = function(cond) { return('ERROR') } )
int.data.avail.cap     = tryCatch( int_avail_cap_query(db), error = function(cond) { return('ERROR') } )
int.data.region        = tryCatch( int_region_query(db), error = function(cond) { return('ERROR') } )
int.data.interface     = tryCatch( int_interface_query(db), error = function(cond) { return('ERROR') } )
int.data.reserve       = tryCatch( int_reserve_query(db), error = function(cond) { return('ERROR') } )

#------------------------------------------------------------------------------|
# Save RData file
#------------------------------------------------------------------------------|

savename = gsub('.*Model', 'Model', db$filename)

save(yr.data.generator, yr.data.region, yr.data.reserve, yr.data.interface.flow, 
     int.data.gen, int.data.avail.cap, int.data.region, int.data.interface, int.data.reserve,
     file = paste0(db.loc, '/', gsub('.db', '', savename),'.RData'))