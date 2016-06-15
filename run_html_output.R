
#------------------------------------------------------------------------------|
# USER INPUT: set input parameters ----
#------------------------------------------------------------------------------|

setwd('C:/Users/moconnel/documents/PLEXOS-Vis')
# point to location of PSSE2PLEXOS master script
master.script.dir <- 'PLEXOS-Vis'
input.csv = '//plexossql/data/moconnel/MAGMA/input_data.csv'
# Sourcing the setup file and required functions
source(file.path('query_functions.R'))
source(file.path('setup_plexosAnalysis.R'))
output.dir = file.path((na.exclude(inputs$Fig.Path)))
render(input=file.path('HTML_output.Rmd'), c("html_document"), output_dir = output.dir)
