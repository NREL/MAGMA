
#------------------------------------------------------------------------------|
# USER INPUT: set input parameters ----
#------------------------------------------------------------------------------|

setwd('C:/Users/moconnel/documents/MAGMA')
master.script.dir <- 'MAGMA'
input.csv = 'C:/Users/moconnel/documents/MAGMA/Examples/input_data.csv'
# Sourcing the setup file and required functions
source(file.path('query_functions.R'))
source(file.path('setup_plexosAnalysis.R'))
output.dir = file.path('Examples/reports')
render(input=file.path('HTML_output.Rmd'), c("html_document"), output_dir = output.dir)
