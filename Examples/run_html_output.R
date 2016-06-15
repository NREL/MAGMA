
#------------------------------------------------------------------------------|
# USER INPUT: set input parameters ----
#------------------------------------------------------------------------------|

setwd('<Main directory where MAGMA files are located>')
master.script.dir <- 'MAGMA'
input.csv = '<This should point to your input data CSV file>'
# Sourcing the setup file and required functions
source(file.path('query_functions.R'))
source(file.path('setup_plexosAnalysis.R'))
output.dir = file.path('<Directory where HTML reports should be saved>')
render(input=file.path('HTML_output.Rmd'), c("html_document"), output_dir = output.dir)
