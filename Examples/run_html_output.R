
#------------------------------------------------------------------------------|
# USER INPUT: set input parameters ----
#------------------------------------------------------------------------------|

setwd('<Main directory where MAGMA files are located>')
input.csv = '<This should point to your input data CSV file>'
# Sourcing the setup file and required functions
source(file.path('query_functions.R'))
source(file.path('setup_plexosAnalysis.R'))
source(file.path('setup_dataQueries.R'))
output.dir = file.path('<Directory where HTML reports should be saved>')
render(input=file.path('HTML_output.Rmd'), c("html_document"), output_file='<Desired output file name. Include .html at end.>', output_dir = output.dir)
