
#------------------------------------------------------------------------------|
# USER INPUT: set input parameters ----
#------------------------------------------------------------------------------|
magma.dir = '<Main directory where MAGMA files are located>'
input.csv = '<This should point to your input data CSV file>'
output.dir = '<Directory where HTML reports should be saved>'
output.name = '<Desired output file name. Include .html at end.>'
#------------------------------------------------------------------------------|
# Run code to create HTML
#------------------------------------------------------------------------------|
setwd(magma_dir)
# Sourcing the setup file and required functions
source(file.path('query_functions.R'))
source(file.path('query_functions_compare.R'))
source(file.path('setup_plexosAnalysis.R'))
source(file.path('setup_dataQueries.R'))
output.path = file.path(output.dir)
render(input=file.path('HTML_output_compare.Rmd'), c("html_document"), output_dir = output.dir)
