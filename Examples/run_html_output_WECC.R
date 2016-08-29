
#------------------------------------------------------------------------------|
# USER INPUT: set input parameters ----
#------------------------------------------------------------------------------|

setwd('C:/Users/moconnel/documents/MAGMA')
input.csv = '//plexossql/data/moconnel/MAGMA/input_data_example_WECC.csv'
# Sourcing the setup file and required functions
source(file.path('query_functions.R'))
source(file.path('setup_plexosAnalysis.R'))
source(file.path('setup_dataQueries.R'))
output.dir = file.path('//plexossql/data/moconnel/MAGMA/reports')
render(input=file.path('HTML_output.Rmd'), c("html_document"), output_file='HTML_output_WECC_countries.html', output_dir = output.dir)
