
#------------------------------------------------------------------------------|
# USER INPUT: set input parameters ----
#------------------------------------------------------------------------------|
magma.dir = 'C:/Users/moconnel/documents/MAGMA'
input.csv = '//plexossql/data/moconnel/MAGMA/input_data_example_EI.csv'
output.dir = '//plexossql/data/moconnel/MAGMA/reports'
output.name = 'HTML_output_EI.html'
#------------------------------------------------------------------------------|
# Run code to create HTML
#------------------------------------------------------------------------------|
setwd(magma.dir)
# Sourcing the setup file and required functions
source(file.path('query_functions.R'))
source(file.path('setup_plexosAnalysis.R'))
source(file.path('setup_dataQueries.R'))
render(input=file.path('HTML_output.Rmd'), c("html_document"), 
	   output_file=output.name, output_dir = file.path(output.dir))