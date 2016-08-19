
#------------------------------------------------------------------------------|
# USER INPUT: set input parameters ----
#------------------------------------------------------------------------------|
magma.dir = '<Main directory where MAGMA files are located>'
input.csv = '<This should point to your input data CSV file>'
output.dir = '<Directory where HTML reports should be saved>'
output.name = '<Desired output file name. Include .html at end.>'
query.data = '<TRUE if you want to create new queries or 
               FALSE if you want to load existing data>'
load.data = '<Name of file to load if query.data=FALSE >'
#------------------------------------------------------------------------------|
# Run code to create HTML
#------------------------------------------------------------------------------|
setwd(magma.dir)
# Sourcing the setup file and required functions
source(file.path('query_functions.R'))
source(file.path('query_functions_compare.R'))
source(file.path('setup_plexosAnalysis.R'))

# Either query data from database or load existing data
source(file.path('query_functions.R'))
source(file.path('setup_plexosAnalysis.R'))
if (query.data){
    source(file.path('setup_dataQueries.R'), echo=TRUE)
} else{
    load(load.data)
}

render(input=file.path('HTML_output_compare.Rmd'), c("html_document"), 
       output_file=output.name, output_dir = file.path(output.dir))
