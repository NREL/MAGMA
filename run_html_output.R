
#------------------------------------------------------------------------------|
# set working directory ----
#------------------------------------------------------------------------------|

# set working directory

if (interactive()) {
  t=try(dirname(sys.frame(1)$ofile),silent = T)
  if(inherits(t, "try-error")) {
    warning("Make sure you are in the PLEXOS-Vis submodule path")
  } else {
    script.dir = dirname(sys.frame(1)$ofile)
    setwd(script.dir)
  }
} else {
  dir = getSrcDirectory(function(x) {x})
  m <- regexpr("(?<=^--file=).+", commandArgs(), perl=TRUE)
  script.dir <- dirname(regmatches(commandArgs(), m))
  if(length(script.dir) == 0) 
    stop("can't determine script dir: please call the script with Rscript")
  if(length(script.dir) > 1) 
    stop("can't determine script dir: more than one '--file' argument detected")
  setwd(script.dir)
}


#------------------------------------------------------------------------------|
# USER INPUT: set input parameters ----
#------------------------------------------------------------------------------|

# point to location of PSSE2PLEXOS master script
master.script.dir <- 'PLEXOS-Vis'
input.csv = 'vis_input_data.csv'
# Sourcing the setup file and required functions
source(file.path(master.script.dir,'query_functions.R'))
source(file.path(master.script.dir,'setup_plexosAnalysis.R'))

render(file.path(master.script.dir,'HTML_output.Rmd'))
