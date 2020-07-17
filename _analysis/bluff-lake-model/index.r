
# dynamcially set working directory
this_dir <- function(directory)
setwd(file.path(getwd(), directory) )
# end

source("_r/1-global.R")
source("_r/2-functions.R")
source("_r/3-load-and-clean.R")
source("_r/4-tables.R")
source("_r/5-figures.R")
source("_r/6-analysis.R")


# figures

# tables