
# dynamically set working directory
this_dir <- function(directory)
setwd(file.path(getwd(), directory) )
# end

source("_r/1-global.R");library(scales);library(tidyverse)
source("_r/2-functions.R")
source("_r/3-load-and-clean.R")
source("_r/4-tables.R")
source("_r/5-figures.R")
source("_r/6.5-analysis-hour-edt-all-elevations.R")

install.packages("tidyverse")
# figures

# tables