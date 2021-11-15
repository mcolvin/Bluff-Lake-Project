library("devtools")
library(roxygen2)
library(data.table)
# https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/

#---create the package structure in the working directory
#create("bluff.sim") # run once





#---lake information
lake_meta<-list(
    #  watershed size for bluff lake
    bluff_lake = 220*2.58999, # km^2
    # watershed at macon
    macon = 768*2.58999, # km^2
    # elevation at empty lake
    empty_wse = 64.754) # min ele from bathymetry


#---add data needed 
#  BATHYMETRIC DATA
bath<- data.table::fread("../_analysis/bluff-lake-model/_dat/CompleteMap.csv")
names(bath)<-c("X","Y","elevation")
drawdown_dat<-as.data.table(readxl::read_excel("./AssortedElevationNotes.xlsx",sheet="drawdown_cycle"))



#---build documentation and add datasets
setwd("./bluff.sim")
document()
use_data(bath,overwrite=TRUE) # saves to data folder as rda
use_data(drawdown_dat,overwrite=TRUE) # saves to data folder as rda
use_data(lake_meta,overwrite=TRUE) # saves to data folder as rda

use_package("data.table")


#---install the package
setwd("..")# move back to root
install("./bluff.sim")