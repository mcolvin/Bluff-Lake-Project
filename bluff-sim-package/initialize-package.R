library("devtools")
library(roxygen2)
library(data.table)
# https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/

#---create the package structure in the working directory
#create("bluff.sim") # run once





#---lake information

#  watershed size for bluff lake
bluff_lake<- 220 # mi^2
bluff_lake<- bluff_lake*2.58999 # km^2
# watershed at macon
macon<- 768 # mi^2
macon<- macon*2.58999 # km^2
# elevation at empty lake
empty_wse<-64.754 # min ele from bathymetry


#---add data needed 
#  BATHYMETRIC DATA
bath<- fread("_dat/CompleteMap.csv")
names(bath)<-c("X","Y","elevation")

#----------------------------------------------------------------------
# 
#  drawdown period information (3 years, first extended drawdawn followed
#  by 2 years of normal drawdowns)
#
#----------------------------------------------------------------------
drawdown_dat<-as.data.table(read_excel("AssortedElevationNotes.xlsx",sheet="drawdown_cycle"))



bends<-fread("../PSPAP-data/bend-data.csv")
bends<-bends[B_SEGMENT%in%LB,.SD,
	.SDcols=c("B_SEGMENT","BEND_NUM","Length.RKM")]
names(bends)<-c("segment","bend","L")
setorder(bends,segment,bend)
bends[,sid:=.GRP,by="segment"] 
bends[,id:=.I]
bends<-merge(bends,
	segment_meta[,.SD,.SDcols=c("segment","pr_seg","pr_seg_a0")],
	by="segment",
	all.x=TRUE) 
bends[,pr:= L /sum(L)]
bends[,pr_a0:= 0]
setDT(bends)[segment%in%c(9,10,13,14),pr_a0:= L /sum(L)]    


#---build documentation and add datasets
setwd("./bluff.sim")
document()
use_data(segment_meta,overwrite=TRUE) # saves to data folder as rda
use_data(bends,overwrite=TRUE) # saves to data folder as rda

use_package("data.table")
use_package("deSolve")

#---install the package
setwd("..")# move back to root
install("./bluff.sim")