#read all files 

setwd("C:/Users/simonh/OneDrive/SLU/WATERSv2/delprojekt/20160405 Uncertainty in establishment of ref cond/Unc_refmods")

taxa.lakes<-read.table("data/inv_lakes.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")
taxa.streams<-read.table("data/inv_streams.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")

env.lakes<-read.table("data/env_lakes.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")
env.streams<-read.table("data/env_streams.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")

status.lakes<-read.table("data/status_lakes.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")
status.streams<-read.table("data/status_streams.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")

types.lakes<-read.table("data/types_lakes.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")
types.streams<-read.table("data/types_streams.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")


#check that the rownames are the same for all three files
summary(rownames(taxa.lakes)==rownames(env.lakes))
summary(rownames(env.lakes)==rownames(status.lakes))
summary(rownames(status.lakes)==rownames(types.lakes))
summary(rownames(taxa.streams)==rownames(env.streams))
summary(rownames(env.streams)==rownames(status.streams))
summary(rownames(status.streams)==rownames(types.streams))


#create subsets with only calibration sites
#convert invertebrate matrix to presence-absence

reference.lakes<-rownames(status.lakes)[which(status.lakes[,'reference']==1)]
reference.streams<-rownames(status.streams)[which(status.streams[,'reference']==1)]

set.seed(75007)
calib.lakes<-sample(reference.lakes,58)
calib.streams<-sample(reference.streams,58)

valid.lakes<-setdiff(reference.lakes,calib.lakes)
valid.streams<-setdiff(reference.streams,calib.streams)



#-----------------

#environmental variables
env.calib             <-env.all[references,]


types.cal<-types[calib,,drop=F]

#biology
taxa.pa<-taxa.all;taxa.pa[taxa.all>0]<-1

taxa.ref<-taxa.pa[references,]
taxa.ref<-taxa.ref[,colSums(taxa.ref)>3 & colSums(taxa.ref)<(nrow(taxa.ref)-3)] #remove rare and common taxa

taxa.cal<-taxa.pa[calib,]
taxa.cal<-taxa.cal[,colSums(taxa.cal)>3 & colSums(taxa.cal)<(nrow(taxa.cal)-3)] #remove rare and common taxa

taxa.all.seltaxa<-taxa.pa[,colnames(taxa.cal)]

index.cal<-index.all[calib,]
