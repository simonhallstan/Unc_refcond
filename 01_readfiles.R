#read all files 

setwd("C:/Users/simonh/OneDrive/SLU/WATERSv2/delprojekt/20160405 Uncertainty in establishment of ref cond/Unc_refcond")

taxa.lakes<-read.table("data/inv_lakes.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")
taxa.streams<-read.table("data/inv_streams.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")

env.lakes<-read.table("data/env_lakes.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")
env.streams<-read.table("data/env_streams.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")

status.lakes<-read.table("data/status_lakes.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")
status.streams<-read.table("data/status_streams.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")

types_SystemA.lakes<-read.table("data/types_SystemA_lakes.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")
types_SystemA.streams<-read.table("data/types_SystemA_streams.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")

types_Drakare.lakes<-read.table("data/types_Drakare_lakes.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")
types_Drakare.streams<-read.table("data/types_Drakare_streams.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")

#check that the rownames are the same for all three files
summary(rownames(taxa.lakes)==rownames(env.lakes))
summary(rownames(env.lakes)==rownames(status.lakes))
summary(rownames(status.lakes)==rownames(types_SystemA.lakes))
summary(rownames(types_SystemA.lakes)==rownames(types_Drakare.lakes))

summary(rownames(taxa.streams)==rownames(env.streams))
summary(rownames(env.streams)==rownames(status.streams))
summary(rownames(status.streams)==rownames(types_SystemA.streams))
summary(rownames(types_Drakare.streams)==rownames(types_SystemA.streams))

#create subsets with only calibration sites
#convert invertebrate matrix to presence-absence

reference.lakes<-rownames(status.lakes)[which(status.lakes[,'reference']==1)]
reference.streams<-rownames(status.streams)[which(status.streams[,'reference']==1)]

set.seed(75007)
calib.lakes<-sample(reference.lakes,58)
calib.streams<-sample(reference.streams,58)

valid.lakes<-setdiff(reference.lakes,calib.lakes)
valid.streams<-setdiff(reference.streams,calib.streams)

imapact.lakes<-setdiff(rownames(taxa.lakes),reference.lakes)
imapact.streams<-setdiff(rownames(taxa.streams),reference.streams)


status.lakes<-rbind(cbind(calib.lakes,rep( "calib",length(calib.lakes))),cbind(valid.lakes,rep( "valid",length(valid.lakes))),cbind(imapact.lakes,rep( "impacted",length(imapact.lakes))))
status.streams<-rbind(cbind(calib.streams,rep( "calib",length(calib.streams))),cbind(valid.streams,rep( "valid",length(valid.streams))),cbind(imapact.streams,rep( "impacted",length(imapact.streams))))

colnames(status.lakes)<-c("sjoid","status")
colnames(status.streams)<-c("vdrid","status")


write.table(status.lakes, "results/status.lakes.txt", sep="\t", dec=",", col.names=NA)
write.table(status.streams, "results/status.streams.txt", sep="\t", dec=",", col.names=NA)


#-----------------


#convert to present-absence
#and define "reference-taxa" as those present in 3-(all-3) calibration lakes
taxa.lakes.pa<-taxa.lakes;taxa.lakes.pa[taxa.lakes>0]<-1
taxa.streams.pa<-taxa.streams;taxa.streams.pa[taxa.streams>0]<-1

reference_taxa.lakes<-colnames(taxa.lakes)[colSums(taxa.lakes.pa[calib.lakes,])>3 & colSums(taxa.lakes.pa[calib.lakes,])<(length(calib.lakes)-3)]
reference_taxa.streams<-colnames(taxa.streams)[colSums(taxa.streams.pa[calib.streams,])>3 & colSums(taxa.streams.pa[calib.streams,])<(length(calib.streams)-3)]






