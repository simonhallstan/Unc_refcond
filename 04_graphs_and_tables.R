
setwd("C:/Users/simonh/OneDrive/SLU/WATERSv2/delprojekt/20160405 Uncertainty in establishment of ref cond/Unc_refcond")
load("results/workspace.RData")



#4 plots in one
windows(1000,1000)
bp.labels<-c("Model (all)", "Model (SysA)", "Model (SysB)", "typology (SysA)","Typology (SysB)", "null model")
par(mfrow=c(2,2),mar=c(7,2,4,2),oma=c(2,3,0,0), cex.lab=0.8, cex.axis=1.2, adj=0, las=2)
boxplot(AUC_taxa.lakes, names=bp.labels, );title("(a) AUC per taxon validation lakes", line = 0.5)
boxplot(AUC_taxa.streams, names=bp.labels, );title("(b) AUC per taxon validation streams", line = 0.5)
boxplot(metrics_lakes[valid.lakes,,'BC'],names=bp.labels);title("(c) BC validation lakes", line = 0.5)
boxplot(metrics_streams[valid.streams,,'BC'],names=bp.labels);title("(d) BC validation streams", line = 0.5)

savePlot("clipboard", type="wmf")


# AUC per taxon -----------------------------------------------------------


windows(500,1000)
bp.labels<-c("Model (all)", "Model (SysA)", "Model (SysB)", "typology (SysA)","Typology (SysB)", "null model")
par(mfrow=c(2,1),mar=c(5,2,4,2),oma=c(5,3,0,0), cex.lab=0.8, cex.axis=1.2, adj=0, las=2)
boxplot(AUC_taxa.lakes, names=bp.labels, );title("(a) lakes", line = 0.5)
boxplot(AUC_taxa.streams, names=bp.labels, );title("(b) streams", line = 0.5)
savePlot("clipboard", type="wmf")

# Metrics per lake/stream --------------------------------------------------------

windows(1000,1000)
bp.labels<-c("Model (all)", "Model (SysA)", "Model (SysB)", "typology (SysA)","Typology (SysB)", "null model")
par(mfrow=c(2,2),mar=c(5,2,4,2),oma=c(5,3,0,0), cex.lab=0.8, cex.axis=1.2, adj=0, las=2)
boxplot(metrics_lakes[valid.lakes,,'OE00'], names=bp.labels, );title("(a) OE00", line = 0.5)
boxplot(metrics_lakes[valid.lakes,,'OE50'],names=bp.labels);title("(b) OE50", line = 0.5)
boxplot(metrics_lakes[valid.lakes,,'BC'],names=bp.labels);title("(c) BC", line = 0.5)
boxplot(metrics_lakes[valid.lakes,,'AUC'], names=bp.labels);title("(d) AUC", line = 0.5)
#mtext("validation lakes", side=3, outer = T,padj=2, cex=1.5, las=0)
savePlot("clipboard", type="wmf")

windows(1000,1000)
bp.labels<-c("Model (all)", "Model (SysA)", "Model (SysB)", "typology (SysA)","Typology (SysB)", "null model")
par(mfrow=c(2,2),mar=c(5,2,4,2),oma=c(5,3,0,0), cex.lab=0.8, cex.axis=1.2, adj=0, las=2)
boxplot(metrics_streams[valid.streams,,'OE00'], names=bp.labels, );title("(a) OE00", line = 0.5)
boxplot(metrics_streams[valid.streams,,'OE50'],names=bp.labels);title("(b) OE50", line = 0.5)
boxplot(metrics_streams[valid.streams,,'BC'],names=bp.labels);title("(c) BC", line = 0.5)
boxplot(metrics_streams[valid.streams,,'AUC'], names=bp.labels);title("(d) AUC", line = 0.5)
#mtext("validation streams", side=3, outer = T,padj=2, cex=1.5, las=0)
savePlot("clipboard", type="wmf")


# Only 2 metrics, lakes and streams in the same figure
windows(1000,1000)
bp.labels<-c("Model (all)", "Model (SysA)", "Model (SysB)", "typology (SysA)","Typology (SysB)", "null model")
par(mfrow=c(2,2),mar=c(6,2,4,2),oma=c(5,3,0,0), cex.lab=0.8, cex.axis=1.2, adj=0, las=2)
boxplot(metrics_lakes[valid.lakes,,'OE25'], names=bp.labels, );title("(a) validation lakes O/E", line = 0.5)
boxplot(metrics_lakes[valid.lakes,,'BC'],names=bp.labels);title("(b) validation lakes BC", line = 0.5)
boxplot(metrics_streams[valid.streams,,'OE25'],names=bp.labels);title("(c) validation streams O/E", line = 0.5)
boxplot(metrics_streams[valid.streams,,'BC'], names=bp.labels);title("(d) validation streams BC", line = 0.5)
savePlot("clipboard", type="wmf")





# tables metrics ----------------------------------------------------------

metrics_lakes.stacked<-as.data.frame.table(metrics_lakes[valid.lakes,,], responseName="data")
colnames(metrics_lakes.stacked)<-c("id", "model","metric","data")
lakes.metrics.mean<-aggregate(data~model+metric, metrics_lakes.stacked, mean);colnames(lakes.metrics.mean)[3]<-"mean"
lakes.metrics.sd<-aggregate(data~model+metric, metrics_lakes.stacked, sd);colnames(lakes.metrics.sd)[3]<-"sd"
write.table(lakes.metrics.mean, "results/lakes.metrics.mean.txt", sep="\t", dec=",", col.names=NA)
write.table(lakes.metrics.sd, "results/lakes.metrics.sd.txt", sep="\t", dec=",", col.names=NA)

metrics_streams.stacked<-as.data.frame.table(metrics_streams[valid.streams,,], responseName="data")
colnames(metrics_streams.stacked)<-c("id", "model","metric","data")
streams.metrics.mean<-aggregate(data~model+metric, metrics_streams.stacked, mean);colnames(streams.metrics.mean)[3]<-"mean"
streams.metrics.sd<-aggregate(data~model+metric, metrics_streams.stacked, sd);colnames(streams.metrics.sd)[3]<-"sd"
write.table(streams.metrics.mean, "results/streams.metrics.mean.txt", sep="\t", dec=",", col.names=NA)
write.table(streams.metrics.sd, "results/streams.metrics.sd.txt", sep="\t", dec=",", col.names=NA)




# uncert boxplots ------------------------------------------------------------------
CV <- function(x) {100*(sqrt(var(x))/mean(x))}


metrics_lakes.stacked.all<-as.data.frame.table(metrics_lakes[,,])
status.lakes.sorted<-status.lakes[order(status.lakes[,1]),];rownames(status.lakes.sorted)<-status.lakes.sorted[,1]
#summary(rownames(metrics_lakes[,,'AUC'])==rownames(status.lakes.sorted))
metrics_lakes.stacked.all<-cbind(metrics_lakes.stacked.all,rep(status.lakes.sorted[,'status'], 30))
colnames(metrics_lakes.stacked.all)<-c("id", "model","metric","data", "status")
metrics_lakes.SD_across_model<-aggregate(data~id+status+metric, metrics_lakes.stacked.all[metrics_lakes.stacked.all[,'model']!='lakes.null',], sd)
metrics_lakes.CV_across_model<-aggregate(data~id+status+metric, metrics_lakes.stacked.all[metrics_lakes.stacked.all[,'model']!='lakes.null',], CV)

metrics_streams.stacked.all<-as.data.frame.table(metrics_streams[,,])
status.streams.sorted<-status.streams[order(as.numeric(status.streams[,1])),];rownames(status.streams.sorted)<-status.streams.sorted[,1]
#summary(rownames(metrics_streams[,,'AUC'])==rownames(status.streams.sorted))
metrics_streams.stacked.all<-cbind(metrics_streams.stacked.all,rep(status.streams.sorted[,'status'], 30))
colnames(metrics_streams.stacked.all)<-c("id", "model","metric","data", "status")
metrics_streams.SD_across_model<-aggregate(data~id+status+metric, metrics_streams.stacked.all[metrics_streams.stacked.all[,'model']!='streams.null',], sd)
metrics_streams.CV_across_model<-aggregate(data~id+status+metric, metrics_streams.stacked.all[metrics_streams.stacked.all[,'model']!='streams.null',], CV)

#save stacked.all
write.table(metrics_lakes.stacked.all, "results/metrics_lakes.stacked.all.txt", sep="\t", dec=",", col.names=NA)
write.table(metrics_streams.stacked.all, "results/metrics_streams.stacked.all.txt", sep="\t", dec=",", col.names=NA)



#all metrics
windows(666,1000)
par(mfrow=c(3,2),mar=c(2,2,4,2),oma=c(5,3,0,0), cex.lab=0.8, cex.axis=1.2, adj=0, las=2)
boxplot(data~metric,data=metrics_lakes.CV_across_model[metrics_lakes.CV_across_model[,'status']=='calib',]);title("(a) calibration lakes", line = 0.5)
boxplot(data~metric,data=metrics_streams.CV_across_model[metrics_streams.CV_across_model[,'status']=='calib',]);title("(b) calibration streams", line = 0.5)
boxplot(data~metric,data=metrics_lakes.CV_across_model[metrics_lakes.CV_across_model[,'status']=='valid',]);title("(c) validation lakes", line = 0.5)
boxplot(data~metric,data=metrics_streams.CV_across_model[metrics_streams.CV_across_model[,'status']=='valid',]);title("(d) validation streams", line = 0.5)
boxplot(data~metric,data=metrics_lakes.CV_across_model[metrics_lakes.CV_across_model[,'status']=='impacted',]);title("(e) non-reference lakes", line = 0.5)
boxplot(data~metric,data=metrics_streams.CV_across_model[metrics_streams.CV_across_model[,'status']=='impacted',]);title("(f) non-reference streams", line = 0.5)
savePlot("clipboard", type="wmf")


         
# BC and OE25

metrics_lakes.CV_across_model$status <- ordered(metrics_lakes.CV_across_model$status, levels=c("calib", "valid", "impacted"))
levels(metrics_lakes.CV_across_model$status)<-c("calibration", "validation", "impacted")

windows(1000,1000)
par(mfrow=c(2,2),mar=c(4,2,4,2),oma=c(5,3,0,0), cex.lab=1, cex.axis=1.2, adj=0, las=2)
boxplot(data~status,data=metrics_lakes.CV_across_model[metrics_lakes.CV_across_model[,'metric']=='OE25',]);title("(a) lakes O/E", line = 0.5)
boxplot(data~status,data=metrics_lakes.CV_across_model[metrics_lakes.CV_across_model[,'metric']=='BC',]);title("(b) lakes BC", line = 0.5)
boxplot(data~status,data=metrics_lakes.CV_across_model[metrics_lakes.CV_across_model[,'metric']=='OE25',]);title("(c) streams O/E", line = 0.5)
boxplot(data~status,data=metrics_lakes.CV_across_model[metrics_lakes.CV_across_model[,'metric']=='BC',]);title("(d) streams BC", line = 0.5)
savePlot("clipboard", type="wmf")





# tables for unc ----------------------------------------------------------
lakes.unc.mean<-aggregate(data~metric+status, metrics_lakes.CV_across_model, mean)
lakes.unc.sd<-aggregate(data~metric+status, metrics_lakes.CV_across_model, sd)
write.table(lakes.unc.mean, "results/lakes.unc.mean.txt", sep="\t", dec=",", col.names=NA)
write.table(lakes.unc.sd, "results/lakes.unc.sd.txt", sep="\t", dec=",", col.names=NA)

streams.unc.mean<-aggregate(data~metric+status, metrics_streams.CV_across_model, mean)
streams.unc.sd<-aggregate(data~metric+status, metrics_streams.CV_across_model, sd)
write.table(streams.unc.mean, "results/streams.unc.mean.txt", sep="\t", dec=",", col.names=NA)
write.table(streams.unc.sd, "results/streams.unc.sd.txt", sep="\t", dec=",", col.names=NA)





# varimp ------------------------------------------------------------------

varimp.lakes.all.rel<-as.data.frame.table(varimp.lakes.all/apply(varimp.lakes.all,1, max))
varimp.lakes.all.rel<-data.frame(varimp.lakes.all.rel, lake_stream="lakes", model="all")
varimp.lakes.systemA_model.rel<-as.data.frame.table(varimp.lakes.systemA_model/apply(varimp.lakes.systemA_model,1, max))
varimp.lakes.systemA_model.rel<-data.frame(varimp.lakes.systemA_model.rel, lake_stream="lakes", model="Sys-A")
varimp.lakes.Drakare_model.rel<-as.data.frame.table(varimp.lakes.Drakare_model/apply(varimp.lakes.Drakare_model,1, max))
varimp.lakes.Drakare_model.rel<-data.frame(varimp.lakes.Drakare_model.rel, lake_stream="lakes", model="sys-B")

varimp.streams.all.rel<-as.data.frame.table(varimp.streams.all/apply(varimp.streams.all,1, max))
varimp.streams.all.rel<-data.frame(varimp.streams.all.rel, lake_stream="streams", model="all")
varimp.streams.systemA_model.rel<-as.data.frame.table(varimp.streams.systemA_model/apply(varimp.streams.systemA_model,1, max))
varimp.streams.systemA_model.rel<-data.frame(varimp.streams.systemA_model.rel, lake_stream="streams", model="Sys-A")
varimp.streams.Drakare_model.rel<-as.data.frame.table(varimp.streams.Drakare_model/apply(varimp.streams.Drakare_model,1, max))
varimp.streams.Drakare_model.rel<-data.frame(varimp.streams.Drakare_model.rel, lake_stream="streams", model="sys-B")

all.relvarimp<-rbind(varimp.lakes.all.rel,varimp.lakes.systemA_model.rel,varimp.lakes.Drakare_model.rel,varimp.streams.all.rel,varimp.streams.systemA_model.rel,varimp.streams.Drakare_model.rel)
colnames(all.relvarimp)[1:3]<-c("taxa", "predictor", "Relative.importance")

all.relvarimp[which(is.na(all.relvarimp[,'Relative.importance'])),'Relative.importance']<-0
all.relvarimp[all.relvarimp[,'Relative.importance']>1,'Relative.importance']<-0
all.relvarimp[all.relvarimp[,'Relative.importance']<0,'Relative.importance']<-0


write.table(all.relvarimp, "results/all.relvarimp.txt", sep="\t", dec=",", col.names=NA)


all.relvarimp.mean<-aggregate(Relative.importance~predictor+model+lake_stream, all.relvarimp, mean)

plot(Relative.importance~predictor+model+lake_stream, data=all.relvarimp.mean)

boxplot(Relative.importance~predictor,data=all.relvarimp[all.relvarimp[,'model']=='Sys-A',], horizontal=T, las=2)


varimp.streams.all

varimp.lakes.systemA_model
varimp.streams.systemA_model


varimp.lakes.Drakare_model
varimp.streams.Drakare_model




varimp.lakes.Drakare_model/apply(varimp.lakes.Drakare_model,1, max)
















# xy plot O v E -----------------------------------------------------------





metrics_lakes[site,model,'OE00']

par(mfrow=c(3,3))
par(mar=c(2,2,2,2))
par(oma=c(3,3,0,0))
plot(rowSums(predicted_probs.lakes.Drakare_model[valid.lakes,])~rowSums(taxa.lakes.pa[valid.lakes,reference_taxa.lakes]), xlim=c(0,22), ylim=c(0,22), main="Model (SysB)", xlab="", ylab="");abline(0, 1);

text(5,100, RF.all.text)



