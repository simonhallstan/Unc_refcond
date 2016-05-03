# R code for 
# For details and data source, see 

require(randomForest)
require(PresenceAbsence)


# Models _______________________________________________________________________________________________________________________________

# Lakes  ----------------------------------  ----------------------------------  ----------------------------------  


# Model 1 - Lakes all predictors
#Empty matrix for probabilties and variable importance
predicted_probs.lakes.all<-matrix(data = NA, nrow = nrow(taxa.lakes), ncol = length(reference_taxa.lakes))
colnames(predicted_probs.lakes.all)<-reference_taxa.lakes;rownames(predicted_probs.lakes.all)<-rownames(taxa.lakes)

varimp.lakes.all<-matrix(data = NA, ncol = ncol(env.lakes), nrow = length(reference_taxa.lakes))
rownames(varimp.lakes.all)<-reference_taxa.lakes;colnames(varimp.lakes.all)<-colnames(env.lakes)

  
  for(taxon in reference_taxa.lakes) 
  {
    data.tmp<-data.frame(taxa.lakes.pa[calib.lakes,taxon], env.lakes[calib.lakes,])
    model.RF<-randomForest(as.factor(data.tmp[,1]) ~ .,data=data.tmp[,-1], importance=T, ntree=1000, mtry=3, sampsize=42, nodesize=1)
    predicted_probs.lakes.all[,taxon]<-predict(model.RF,type="prob", newdata=env.lakes)[,'1']
    
    save(model.RF, file=paste("results/models/lake_all",taxon, sep="_"))
    varimp.lakes.all[taxon,colnames(env.lakes)]<-  model.RF$importance[,'MeanDecreaseAccuracy']
  } 

write.table(predicted_probs.lakes.all, "results/predicted_probs.lakes.all.txt", sep="\t", dec=",", col.names=NA)
write.table(varimp.lakes.all, "results/varimp.lakes.all.txt", sep="\t", dec=",", col.names=NA)


# Model 2 - Lakes systemA
preds<-c("Illies.ecoregion", "altitude","mean.depth..pred.or.observed.", "lake.area","Alk_Acid","Abs..filtered.")


#Empty matrix for probabilties and variable importance
predicted_probs.lakes.systemA_model<-matrix(data = NA, nrow = nrow(taxa.lakes), ncol = length(reference_taxa.lakes))
colnames(predicted_probs.lakes.systemA_model)<-reference_taxa.lakes;rownames(predicted_probs.lakes.systemA_model)<-rownames(taxa.lakes)

varimp.lakes.systemA_model<-matrix(data = NA, ncol = length(preds), nrow = length(reference_taxa.lakes))
rownames(varimp.lakes.systemA_model)<-reference_taxa.lakes;colnames(varimp.lakes.systemA_model)<-preds


for(taxon in reference_taxa.lakes) 
{
  data.tmp<-data.frame(taxa.lakes.pa[calib.lakes,taxon], env.lakes[calib.lakes,preds])
  model.RF<-randomForest(as.factor(data.tmp[,1]) ~ .,data=data.tmp[,-1], importance=T, ntree=1000, mtry=3, sampsize=42, nodesize=1)
  predicted_probs.lakes.systemA_model[,taxon]<-predict(model.RF,type="prob", newdata=env.lakes)[,'1']
  
  save(model.RF, file=paste("results/models/lake_SystemA",taxon, sep="_"))
  varimp.lakes.systemA_model[taxon,preds]<-  model.RF$importance[,'MeanDecreaseAccuracy']
} 


write.table(predicted_probs.lakes.systemA_model, "results/predicted_probs.lakes.systemA.txt", sep="\t", dec=",", col.names=NA)
write.table(varimp.lakes.systemA_model, "results/varimp.lakes.systemA.txt", sep="\t", dec=",", col.names=NA)

# Model 3 - Lakes Drakare
preds<-c("LN", "altitude","mean.depth..pred.or.observed.", "Alk_Acid","Abs..filtered.")


#Empty matrix for probabilties and variable importance
predicted_probs.lakes.Drakare_model<-matrix(data = NA, nrow = nrow(taxa.lakes), ncol = length(reference_taxa.lakes))
colnames(predicted_probs.lakes.Drakare_model)<-reference_taxa.lakes;rownames(predicted_probs.lakes.Drakare_model)<-rownames(taxa.lakes)

varimp.lakes.Drakare_model<-matrix(data = NA, ncol = length(preds), nrow = length(reference_taxa.lakes))
rownames(varimp.lakes.Drakare_model)<-reference_taxa.lakes;colnames(varimp.lakes.Drakare_model)<-preds


for(taxon in reference_taxa.lakes) 
{
  data.tmp<-data.frame(taxa.lakes.pa[calib.lakes,taxon], env.lakes[calib.lakes,preds])
  model.RF<-randomForest(as.factor(data.tmp[,1]) ~ .,data=data.tmp[,-1], importance=T, ntree=1000, mtry=3, sampsize=42, nodesize=1)
  predicted_probs.lakes.Drakare_model[,taxon]<-predict(model.RF,type="prob", newdata=env.lakes)[,'1']
  
  save(model.RF, file=paste("results/models/lake_Drakare",taxon, sep="_"))
  varimp.lakes.Drakare_model[taxon,preds]<-  model.RF$importance[,'MeanDecreaseAccuracy']
} 

write.table(predicted_probs.lakes.Drakare_model, "results/predicted_probs.lakes.Drakare_model.txt", sep="\t", dec=",", col.names=NA)
write.table(varimp.lakes.Drakare_model, "results/varimp.lakes.Drakare_model.txt", sep="\t", dec=",", col.names=NA)


# Streams  ----------------------------------  ----------------------------------  ----------------------------------  


# Model 1 - streams all predictors
#Empty matrix for probabilties and variable importance
predicted_probs.streams.all<-matrix(data = NA, nrow = nrow(taxa.streams), ncol = length(reference_taxa.streams))
colnames(predicted_probs.streams.all)<-reference_taxa.streams;rownames(predicted_probs.streams.all)<-rownames(taxa.streams)

varimp.streams.all<-matrix(data = NA, ncol = ncol(env.streams), nrow = length(reference_taxa.streams))
rownames(varimp.streams.all)<-reference_taxa.streams;colnames(varimp.streams.all)<-colnames(env.streams)


for(taxon in reference_taxa.streams) 
{
  data.tmp<-data.frame(taxa.streams.pa[calib.streams,taxon], env.streams[calib.streams,])
  model.RF<-randomForest(as.factor(data.tmp[,1]) ~ .,data=data.tmp[,-1], importance=T, ntree=1000, mtry=3, sampsize=42, nodesize=1)
  predicted_probs.streams.all[,taxon]<-predict(model.RF,type="prob", newdata=env.streams)[,'1']
  
  save(model.RF, file=paste("results/models/strem_all",taxon, sep="_"))
  varimp.streams.all[taxon,colnames(env.streams)]<-  model.RF$importance[,'MeanDecreaseAccuracy']
} 

write.table(predicted_probs.streams.all, "results/predicted_probs.streams.all.txt", sep="\t", dec=",", col.names=NA)
write.table(varimp.streams.all, "results/varimp.streams.all.txt", sep="\t", dec=",", col.names=NA)


# Model 2 - streams systemA
preds<-c("Illies.ecoreg", "altitude", "catch_area.m2.", "Alk_Acid","Abs_F")


#Empty matrix for probabilties and variable importance
predicted_probs.streams.systemA_model<-matrix(data = NA, nrow = nrow(taxa.streams), ncol = length(reference_taxa.streams))
colnames(predicted_probs.streams.systemA_model)<-reference_taxa.streams;rownames(predicted_probs.streams.systemA_model)<-rownames(taxa.streams)

varimp.streams.systemA_model<-matrix(data = NA, ncol = length(preds), nrow = length(reference_taxa.streams))
rownames(varimp.streams.systemA_model)<-reference_taxa.streams;colnames(varimp.streams.systemA_model)<-preds


for(taxon in reference_taxa.streams) 
{
  data.tmp<-data.frame(taxa.streams.pa[calib.streams,taxon], env.streams[calib.streams,preds])
  model.RF<-randomForest(as.factor(data.tmp[,1]) ~ .,data=data.tmp[,-1], importance=T, ntree=1000, mtry=3, sampsize=42, nodesize=1)
  predicted_probs.streams.systemA_model[,taxon]<-predict(model.RF,type="prob", newdata=env.streams)[,'1']
  
  save(model.RF, file=paste("results/models/stream_SystemA",taxon, sep="_"))
  varimp.streams.systemA_model[taxon,preds]<-  model.RF$importance[,'MeanDecreaseAccuracy']
} 


write.table(predicted_probs.streams.systemA_model, "results/predicted_probs.streams.systemA.txt", sep="\t", dec=",", col.names=NA)
write.table(varimp.streams.systemA_model, "results/varimp.streams.systemA.txt", sep="\t", dec=",", col.names=NA)

# Model 3 - streams Drakare
preds<-c("LN", "altitude","catch_area.m2.", "slope")


#Empty matrix for probabilties and variable importance
predicted_probs.streams.Drakare_model<-matrix(data = NA, nrow = nrow(taxa.streams), ncol = length(reference_taxa.streams))
colnames(predicted_probs.streams.Drakare_model)<-reference_taxa.streams;rownames(predicted_probs.streams.Drakare_model)<-rownames(taxa.streams)

varimp.streams.Drakare_model<-matrix(data = NA, ncol = length(preds), nrow = length(reference_taxa.streams))
rownames(varimp.streams.Drakare_model)<-reference_taxa.streams;colnames(varimp.streams.Drakare_model)<-preds


for(taxon in reference_taxa.streams) 
{
  data.tmp<-data.frame(taxa.streams.pa[calib.streams,taxon], env.streams[calib.streams,preds])
  model.RF<-randomForest(as.factor(data.tmp[,1]) ~ .,data=data.tmp[,-1], importance=T, ntree=1000, mtry=3, sampsize=42, nodesize=1)
  predicted_probs.streams.Drakare_model[,taxon]<-predict(model.RF,type="prob", newdata=env.streams)[,'1']
  
  save(model.RF, file=paste("results/models/lake_Drakare",taxon, sep="_"))
  varimp.streams.Drakare_model[taxon,preds]<-  model.RF$importance[,'MeanDecreaseAccuracy']
} 

write.table(predicted_probs.streams.Drakare_model, "results/predicted_probs.streams.Drakare_model.txt", sep="\t", dec=",", col.names=NA)
write.table(varimp.streams.Drakare_model, "results/varimp.streams.Drakare_model.txt", sep="\t", dec=",", col.names=NA)




# Types _______________________________________________________________________________________________________________________________

# Lakes  ----------------------------------  ----------------------------------  ----------------------------------  

#SystemA lakes

#Empty matrix for probabilties 
predicted_probs.lakes.systemA_typology<-matrix(data = NA, nrow = nrow(taxa.lakes), ncol = length(reference_taxa.lakes))
colnames(predicted_probs.lakes.systemA_typology)<-reference_taxa.lakes;rownames(predicted_probs.lakes.systemA_typology)<-rownames(taxa.lakes)

for(site in rownames(taxa.lakes.pa)) 
{
  sites_sametypes<-calib.lakes[which(types_SystemA.lakes[calib.lakes,'SystemA']==types_SystemA.lakes[site,])]
  if(is.numeric(length(sites_sametypes)))
  {

    taxa.sametypes<-taxa.lakes.pa[sites_sametypes,reference_taxa.lakes]
    predicted_probs.lakes.systemA_typology[site,]<-colSums(taxa.sametypes)/length(sites_sametypes)
  }}


write.table(predicted_probs.lakes.systemA_typology, "results/predicted_probs.lakes.systemA_typology.txt", sep="\t", dec=",", col.names=NA)

#Drakare lakes

#Empty matrix for probabilties 
predicted_probs.lakes.Drakare_typology<-matrix(data = NA, nrow = nrow(taxa.lakes), ncol = length(reference_taxa.lakes))
colnames(predicted_probs.lakes.Drakare_typology)<-reference_taxa.lakes;rownames(predicted_probs.lakes.Drakare_typology)<-rownames(taxa.lakes)

for(site in rownames(taxa.lakes.pa)) 
{
  sites_sametypes<-calib.lakes[which(types_Drakare.lakes[calib.lakes,'type']==types_Drakare.lakes[site,])]
  if(is.numeric(length(sites_sametypes)))
  {
    
    taxa.sametypes<-taxa.lakes.pa[sites_sametypes,reference_taxa.lakes]
    predicted_probs.lakes.Drakare_typology[site,]<-colSums(taxa.sametypes)/length(sites_sametypes)
  }}

write.table(predicted_probs.lakes.Drakare_typology, "results/predicted_probs.lakes.Drakare_typology.txt", sep="\t", dec=",", col.names=NA)


# Streams  ----------------------------------  ----------------------------------  ----------------------------------  

#SystemA streams

#Empty matrix for probabilties 
predicted_probs.streams.systemA_typology<-matrix(data = NA, nrow = nrow(taxa.streams), ncol = length(reference_taxa.streams))
colnames(predicted_probs.streams.systemA_typology)<-reference_taxa.streams;rownames(predicted_probs.streams.systemA_typology)<-rownames(taxa.streams)

for(site in rownames(taxa.streams.pa)) 
{
  sites_sametypes<-calib.streams[which(types_SystemA.streams[calib.streams,'SystemA']==types_SystemA.streams[site,])]
  if(is.numeric(length(sites_sametypes)))
  {
    
    taxa.sametypes<-taxa.streams.pa[sites_sametypes,reference_taxa.streams]
    predicted_probs.streams.systemA_typology[site,]<-colSums(taxa.sametypes)/length(sites_sametypes)
  }}


write.table(predicted_probs.streams.systemA_typology, "results/predicted_probs.streams.systemA_typology.txt", sep="\t", dec=",", col.names=NA)

#Drakare streams

#Empty matrix for probabilties 
predicted_probs.streams.Drakare_typology<-matrix(data = NA, nrow = nrow(taxa.streams), ncol = length(reference_taxa.streams))
colnames(predicted_probs.streams.Drakare_typology)<-reference_taxa.streams;rownames(predicted_probs.streams.Drakare_typology)<-rownames(taxa.streams)

for(site in rownames(taxa.streams.pa)) 
{
  sites_sametypes<-calib.streams[which(types_Drakare.streams[calib.streams,'type']==types_Drakare.streams[site,])]
  if(is.numeric(length(sites_sametypes)))
  {
    
    taxa.sametypes<-taxa.streams.pa[sites_sametypes,reference_taxa.streams]
    predicted_probs.streams.Drakare_typology[site,]<-colSums(taxa.sametypes)/length(sites_sametypes)
  }}

write.table(predicted_probs.streams.Drakare_typology, "results/predicted_probs.streams.Drakare_typology.txt", sep="\t", dec=",", col.names=NA)


#------------------------------------------------------------------------------------------------------------------------


####################################################################################
# Modellering - null model
####################################################################################
predicted_probs.null<-matrix(data = NA, nrow = nrow(taxa.all.seltaxa), ncol = ncol(taxa.all.seltaxa))
rownames(predicted_probs.null)<-rownames(taxa.all.seltaxa)

predicted_probs.null<-t(replicate(nrow(predicted_probs.null), (colSums(taxa.cal)/nrow(taxa.cal))))
rownames(predicted_probs.null)<-rownames(taxa.all.seltaxa)


#Save probabilties

for(pset in predcitorsets) write.table(get(paste('predicted_probs.rf.', pset, sep='')), paste('results/predicted_probs.rf.', pset, '.txt', sep=''), sep="\t", dec=",", col.names=NA)
write.table(predicted_probs.types, "results/predicted_probs.types.txt", sep="\t", dec=",", col.names=NA)
write.table(predicted_probs.null, "results/predicted_probs.null.txt", sep="\t", dec=",", col.names=NA)

#save varimp
write.table(varimp.rivpacs, "results/varimp.rivpacs.txt", sep="\t", dec=",", col.names=NA)
