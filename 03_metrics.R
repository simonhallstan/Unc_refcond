#Metrics for evaluation

require(PresenceAbsence)


#functions for O/E och BC
OE<-function(obs, pred, Pc){sum(obs[pred>=Pc]) / sum(pred[pred>=Pc])}
OE00<-function(obs, pred){sum(obs[pred>0]) / sum(pred[pred>0])}
OE25<-function(obs, pred){sum(obs[pred>=0.25]) / sum(pred[pred>=0.25])}
OE50<-function(obs, pred){sum(obs[pred>=0.5]) / sum(pred[pred>=0.5])}
BC<-function(obs, pred){sum(abs(obs-pred))/ (sum(obs+pred))}




# Lakes - AUC per taxon ---------------------------------------------------

models<-c("lakes.all", "lakes.systemA_model", "lakes.Drakare_model", "lakes.systemA_typology","lakes.Drakare_typology", "lakes.null")

AUC_taxa.lakes<-matrix(data = NA, ncol = length(models), nrow = length(reference_taxa.lakes))
rownames(AUC_taxa.lakes)<-reference_taxa.lakes
colnames(AUC_taxa.lakes)<-models

for(model in models)
{
  predicted_probs<-get(paste('predicted_probs.', model, sep=''))
  for(taxon in reference_taxa.lakes)
  {
    
    modelresults.tmp<-data.frame(valid.lakes,taxa.lakes.pa[valid.lakes,taxon],predicted_probs[valid.lakes,taxon])
    AUC_taxa.lakes[taxon,model]<-auc(modelresults.tmp, na.rm=T)[1,'AUC']
  }}
write.table(AUC_taxa.lakes, "results/AUC_taxa.lakes.txt", sep="\t", dec=",", col.names=NA)


# Streams - AUC per taxon -------------------------------------------------

models<-c("streams.all", "streams.systemA_model", "streams.Drakare_model", "streams.systemA_typology","streams.Drakare_typology", "streams.null")

AUC_taxa.streams<-matrix(data = NA, ncol = length(models), nrow = length(reference_taxa.streams))
rownames(AUC_taxa.streams)<-reference_taxa.streams
colnames(AUC_taxa.streams)<-models

for(model in models)
{
  predicted_probs<-get(paste('predicted_probs.', model, sep=''))
  for(taxon in reference_taxa.streams)
  {
    
    modelresults.tmp<-data.frame(valid.streams,taxa.streams.pa[valid.streams,taxon],predicted_probs[valid.streams,taxon])
    AUC_taxa.streams[taxon,model]<-auc(modelresults.tmp, na.rm=T)[1,'AUC']
  }}

write.table(AUC_taxa.streams, "results/AUC_taxa.streams.txt", sep="\t", dec=",", col.names=NA)


# Lakes - metrics per site ------------------------------------------------

models<-c("lakes.all", "lakes.systemA_model", "lakes.Drakare_model", "lakes.systemA_typology","lakes.Drakare_typology", "lakes.null")

metrics_lakes<-array(data = NA, dim=c(nrow(taxa.lakes.pa), 6,5), dimnames=list(rownames(taxa.lakes.pa),models, c('OE00', 'OE25', 'OE50', 'BC', 'AUC')))

for(model in models)
{
  predicted_probs<-get(paste('predicted_probs.', model, sep=''))
  for(site in rownames(taxa.lakes.pa))
  {
    
    modelresults.tmp<-data.frame((as.matrix(reference_taxa.lakes)),t(taxa.lakes.pa[site,reference_taxa.lakes]),t(predicted_probs[site,,drop=F]))
    
    
    if(all(!is.na(modelresults.tmp[,3])))
    {
      if(max(predicted_probs[site,])>0)     metrics_lakes[site,model,'OE00']<-OE(taxa.lakes.pa[site,reference_taxa.lakes], predicted_probs[site,], 0)
      if(max(predicted_probs[site,])>0.25)  metrics_lakes[site,model,'OE25']<-OE(taxa.lakes.pa[site,reference_taxa.lakes], predicted_probs[site,], 0.25)
      if(max(predicted_probs[site,])>0.50)  metrics_lakes[site,model,'OE50']<-OE(taxa.lakes.pa[site,reference_taxa.lakes], predicted_probs[site,], 0.5)
      metrics_lakes[site,model,'BC']<-BC(taxa.lakes.pa[site,reference_taxa.lakes], predicted_probs[site,])
      metrics_lakes[site,model,'AUC']<-auc(modelresults.tmp, na.rm=T)[1,'AUC']
    }
  }}

for(metric in c('OE00', 'OE25', 'OE50', 'BC', 'AUC'))   write.table(metrics_lakes[,,metric], paste("results/metrics_lakes_", metric, ".txt", sep=''), sep="\t", dec=",", col.names=NA)


# Streams - metric per site -----------------------------------------------

models<-c("streams.all", "streams.systemA_model", "streams.Drakare_model", "streams.systemA_typology","streams.Drakare_typology", "streams.null")

metrics_streams<-array(data = NA, dim=c(nrow(taxa.streams.pa), 6,5), dimnames=list(rownames(taxa.streams.pa),models, c('OE00', 'OE25', 'OE50', 'BC', 'AUC')))

for(model in models)
{
  predicted_probs<-get(paste('predicted_probs.', model, sep=''))
  for(site in rownames(taxa.streams.pa))
  {
    
    modelresults.tmp<-data.frame((as.matrix(reference_taxa.streams)),t(taxa.streams.pa[site,reference_taxa.streams]),t(predicted_probs[site,,drop=F]))
    
    
    if(all(!is.na(modelresults.tmp[,3])))
    {
      if(max(predicted_probs[site,])>0)     metrics_streams[site,model,'OE00']<-OE(taxa.streams.pa[site,reference_taxa.streams], predicted_probs[site,], 0)
      if(max(predicted_probs[site,])>0.25)  metrics_streams[site,model,'OE25']<-OE(taxa.streams.pa[site,reference_taxa.streams], predicted_probs[site,], 0.25)
      if(max(predicted_probs[site,])>0.50)  metrics_streams[site,model,'OE50']<-OE(taxa.streams.pa[site,reference_taxa.streams], predicted_probs[site,], 0.5)
      metrics_streams[site,model,'BC']<-BC(taxa.streams.pa[site,reference_taxa.streams], predicted_probs[site,])
      metrics_streams[site,model,'AUC']<-auc(modelresults.tmp, na.rm=T)[1,'AUC']
    }
  }}

for(metric in c('OE00', 'OE25', 'OE50', 'BC', 'AUC'))   write.table(metrics_streams[,,metric], paste("results/metrics_streams_", metric, ".txt", sep=''), sep="\t", dec=",", col.names=NA)


# Save workspace ----------------------------------------------------------
save.image("results/workspace.RData")



