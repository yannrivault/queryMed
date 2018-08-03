mapping_cui <- function(codes=NULL,ontologies_source="",ontologies_target="",api_key=""){
  
  options(warn=2)
  
  cui_source <- codes2cui(codes=codes,ontologies=ontologies_source,api_key=api_key)
  target <- codes2cui(codes=cui_source$cui,ontologies=ontologies_target,api_key=api_key)
  
  res <- merge(cui_source,target,by="cui",all=T)
  colnames(res)=c("cui","source","target")
  
  options(warn=1)
  
  return(unique(res))
}
