sparql <- function(query="",url="",api_key=""){
  
  encoded_query=URLencode(gsub("[[:space:]]+", " ", query),reserved = TRUE)
  
  encoded_url=paste(url,"?query=",encoded_query,sep="")
  
  
  
  if(api_key!=""){
    encoded_url=paste(encoded_url,"&apikey=",api_key,sep="")
  }
  
  cat(paste("Querying ",url,sep=""),"\n",append=T)
  document <- fromJSON(encoded_url)
  
  if(length(document$results$bindings)!=0){
    results=document$results$bindings %>% map_df("value")
  }
  else{results=NULL}
  
  return(results)
}
