sparql <-
function(query="",url="",api_key=""){
  
  encoded_query=URLencode(gsub("[[:space:]]+", " ", query),reserved = TRUE)
  
  url=paste(url,"?query=",encoded_query,sep="")
  
  if(api_key!=""){
    url=paste(url,"&apikey=",api_key,sep="")
  }
  
  json_doc=getURL(url,httpheader=c(Accept = "application/sparql-results+json"))
  document <- fromJSON(json_doc)
  
  if(length(document$results$bindings)!=0){
  results=data.frame(matrix(nrow=dim(document$results$bindings)[1],ncol=dim(document$results$bindings)[2]))
  colnames(results)=names(document$results$bindings)
  for(i in colnames(results)){
    results[,i]=document[["results"]][["bindings"]][[i]][["value"]]
  }
  }
  else{results=NULL}
  
  return(results)
}
