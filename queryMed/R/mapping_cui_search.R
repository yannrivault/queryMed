mapping_cui_search <- function(codes=NULL,ontology="",source="cui",api_key=""){
  
  if (source=="cui"){
    codes=unlist(str_extract_all(unique(codes),"C[0-9]{7}"))
  }
  
  n <- length(codes) %/% 800
  rest <- length(codes) %% 800
  
  results <- data.frame()
  
  if (n>0){
    for(i in 1:n){
      S=search_endpoint(term = paste(codes[((i-1)*800):(i*800-1)],collapse="+"), ontologies = ontology, service = "bioportal", api_key = api_key, extra_args = "&display_context=false&display_links=false")
      if (!is.null(S)){
        for(j in 1:length(S$collection)){
          for(k in 1:length(S$collection[[j]]$cui)){
            if(S$collection[[j]]$cui[[k]] %in% codes){
              results[dim(results)[1]+1,"cui"]=S$collection[[j]]$cui[[k]]
              results[dim(results)[1],"mapping"]=S$collection[[j]]$"@id"
            }
          }
        }
      }
    }
  }
  if (rest>0){
    S=search_endpoint(term=paste(codes[(n*800):(n*800+rest)],collapse="+"), ontologies = ontology, service = "bioportal", api_key = api_key, extra_args = "&display_context=false&display_links=false")
    if (!is.null(S)){
      for(j in 1:length(S$collection)){
        for(k in 1:length(S$collection[[j]]$cui)){
          if(S$collection[[j]]$cui[[k]] %in% codes){
            results[dim(results)[1]+1,"cui"]=S$collection[[j]]$cui[[k]]
            results[dim(results)[1],"mapping"]=S$collection[[j]]$"@id"
          }
        }
      }
    }
  }
  
  return(results)
}
