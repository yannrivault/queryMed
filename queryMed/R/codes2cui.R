codes2cui <- function(codes=NULL,ontologies="",api_key=""){
  
  codes <- unique(codes)
  
  n <- length(codes) %/% 800
  rest <- length(codes) %% 800
  results <- data.frame()
  
  cat("Querying Bioportal search REST API to retrieving mapping\n")
  
  if (n>0){
    for(i in 1:n){
      S=search(term = codes[((i-1)*800):(i*800-1)], ontologies = ontologies, service = "bioportal", api_key = api_key, extra_args = "&display_context=false&display_links=false")
      if (!is.null(S)){
        for(j in 1:length(S$collection)){
          if((uri2norm(S$collection[[j]]$'@id') %in% codes) | (length(intersect(unlist(S$collection[[j]]$cui),codes))>0)){
            for(k in 1:length(S$collection[[j]]$cui)){
              results[dim(results)[1]+1,"cui"]=S$collection[[j]]$cui[[k]]
              results[dim(results)[1],"classe"]=S$collection[[j]]$"@id"
            }
          }
        }
      }
    }
    
  }
  if (rest>0){
    S=search(term=paste(codes[(n*800):(n*800+rest)],collapse="+"), ontologies = ontologies, service = "bioportal", api_key = api_key, extra_args = "&display_context=false&display_links=false")
    if (!is.null(S)){
      for(j in 1:length(S$collection)){
        if((uri2norm(S$collection[[j]]$'@id') %in% codes) | (length(intersect(unlist(S$collection[[j]]$cui),codes))>0)){
          for(k in 1:length(S$collection[[j]]$cui)){
            results[dim(results)[1]+1,"cui"]=S$collection[[j]]$cui[[k]]
            results[dim(results)[1],"classe"]=S$collection[[j]]$"@id"
          }
        }
      }
    }
  }
  return(uri2norm(results))
}
