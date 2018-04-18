mapping_cui <- function(codes=NULL,ontology="",api_key="",progress=TRUE){
  
  codes <- unique(codes)
  
  n <- length(codes) %/% 800
  rest <- length(codes) %% 800
  
  if(progress==TRUE) progress_bar <- txtProgressBar(min = 0, max = length(codes), style = 3)
  
  results <- data.frame()
  
  if (n>0){
    for(i in 1:n){
      S=search_endpoint(term = paste(codes[((i-1)*800):(i*800-1)],collapse="+"), ontologies = ontology, service = "bioportal", api_key = api_key, extra_args = "&display_context=false&display_links=false")
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
      if(progress==TRUE) setTxtProgressBar(progress_bar, i*800)
    }
  }
  if (rest>0){
    S=search_endpoint(term=paste(codes[(n*800):(n*800+rest)],collapse="+"), ontologies = ontology, service = "bioportal", api_key = api_key, extra_args = "&display_context=false&display_links=false")
    if (!is.null(S)){
      for(j in 1:length(S$collection)){
        if((uri2norm(S$collection[[j]]$'@id') %in% codes) | (length(intersect(unlist(S$collection[[j]]$cui),codes))>0)){
          for(k in 1:length(S$collection[[j]]$cui)){
            results[dim(results)[1]+1,"cui"]=S$collection[[j]]$cui[[k]]
            results[dim(results)[1],"classe"]=S$collection[[j]]$"@id"
          }
        }
        if(progress==TRUE) setTxtProgressBar(progress_bar, n*800+rest)
      }
    }
  }
  if(progress==TRUE) close(progress_bar)
  return(uri2norm(results))
}
