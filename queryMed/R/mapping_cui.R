mapping_cui <- function(codes=NULL,ontologies="",api_key="",progress=T){
  
  codes <- unique(codes)
  
  n <- length(codes) %/% 800
  rest <- length(codes) %% 800
  results <- data.frame()
  
  cat("Querring Bioportal search REST API to retrieving mapping\n")
  if (progress) progress_bar <- tkProgressBar(title = "progress bar", label="Querring Bioportal search REST API: O%", width = 300,min=0,max=length(codes))
    
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
      if (progress) setTkProgressBar(progress_bar, i*800, label=paste("Querring Bioportal search REST API: ",round(800*i/length(codes)*100, 0),"%",sep=""))
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
  if (progress) {
    setTkProgressBar(progress_bar, length(codes),label="Querring Bioportal search REST API: 10O%")
    close(progress_bar)
  }
  return(uri2norm(results))
}
