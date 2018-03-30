mapping_atc_cui <-
function(codes=NULL,source="cui",api_key=""){
  
  if (source=="cui"){
    codes=unlist(str_extract_all(unique(codes),"C[0-9]{7}"))
  }
  
  n <- length(codes) %/% 800 
  rest <- length(codes) %% 800
  
  results <- data.frame()
  
  if (n>0){
    for(i in 1:n){
      S=search_endpoint(term = paste(codes[((i-1)*800):(i*800-1)],collapse="+"), ontologies = "ATC", service = "bioportal", api_key = api_key, extra_args = "&display_context=false&display_links=false")
      for(j in 1:length(S$collection)){
        results[dim(results)[1]+1,"atc"]=S$collection[[j]]$"@id"
        results[dim(results)[1],"cui"]=S$collection[[j]]$cui
      }
    }
  }
  if (rest>0){
    S=search_endpoint(term = paste(codes[(n*800):(n*800+rest)],collapse="+"), ontologies = "ATC", service = "bioportal", api_key = api_key, extra_args = "&display_context=false&display_links=false")
    for(j in 1:length(S$collection)){
      results[dim(results)[1]+1,"atc"]=S$collection[[j]]$"@id"
      results[dim(results)[1],"cui"]=S$collection[[j]]$cui
    }
  }
    
  return(results)
}
