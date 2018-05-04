search_endpoint <- function(term="",cui="",ontologies="",service="bioportal",api_key="",extra_args=""){
  
  if(service=="bioportal"){service <- "http://data.bioontology.org/search?"}
  else if (service=="sifr"){service <- "http://data.bioportal.lirmm.fr/search?"}
  else {
    warning("Wrong search service given")
  }
  
  if(sum(ontologies=="")==0){
    ontologies=paste("&ontologies=",paste(ontologies,collapse=","),sep="")
  }
  
  if (length(cui)<1 && length(term)<1){
    warning("Give at least one term or CUI")
    return(NULL)
  }
  
  if (length(cui)>0 || length(term)>0){
    term=paste("&q=",paste(term,collapse="+"),"",sep="")
    cui=paste("&cui={",paste(cui,collapse=","),"}",sep="")
    term=gsub(">|<","",term)
    term=gsub(" ","+",term)
    cui=gsub(">|<","",cui)
  }
  
  
  
  url <- paste(service,term,cui,ontologies,"&pagesize=1",sep="")
  results<-GET(url,add_headers(Authorization= paste("apikey token=",api_key,sep="")))
  pagesize <- content(results)$totalCount
  
  
  if ("error" %in% names(content)){
    warning(content$error)
    return(NULL)
  }
  else if (pagesize==0){
    return(NULL)
  }
  else{
    url <- paste(service,term,cui,ontologies,extra_args=paste("&pagesize=",pagesize,sep=""),sep="")
    results<-GET(url,add_headers(Authorization= paste("apikey token=",api_key,sep="")))
    content <- content(results)
    return(content)
  }
}
