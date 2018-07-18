search <- function(term="",ontologies="",service="bioportal",api_key="",extra_args=""){
  
  if(service=="bioportal"){service <- "http://data.bioontology.org/search?"}
  else if (service=="sifr"){service <- "http://data.bioportal.lirmm.fr/search?"}
  else {
    warning("Wrong search service given")
  }
  
  
  if (length(term)<1){
    warning("Give at least one term")
    return(NULL)
  }
  
  if (ontologies=="ICD10"|| ontologies=="ICD10CM"){
    term <- gsub('^([A-Z]{1}[0-9]{2})([0-9]+)$', '\\1.\\2', term)  
  }
  
  if (length(term)>0){
    term=paste("&q=",paste(term,collapse="+"),"",sep="")
    term=gsub(">|<","",term)
    term=gsub(" ","+",term)
  }
  
  if(sum(ontologies=="")==0){
    ontologies=paste("&ontologies=",paste(ontologies,collapse=","),sep="")
  }
  
  url <- paste(service,term,ontologies,"&pagesize=1",sep="")
  results<-GET(url,add_headers(Authorization= paste("apikey token=",api_key,sep="")))
  pagesize <- content(results)$totalCount
  
  
  if ("error" %in% names(content)){
    warning(content$error)
    return(NULL)
  }
  else if (is.null(pagesize) || pagesize==0){
    return(NULL)
  }
  else{
    url <- paste(service,term,ontologies,extra_args=paste("&pagesize=",pagesize,sep=""),sep="")
    results<-GET(url,add_headers(Authorization= paste("apikey token=",api_key,sep="")))
    content <- content(results)
    return(content)
  }
}
