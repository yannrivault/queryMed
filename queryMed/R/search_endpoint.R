search_endpoint <-
function(term="",cui="",ontologies="",service="bioportal",api_key="",extra_args=""){
  
  if(service=="bioportal"){service <- "http://data.bioontology.org/search?"}
  else if (service=="sifr"){service <- "http://data.bioportal.lirmm.fr/search?"}
  else {
    warning("Wrong search service given")
  }
  
  if(sum(ontologies=="")==0){
    ontologies=paste("&ontologies=",paste(ontologies,collapse=","),sep="")
  }
  
  if (cui=="" && term==""){
    warning("Give at least one term or CUI")
    return(NULL)
  }
  
  if (cui!="" || term!=""){
    term=paste("&q=",paste(term,collapse="+"),"",sep="")
    cui=paste("&cui={",paste(cui,collapse=","),"}",sep="")
  }
  
  url <- paste(service,term,cui,ontologies,extra_args,sep="")
  results<-GET(url,add_headers(Authorization= paste("apikey token=",api_key,sep="")))
  content <- content(results)
  
  if ("error" %in% names(content)){
    warning(content$error)
    return(NULL)
  }
  else if (content$totalCount==0){
    warning("Term, CUI or CUI and term combination unknown")
    return(NULL)
  }
  else return(content)
}
