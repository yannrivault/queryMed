annotator <- function(text="",ontologies="",service="bioportal",api_key=""){
  
  if(service=="bioportal"){service <- "http://data.bioontology.org/annotator?text="}
  else if (service=="sifr"){service <- "http://data.bioportal.lirmm.fr/annotator?text="}
  else {
    warning("Wrong search service given")
  }
  
  if(sum(ontologies=="")==0){
    ontologies=paste("&ontologies=",paste(ontologies,collapse=","),sep="")
  }
  
  text=gsub(" ","+",text)
  
  url <- paste(service,text,ontologies,sep="")
  test=paste(url,"&apikey=",api_key,sep="")
  coucou <- fromJSON(test,flatten=TRUE)
  cat("Querring Bioportal annotator REST API\n")
  document<-content(GET(url,add_headers(Authorization= paste("apikey token=",api_key,sep=""))))
  
  if ("errors" %in% names(results)){
    warning(results$errors)
  }
  else if(length(results)>0){
    
    results = data.frame(
      id = document %>% map(.,~ .x[["annotatedClass"]]) %>% map(.,~ .x[["@id"]]) %>% unlist(),
      ontology = document %>% map(.,~ .x[["annotatedClass"]]) %>% map(.,~ .x[["links"]]) %>% map(.,~ .x[["ontology"]]) %>% unlist(),
      text = document %>% map(.,~ .x[["annotations"]]) %>% map(.,~ .x[[1]]) %>% map(.,~ .x[["text"]]) %>% unlist()
    )
    
    return(results)
  }
}
