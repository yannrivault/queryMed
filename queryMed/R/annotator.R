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
  results<-content(GET(url,add_headers(Authorization= paste("apikey token=",api_key,sep=""))))
  
  if ("errors" %in% names(results)){
    warning(results$errors)
  }
  else if(length(results)>0){
    
    data_res <- data.frame()
    
    for(i in 1:length(results)){
        data_res[i,"text"]=results[[i]]$annotations[[1]]$text
        data_res[i,"ontology"]=results[[i]]$annotatedClass$links$ontology
        data_res[i,"class"]=results[[i]]$annotatedClass$'@id'
        }
    return(data_res)
  }
}
