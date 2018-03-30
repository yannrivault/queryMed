get_ancestors <-
function(term="",ontology="",api_key=""){
  
  if(length(c("",ontology))!=2 || !is.character(ontology)){
    warning("Give an unique character for ontology parameter")
    return(NULL)
  }
  
  if(term==""){
    warning("Give a term")
    return(NULL)
  }
  
  if (ontology=="ATC") {
    url=paste("http://data.bioontology.org/ontologies/ATC/classes/http%3A%2F%2Fpurl.bioontology.org%2Fontology%2FUATC%2F",term,"/ancestors",sep="")
  }
  else {
    url=paste("http://data.bioontology.org/ontologies/",ontology,"/classes/http%3A%2F%2Fpurl.bioontology.org%2Fontology%2F",ontology,"%2F",term,"/ancestors",sep="")
  }
  results<-GET(url,add_headers(Authorization= paste("apikey token=",api_key,sep="")))
  content <- content(results)
  
  
  if (any(grepl("error",names(content)))){
    warning(content$error)
    return(NULL)
  }
  else if (length(content)==0){
    warning("Term unknown or no ancestors")
    return(NULL)
  }
  
  ancestors_df=setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("id", "prefLabel"))
  for(j in 1:length(content)){
    ancestors_df[dim(ancestors_df)[1]+1,]=c(content[[j]]$"@id",content[[j]]$prefLabel)
  }
  
  return(ancestors_df)
}
