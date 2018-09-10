#' Hierachical search for ancestors from ontologies
#' 
#' Retrieving a list of ancestors of a class from ontologies available on Bioportal or SIFR web service.
#' 
#' @param term Term as character vector
#' @param ontology Name of the ontology as character vector
#' @param api_key An API Key is required to access any API call on Bioportal and SIFR. To have one, you need to register at http://bioportal.bioontology.org/, respectively to http://bioportal.lirmm.fr/ for SIFR.
#' 
#' @importFrom stats setNames
#' 
#' @details The available ontologies are for instance: ATC, ICD10, NDFRT,SNOMEDCT. See http://bioportal.bioontology.org/ for more details.
#' 
#' @return Returns a list of ancestors with for instance id as URI with ancestors' code and prefLabel as Labels of the retrieve term
#' 
#' @references Noy NF, Shah NH, Whetzel PL, Dai B, Dorf M, Griffith N, Jonquet C, Rubin DL, Storey MA, Chute CG, Musen MA. BioPortal: ontologies and integrated data resources at the click of a mouse. Nucleic Acids Res. 2009 Jul;37 (Web Server issue):W170-3.
#' @references Whetzel PL, Noy NF, Shah NH, Alexander PR, Nyulas C, Tudorache T, Musen MA. BioPortal: enhanced functionality via new Web services from the National Center for Biomedical Ontology to access and use ontologies in software applications. Nucleic Acids Res. 2011 Jul;39(Web Server issue):W541-5. Epub 2011 Jun 14.
#' 
#' @author Y. Rivault
#' 
#' @export
#' 
#' @examples
#'   \dontrun{
#'     get_ancestors("A05AA",ontology="ATC",api_key = "yourAPIkey")
#'     get_ancestors("C50",ontology="ICD10",api_key = "yourAPIkey")
#'     get_ancestors("N0000153235",ontology="NDFRT",api_key = "yourAPIkey")
#'   }

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
