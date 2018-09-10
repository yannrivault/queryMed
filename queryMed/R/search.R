#' Access to BioPortal or SIFR search REST API
#' 
#' This function allows retrieving knowledge from medical ontologies through BioPortal or SIFR search REST API.
#' 
#' @param term A list of terms to search for informations on Bioportal. It can be codes from nomenclatures.
#' @param ontologies A list of ontologies to constrain the search of informations.
#' @param service Character vector that specifies the web service to query either "bioportal" or "sifr".
#' @param api_key An API Key is required to access any API call on BioPortal and SIFR. To have one, you need to register at <http://bioportal.bioontology.org/>, respectively to <http://bioportal.lirmm.fr/> for SIFR.
#' @param extra_args Extra arguments to parse into the query.
#' 
#' @details This is a low level function. The return are return as JSON list of lists.
#'
#' @return The return value is a list of lists of elements that define the request term
#'
#' @references Noy NF, Shah NH, Whetzel PL, Dai B, Dorf M, Griffith N, Jonquet C, Rubin DL, Storey MA, Chute CG, Musen MA. BioPortal: ontologies and integrated data resources at the click of a mouse. Nucleic Acids Res. 2009 Jul;37 (Web Server issue):W170-3.
#' @references Whetzel PL, Noy NF, Shah NH, Alexander PR, Nyulas C, Tudorache T, Musen MA. BioPortal: enhanced functionality via new Web services from the National Center for Biomedical Ontology to access and use ontologies in software applications. Nucleic Acids Res. 2011 Jul;39(Web Server issue):W541-5. Epub 2011 Jun 14.
#' @references Salvadores M, Horridge M, Alexander PR, Fergerson RW, Musen MA, and Noy NF. Using SPARQL to Query BioPortal Ontologies and Metadata. International Semantic Web Conference. Boston US. LNCS 7650, pp. 180195, 2012.
#'
#' @export
#'
#' @author Y. Rivault
#' @examples
#' \donttest{
#'   \dontrun{
#'     disease72 = search(term = "I72", ontologies = "ICD10", 
#'     service = "bioportal", api_key = "yourAPIkey")
#'     disease72$collection[[1]]$prefLabel
#'   }   
#'   }
#'   
 
search <- function(term="",ontologies="",service="bioportal",api_key="",extra_args=""){
  
  options(warn=2)
  
  if(service=="bioportal"){service <- "http://data.bioontology.org/search?"}
  else if (service=="sifr"){service <- "http://data.bioportal.lirmm.fr/search?"}
  else {
    warning("Wrong search service given")
  }
  
  
  if (length(term)<1){
    warning("Give at least one term")
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
  
  
  if ("error" %in% names(content(results))){
    warning(content(results)$error)
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
  
  options(warn=1)
  
}
