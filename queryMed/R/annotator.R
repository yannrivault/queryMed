if(getRversion()>="2.15.1") utils::globalVariables(c("."))

#' Bioportal and SIFR annotator 
#'
#' Function to use annotator API from Bioportal (https://bioportal.bioontology.org/annotator) and SIFR (http://bioportal.lirmm.fr/annotator) through R. Annotator API recognizes relevant ontology classes in a text given by users.
#'
#' @importFrom httr content GET add_headers
#' @importFrom purrr map
#' @importFrom dplyr %>% 
#'
#' @param text Text the user wants to annotate by ontology classes.
#' @param ontologies A set of ontologies that can serve to annotate text. By default, all available ontologies are used.
#' @param service Character vector that specifies the web service to query either "bioportal" or "sifr".
#' @param api_key An API Key is required to access any API call on Bioportal and SIFR. To have one, you need to register at http://bioportal.bioontology.org/, respectively to http://bioportal.lirmm.fr/ for SIFR.
#' 
#' @return Returns a data frame describing terms from the text, their relative annotations and ontologies.
#' 
#' @references Clement Jonquet, Amina Annane, Khedidja Bouarech, Vincent Emonet & Soumia Melzi. SIFR BioPortal : Un portail ouvert et generique d'ontologies et de terminologies biomedicales francaises au service de l'annotation semantique, In 16th Journees Francophones d'Informatique Medicale, JFIM'16. Geneve, Suisse, July 2016. pp. 16.
#' @references Whetzel PL, Noy NF, Shah NH, Alexander PR, Nyulas C, Tudorache T, Musen MA. BioPortal: enhanced functionality via new Web services from the National Center for Biomedical Ontology to access and use ontologies in software applications. Nucleic Acids Res. 2011 Jul;39(Web Server issue):W541-5. Epub 2011 Jun 14.
#' 
#' @author Y. Rivault
#' @export
#' @examples
#'  \dontrun{
#'    annotator(text="",ontologies="", service="bioportal",api_key="yourAPIKey")
#'  }
#'  

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
  cat("Querring Bioportal annotator REST API\n")
  document<- content(GET(url,add_headers(Authorization= paste("apikey token=",api_key,sep=""))))
  
  if ("errors" %in% names(results)){
    warning(results$errors)
  }
  else if(length(results)>0){
    
    results = data.frame(
      id = document %>% map(.,~ .x[["annotatedClass"]]) %>% map(.,~ .x[["@id"]]) %>% unlist(),
      ontology = document %>% map(.~ .x[["annotatedClass"]]) %>% map(.,~ .x[["links"]]) %>% map(.,~ .x[["ontology"]]) %>% unlist(),
      text = document %>% map(.,~ .x[["annotations"]]) %>% map(.,~ .x[[1]]) %>% map(.,~ .x[["text"]]) %>% unlist()
    )
    
    return(results)
  }
}
