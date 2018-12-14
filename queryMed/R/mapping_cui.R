#' @include codes2cui.R
NULL

#' Mapping between medical nomenclatures using CUI
#' 
#' The function allows mapping codes from a medical terminology to another, using the Concept Unique Identifier (CUI) from the Unified Medical Language System (UMLS). \emph{mapping_cui()} uses \emph{codes2cui()} function, which programmatically access to BioPortal API to search for a potential mapping. Thus it needs a BioPortal API key. To have one, you need to register at \href{http://bioportal.bioontology.org/}{BioPortal}.
#' 
#' @param codes Codes from source ontologies (or nomenclatures) the user want to mapp to target ontologies (or nomenclatures).
#' @param ontologies_source A set of ontologies where the codes come from. Can be void.
#' @param ontologies_target A set of ontologies to search for mapping. If void, the mapping will be searched in all available ontologies in \href{http://bioportal.bioontology.org/}{BioPortal}.
#' @param api_key  An API Key is required to access any API call on BioPortal. To have one, you need to register at \href{http://bioportal.bioontology.org/}{BioPortal}.
#'
#' @return Data.frame of mapped codes
#' 
#' @export
#' 
#' @author Y. Rivault
#' @examples
#'  \dontrun{
#'    data(drug_set)
#'    drug_ATC_NDFRT <- mapping_cui(drug_set$ATC, ontologies_source="ATC", 
#'    ontologies_target="NDFRT", api_key="your_api_key")
#'    head(drug_ATC_NDFRT)
#'  } 
#'  
mapping_cui <- function(codes=NULL, ontologies_source="", ontologies_target="",api_key=""){
  
  options(warn=2)
  
  cui_source <- codes2cui(codes, ontologies=ontologies_source, api_key=api_key)
  if(length(cui_source)>0){
    target <- codes2cui(cui_source$cui, ontologies=ontologies_target, api_key=api_key)
    if(length(target)>0){
      res <- merge(cui_source, target, by="cui",all=T)
      colnames(res)=c("cui","source","target")
      return(unique(res))
    }
    else{return(NULL)}
  }
  else{return(NULL)}
  
  options(warn=1)
}
