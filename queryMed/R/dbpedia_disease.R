#' Disease information from dbpedia
#' 
#' Retrieve disease information from dbpedia such as ICD10-ICD9 matching, label, synonyms etc.
#' 
#' @param lang Character vector to specify the language of the results returned 
#' @param icd10 A vector of icd10 codes for which to retrieve informations
#' @param icd9 A vector of icd9 codes for which to retrieve informations
#'   
#' @importFrom dplyr mutate
#' @importFrom tidyr unnest
#'     
#' @return data table of disease terms and their annotation: disease name, icd10, icd9,   abstract, comment, label, synonyms. When querying the whole dataset, the data table is of size 5523 rows by 6 columns.
#' 
#' @references Lehman, J et al (2015) Dbpedia: a large-scale, multilingual knowledge extracted from wikipedia. Semantic Web, 6: 167-195 
#' 
#' @export
#' 
#' @author Y. Rivault
#' 
#' @seealso [dbpedia_drug]
#' 
#' @examples
#'  \dontrun{
#'     disease_dbp <- dbpedia_disease(lang="en")
#'     head(disease_dbp)
#'   }

dbpedia_disease <- function(lang="en",icd10=NULL,icd9=NULL){
  
  lang=paste(lang,collapse="\",\"")
  
  query=paste(
    c("SELECT DISTINCT * 
  WHERE {

  ?disease dbo:icd10|dbp:icd10 ?icd10 .
  OPTIONAL{?disease dbo:icd9|dbp:icd9 ?icd9 . }

  OPTIONAL{?disease dbo:abstract|dbp:abstract ?abstract .
  FILTER (lang(?abstract) IN (\"","\"))}

  OPTIONAL{?disease rdfs:comment ?comment .
  FILTER (lang(?comment) IN (\"","\"))}

  OPTIONAL{?disease rdfs:label ?label .
  FILTER (lang(?label) IN (\"","\"))}

  OPTIONAL{?disease dbp:synonyms ?synonyms .}
  
}"),collapse=lang)
  
  res=sparql(query,url="https://dbpedia.org/sparql")
  
  res <- res %>% mutate(icd10 = strsplit(as.character(icd10), ",")) %>% unnest(icd10)
  
  if(!is.null(icd10)){
    icd10 <- gsub('^([A-Z]{1}[0-9]{2})([0-9]+)$', '\\1.\\2', icd10)  
    res <- res[res$icd10 %in% icd10,]
  }
  if(!is.null(icd9)){
    res <- res[res$icd9 %in% icd9,]
  }
  
  return(res)
}