#' Retrieve drug informations from bio2rdf
#' 
#' Retrieve drug informations from bio2rdf SPARQL endpoint such as label, description, category, etc.
#'
#' @param lang Character vector to specify the language of the results returned 
#' @param atc A vector of atc codes for which to retrieve informations
#' @param db A vector of db codes for which to retrieve informations
#' 
#' @export
#' 
#' @author Y. Rivault
#' @examples
#'  \dontrun{
#'    bio2rdf <- bio2rdf_db(lang="en")
#'    head(bio2rdf)
#' }
#' 

bio2rdf_db <- function(lang="en",atc=NULL,db=NULL){
  
  lang=paste(lang,collapse="\",\"")
  
  query=paste(c(
    "SELECT DISTINCT *
  WHERE  {
    ?db <http://bio2rdf.org/drugbank_vocabulary:x-atc> ?atc .
    OPTIONAL {?db dcterms:title ?title .}
    OPTIONAL {?db rdfs:label ?label .
    FILTER(lang(?label) IN (\"","\"))}
    OPTIONAL {?db dcterms:description ?description .
    FILTER(lang(?description) IN (\"","\"))}
    OPTIONAL {?db <http://bio2rdf.org/drugbank_vocabulary:category> ?category .}
    
  }"),collapse=lang)
  
  res=sparql(query,url="http://bio2rdf.org/sparql")
  res=uri2norm(res)
  
  if(!is.null(db)){
    res <- res[res$db %in% db,]
  }
  if(!is.null(atc)){
    res <- res[res$atc %in% atc,]
  }
  
  return(res)
  
}