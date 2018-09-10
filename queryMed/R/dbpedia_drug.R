#' Information retrieval from DBpedia drug
#' 
#' Retrieve drug information from dbpedia such as atc and drug bank identifier codes
#'   
#' @param lang Language for character results
#' @param atc A vector of ATC codes for which to retrieve informations
#' @param db A vector of DrugBank codes for which to retrieve informations
#' 
#' @return data table of drug terms and their annotation : drug, atc code,  drug bank identifier,  abstract,   comment, label. When querying the whole dataset, the data table is of size 8712 rows by 6 columns.
#' 
#' @references Lehman, J et al (2015) Dbpedia: a large-scale, multilingual knowledge extracted from wikipedia. Semantic Web, 6: 167-195 
#' @export
#' 
#' @author Y. Rivault
#' 
#' @seealso [dbpedia_disease]
#' 
#' @examples
#'   \dontrun{
#'     drug_dbp <- dbpedia_drug(lang="en")
#'     head(drug_dbp)
#'   }


dbpedia_drug <- function(lang="en",atc=NULL,db=NULL){
  
  lang=paste(lang,collapse="\",\"")
  
  query=paste(
  c(
  "select distinct ?drug ?atc ?db ?abstract ?smiles ?comment ?label ?synonyms
  where {
  
  ?drug rdf:type dbo:Drug . 

  OPTIONAL{
  ?drug dbo:atcPrefix ?prefix .
  ?drug dbo:atcSuffix ?suffix .
  BIND(concat(str(?prefix),str(?suffix)) as ?atc)}
  
  OPTIONAL{?drug dbo:drugbank|dbp:drugbank ?db .}
  
  OPTIONAL{?drug dbo:synonyms ?synonyms .}
  
  OPTIONAL{?drug dbo:abstract ?abstract .
  FILTER (lang(?abstract) IN (\"","\"))}

  OPTIONAL{?drug dbo:smiles ?smiles .}
  
  OPTIONAL{?drug rdfs:comment ?comment .
  FILTER (lang(?comment) IN (\"","\"))}
  
  OPTIONAL{?drug rdfs:label ?label .
  FILTER (lang(?label) IN (\"","\"))}
  
  }"),collapse=lang)
  
  res=sparql(query,url="https://dbpedia.org/sparql")
  
  if(!is.null(atc)){
    res <- res[res$atc %in% atc,]
  }
  if(!is.null(db)){
    res <- res[res$db %in% db,]
  }

return(res)
}
