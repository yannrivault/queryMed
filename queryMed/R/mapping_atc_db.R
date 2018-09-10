#' Mapping the Anatomical Therapeutic and Chemical classidication to DrugBank
#' 
#' The function allows to get all the mapping from the Anatomical Therapeutic and Chemical classification to DrugBank, through the bio2rdf and dbpedia sparql endpoints.
#' 
#' @details No argument needed. 
#' 
#' @return Return a data.table of size 2993 rows by 2  columns giving the whole Anatomical Therapeutic and Chemical classification to DrugBank mapping.
#' 
#' @references Alison Callahan, Jose Cruz-Toledo, Peter Ansell, Michel Dumontier: Bio2RDF Release 2: Improved Coverage, Interoperability and Provenance of Life Science Linked Data. ESWC 2013: 200-212. 
#' @references Lehman, J et al (2015) Dbpedia: a large-scale, multilingual knowledge extracted from wikipedia. Semantic Web, 6: 167-195 
#' 
#' @export
#' @author Y. Rivault
#' 
#' @seealso [get_DID]
#'  
#' @examples
#'   \dontrun{
#'     xx = mapping_atc_db()
#'     head(xx)
#'   }


mapping_atc_db <- function(){
  
  query="SELECT DISTINCT ?atc ?db
         WHERE  {?db <http://bio2rdf.org/drugbank_vocabulary:x-atc> ?atc .}"
  
  bio2rdf <- uri2norm(sparql(query,url="http://bio2rdf.org/sparql/"))
  
  query="select distinct concat(str(?prefix),str(?suffix)) as ?atc ?db where {
        {?drug dbo:atcPrefix ?prefix .
        ?drug dbo:atcSuffix ?suffix .}
        UNION
        {?drug dbp:atcPrefix ?prefix .
        ?drug dbp:atcSuffix ?suffix .}

        ?drug dbo:drugbank|dbp:drugbank ?db .
        }"
  
  dbpedia <- sparql(query,url="https://dbpedia.org/sparql/")
  
  return(unique(rbind(dbpedia,bio2rdf)))
  
}
