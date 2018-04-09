mapping_atc_db <- function(){
  
  query="SELECT DISTINCT ?atc ?db
         WHERE  {?db <http://bio2rdf.org/drugbank_vocabulary:x-atc> ?atc .}"
  
  return(sparql(query,url="bio2rdf.org/sparql/"))
}