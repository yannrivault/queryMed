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
