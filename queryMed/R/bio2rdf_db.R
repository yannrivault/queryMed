# bio2rdf

bio2rdf_db <- function(lang="en"){
  
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
  
  return(sparql(query,url="http://bio2rdf.org/sparql"))
}