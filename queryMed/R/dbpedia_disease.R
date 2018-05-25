dbpedia_disease <- function(lang="en"){
  
  query=paste(
    c("SELECT DISTINCT * 
  WHERE {
  ?disease dbo:icd10|dbp:icd10 ?icd10 .
  OPTIONAL{?disease dbo:icd9|dbp:icd9 ?icd9 . }

  OPTIONAL{?disease dbo:abstract|dbp:abstract ?abstract .
  FILTER (lang(?abstract) IN ('","'))}

  OPTIONAL{?disease rdfs:comment ?comment .
  FILTER (lang(?comment) IN ('","'))}

  OPTIONAL{?disease rdfs:label ?label .
  FILTER (lang(?label) IN ('","'))}

  OPTIONAL{?disease dbp:synonyms ?synonyms .}
  
}"),collapse=lang)
  
  return(sparql(query,url="https://dbpedia.org/sparql"))
}