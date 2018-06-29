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
    res <- res[res$icd10 %in% icd10,]
  }
  if(!is.null(icd9)){
    res <- res[res$icd9 %in% icd9,]
  }
  
  return(res)
}