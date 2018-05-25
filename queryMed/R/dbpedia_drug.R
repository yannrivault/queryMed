dbpedia_drug <- function(lang="en"){
  
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

return(sparql(query,url="https://dbpedia.org/sparql"))
}