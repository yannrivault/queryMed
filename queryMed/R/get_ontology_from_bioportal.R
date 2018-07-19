get_ontology_from_bioportal <- function(ontology, path="", apikey=""){
  download.file(paste(c("http://data.bioontology.org/ontologies/",toupper(ontology),"/download?apikey=",apikey),collapse=""),paste(path,"ATC",""))
  ontology = load.rdf(paste(path,"ATC",""), format="TURTLE")
  return(ontology)
  
}
