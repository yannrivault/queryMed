get_ontology_from_bioportal <- function(ontology, apikey=""){
  
  path=file.path(file.path(find.package("queryMed"), c("data"),"ontologies"))
  dir.create(path, showWarnings = FALSE)
  
  download.file(paste(c("http://data.bioontology.org/ontologies/",toupper(ontology),"/download?apikey=",apikey),collapse=""),paste(path,"ontology",""))
  ontology = load.rdf(paste(path,"ontology",""), format="TURTLE")
  return(ontology)
  
}