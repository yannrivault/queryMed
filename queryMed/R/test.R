test <- function(path=""){
  download.file("http://data.bioontology.org/ontologies/ATC/submissions/9/download",path)
  exData = load.rdf(path, format="TURTLE")
  print(exData)
  
}
