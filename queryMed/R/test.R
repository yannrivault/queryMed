test <- function(path="",apikey=""){
  download.file(paste("http://data.bioontology.org/ontologies/ATC/submissions/9/download?apikey=",apikey,sep=""),path)
  exData = load.rdf(path, format="TURTLE")
  print(exData)
  
}
